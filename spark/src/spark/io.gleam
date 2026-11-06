import arc/host
import arc/rt/types.{
  type JsVal, JFloat, mk_int, mk_number, mk_string, mk_undefined,
}
import gleam/int
import gleam/list

/// everything a script can ask the outside world for
///
/// spark performs none of these; each one reaches the embedder through `Io`
pub type Want {
  /// whether `path` can be reached with this access mode (0 exists, 4 read,
  /// 2 write, 1 execute, or-ed together). node:fs access and existsSync use it
  Access(path: String, mode: Int)
  /// the canonical absolute path for `path`, with every symlink followed.
  /// The embedder answers with Text, and resolves a relative path itself
  Realpath(path: String)
  /// what is at `path`. When `follow` is false a symlink describes itself, not
  /// what it points at. The answer is Info, for node:fs stat and lstat
  Stat(path: String, follow: Bool)
  /// the text the symlink at `path` holds, as written. The answer
  /// is Text, for node:fs readlink
  Readlink(path: String)
  /// what the directory at `path` holds, in any order and without "." and
  /// "..". The answer is Entries, for node:fs readdir
  Readdir(path: String)
  /// every byte of the file at `path`. The answer is Bytes, for node:fs
  /// readFile
  ReadFile(path: String)
  /// make this http request and answer once the status and headers are in,
  /// however long the body is. `url` is an absolute http: or https: url,
  /// `headers` are sent as given and `body` is the bytes to send (none when
  /// empty). Don't follow redirects or unpack a compressed body, because fetch
  /// does both itself. The answer is Head
  Fetch(
    method: String,
    url: String,
    headers: List(#(String, String)),
    body: BitArray,
  )
  /// the next piece, of any size, of the body a Head gave this number.
  /// The answer is Bytes, or Done once the body has ended
  ReadBody(body: Int)
  /// the script won't read any more of this body, so the rest can be let go.
  /// The answer is Done
  DropBody(body: Int)
  /// this many bytes that nobody could guess. The answer is Bytes. node:crypto's
  /// randomBytes, randomUUID and the rest use it, and so does the web's
  /// crypto.getRandomValues
  Random(bytes: Int)
  /// write these bytes to the process's standard output or error, for
  /// process.stdout.write and process.stderr.write
  Write(stream: Stream, data: BitArray)
}

pub type Stream {
  Stdout
  Stderr
}

pub type Answer {
  Done
  Text(String)
  Bytes(BitArray)
  Info(FileInfo)
  Entries(List(Entry))
  /// the answer to a Fetch: the status code and reason, the headers in the
  /// order they came, and a number the embedder picks for the body, which
  /// ReadBody and DropBody then use
  Head(status: Int, reason: String, headers: List(#(String, String)), body: Int)
}

pub type FileKind {
  File
  Directory
  Symlink
  Fifo
  Socket
  CharDevice
  BlockDevice
}

/// what stat reports for one file, field for field. `permissions` is the mode
/// without the bits that say what kind of file it is, such as 0o644. The
/// four times are nanoseconds since the start of 1970
pub type FileInfo {
  FileInfo(
    kind: FileKind,
    permissions: Int,
    size: Int,
    dev: Int,
    ino: Int,
    nlink: Int,
    uid: Int,
    gid: Int,
    rdev: Int,
    blksize: Int,
    blocks: Int,
    atime: Int,
    mtime: Int,
    ctime: Int,
    birthtime: Int,
  )
}

/// one name in a directory
pub type Entry {
  Entry(name: String, kind: FileKind)
}

/// why a want was not met, as a posix error name such as "ENOENT" or "EACCES"
pub type Failure {
  Failure(code: String)
}

/// the two doors all io goes through
pub type Io {
  Io(
    /// answer before returning. The calling process is blocked until then
    now: fn(Want) -> Result(Answer, Failure),
    /// keep the slip and return; answer it later with spark.answer
    later: fn(Want, ReplySlip) -> Nil,
  )
}

/// the right to answer one want that went through the `later` door
pub opaque type ReplySlip {
  ReplySlip(ticket: host.Ticket)
}

/// asks through `now`, for a native behind a sync api. A failure is thrown as
/// its code
pub fn ask(
  io: Io,
  ctx: host.Context(host),
  want: Want,
) -> #(Result(JsVal, JsVal), host.Context(host)) {
  encode(ctx, io.now(want))
}

/// asks through `later`, for a native behind an async api. Returns a promise
/// that settles when the slip is answered, or rejects with the failure's code
pub fn ask_later(
  io: Io,
  ctx: host.Context(host),
  want: Want,
) -> #(Result(JsVal, JsVal), host.Context(host)) {
  let #(promise, ticket, ctx) = host.suspend(ctx)
  io.later(want, ReplySlip(ticket:))
  #(Ok(promise), ctx)
}

pub fn settle(
  slip: ReplySlip,
  ctx: host.Context(host),
  result: Result(Answer, Failure),
) -> #(host.ResumeOutcome, host.Context(host)) {
  let #(encoded, ctx) = encode(ctx, result)
  host.resume(ctx, slip.ticket, encoded)
}

fn encode(
  ctx: host.Context(host),
  result: Result(Answer, Failure),
) -> #(Result(JsVal, JsVal), host.Context(host)) {
  case result {
    Ok(Done) -> #(Ok(mk_undefined()), ctx)
    Ok(Text(text)) -> #(Ok(mk_string(text)), ctx)
    Ok(Bytes(bytes)) -> {
      let #(buffer, ctx) = host.array_buffer(ctx, bytes)
      #(Ok(buffer), ctx)
    }
    Ok(Info(info)) -> {
      let #(fields, ctx) = host.array(ctx, stat_fields(info))
      #(Ok(fields), ctx)
    }
    Ok(Entries(entries)) -> {
      let flat =
        list.flat_map(entries, fn(entry) {
          [mk_string(entry.name), mk_int(dirent_type(entry.kind))]
        })
      let #(array, ctx) = host.array(ctx, flat)
      #(Ok(array), ctx)
    }
    Ok(Head(status:, reason:, headers:, body:)) -> {
      let flat =
        list.flat_map(headers, fn(header) {
          [mk_string(header.0), mk_string(header.1)]
        })
      let #(headers, ctx) = host.array(ctx, flat)
      let #(head, ctx) =
        host.array(ctx, [
          mk_int(status),
          mk_string(reason),
          headers,
          mk_int(body),
        ])
      #(Ok(head), ctx)
    }
    Error(Failure(code:)) -> #(Error(mk_string(code)), ctx)
  }
}

// the eighteen numbers node's own stat hands its javascript, in its order:
// dev, mode, nlink, uid, gid, rdev, blksize, ino, size, blocks, then each
// time as whole seconds and the nanoseconds left over
fn stat_fields(info: FileInfo) -> List(JsVal) {
  let mode = int.bitwise_or(kind_bits(info.kind), info.permissions)
  [
    info.dev,
    mode,
    info.nlink,
    info.uid,
    info.gid,
    info.rdev,
    info.blksize,
    info.ino,
    info.size,
    info.blocks,
  ]
  |> list.append(
    list.flat_map(
      [info.atime, info.mtime, info.ctime, info.birthtime],
      fn(time) { [time / 1_000_000_000, time % 1_000_000_000] },
    ),
  )
  |> list.map(number)
}

// a javascript number holds whole numbers exactly only up to 2^53
fn number(n: Int) -> JsVal {
  case n > 9_007_199_254_740_992 || n < -9_007_199_254_740_992 {
    True -> mk_number(JFloat(int.to_float(n)))
    False -> mk_int(n)
  }
}

// the file-type bits of a posix mode
fn kind_bits(kind: FileKind) -> Int {
  case kind {
    File -> 0o100000
    Directory -> 0o040000
    Symlink -> 0o120000
    Fifo -> 0o010000
    Socket -> 0o140000
    CharDevice -> 0o020000
    BlockDevice -> 0o060000
  }
}

// libuv's numbers for the kind of a directory entry
fn dirent_type(kind: FileKind) -> Int {
  case kind {
    File -> 1
    Directory -> 2
    Symlink -> 3
    Fifo -> 4
    Socket -> 5
    CharDevice -> 6
    BlockDevice -> 7
  }
}
