import arc/host
import arc/rt/types.{type JsVal, mk_string, mk_undefined}

/// everything a script can ask the outside world for
///
/// spark performs none of these; each one reaches the embedder through `Io`
pub type Want {
  /// may `path` be reached with this access mode (0 exists, 4 read, 2 write,
  /// 1 execute, or-ed together); behind node:fs access and existsSync
  Access(path: String, mode: Int)
  /// the canonical absolute path for `path`, with every symlink followed;
  /// answered with Text. A relative path is the embedder's to resolve
  Realpath(path: String)
  /// write these bytes to the process's standard output or error; behind
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
}

/// why a want was not met, as a posix error name such as "ENOENT" or "EACCES"
pub type Failure {
  Failure(code: String)
}

/// the two doors all io goes through
pub type Io {
  Io(
    /// answer before returning; the calling process is blocked meanwhile
    now: fn(Want) -> Result(Answer, Failure),
    /// keep the slip and return; answer it later with spark.answer
    later: fn(Want, ReplySlip) -> Nil,
  )
}

/// the right to answer one want that went through the `later` door
pub opaque type ReplySlip {
  ReplySlip(ticket: host.Ticket)
}

/// for a native behind a sync api; a failure is thrown as its code
pub fn ask(
  io: Io,
  ctx: host.Context(host),
  want: Want,
) -> #(Result(JsVal, JsVal), host.Context(host)) {
  #(encode(io.now(want)), ctx)
}

/// for a native behind an async api; returns a promise that settles when the
/// slip is answered, rejecting with the failure's code
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
  host.resume(ctx, slip.ticket, encode(result))
}

fn encode(result: Result(Answer, Failure)) -> Result(JsVal, JsVal) {
  case result {
    Ok(Done) -> Ok(mk_undefined())
    Ok(Text(text)) -> Ok(mk_string(text))
    Error(Failure(code:)) -> Error(mk_string(code))
  }
}
