import arc/engine.{type Engine, Returned}
import arc/host
import arc/rt/types.{mk_string}
import gleam/bit_array
import gleam/option.{None, Some}

fn new_engine() -> Engine(Nil) {
  engine.new()
  |> engine.define_fn("hex", 1, fn(ctx, args, _this) {
    let text = case host.bytes(ctx, host.first_arg(args)) {
      Some(bytes) -> bit_array.base16_encode(bytes)
      None -> "none"
    }
    #(Ok(mk_string(text)), ctx)
  })
}

fn eval_string(source: String) -> String {
  let assert Ok(#(Returned(value), eng)) = engine.eval(new_engine(), source)
  engine.inspect(eng, value)
}

// 100,000 bytes is far past the size where a written buffer becomes pieces
const big = "var a = new Uint8Array(100000);
  for (var i = 0; i < a.length; i += 997) a[i] = (i / 997) & 255;
"

pub fn element_writes_to_a_big_buffer_read_back_test() {
  assert eval_string(big <> "var wrong = 0;
      for (var i = 0; i < a.length; i++) {
        var want = i % 997 === 0 ? (i / 997) & 255 : 0;
        if (a[i] !== want) wrong++;
      }
      [wrong, a.length, a[997], a[99700], a[99999], a[100000]].join()") == "'0,100000,1,100,0,'"
}

pub fn wide_elements_keep_their_bytes_test() {
  assert eval_string(
      "var f = new Float64Array(2000);
      for (var i = 0; i < f.length; i++) f[i] = i + 0.5;
      var u = new Uint32Array(f.buffer, 8, 4);
      u[1] = 0xdeadbeef;
      var big64 = new BigInt64Array(f.buffer);
      big64[1999] = -5n;
      [f[0], f[127], f[128], f[1998], u[1].toString(16), big64[1999], new Uint8Array(f.buffer)[12].toString(16)].join()",
    )
    == "'0.5,127.5,128.5,1998.5,deadbeef,-5,ef'"
}

pub fn data_view_crosses_piece_edges_test() {
  assert eval_string(
      "var buffer = new ArrayBuffer(8192);
      var view = new DataView(buffer);
      view.setUint8(5000, 1);
      view.setUint32(1022, 0xa1b2c3d4);
      view.setFloat64(2045, 1.5, true);
      view.setBigUint64(4092, 0x0102030405060708n);
      var bytes = new Uint8Array(buffer);
      [view.getUint32(1022).toString(16), bytes[1023].toString(16), bytes[1024].toString(16),
       view.getFloat64(2045, true), view.getBigUint64(4092).toString(16), view.getUint16(4095).toString(16),
       view.getUint8(5000)].join()",
    )
    == "'a1b2c3d4,b2,c3,1.5,102030405060708,405,1'"
}

pub fn bulk_operations_still_work_on_a_buffer_in_pieces_test() {
  assert eval_string(big <> "a.fill(9, 2000, 3000);
      a.set([1, 2, 3], 1022);
      a.copyWithin(50000, 1020, 1030);
      var part = a.slice(1020, 1028);
      var sub = a.subarray(2999, 3001);
      sub[1] = 77;
      [a[1999], a[2000], a[2999], a[3000], Array.from(part).join(' '), Array.from(a.subarray(50000, 50006)).join(' '),
       a.indexOf(77), a.lastIndexOf(9), a.includes(3), new Uint8Array(a).length, a.buffer.slice(1022, 1025).byteLength].join()") == "'0,9,9,77,0 0 1 2 3 0 0 0,0 0 1 2 3 0,3000,8973,true,100000,3'"
}

pub fn host_bytes_reads_a_view_of_a_buffer_in_pieces_test() {
  assert eval_string(big <> "a.set([0xca, 0xfe, 0xba, 0xbe], 1022);
      [hex(a.subarray(1021, 1027)), hex(new DataView(a.buffer, 1023, 2)), hex(a.buffer).length, hex(a.subarray(0, 0))].join()") == "'00CAFEBABE00,FEBA,200000,'"
}

pub fn resize_transfer_and_detach_after_pieces_test() {
  assert eval_string(
      "var buffer = new ArrayBuffer(10000, { maxByteLength: 20000 });
      var a = new Uint8Array(buffer);
      a[9999] = 5; a[5000] = 6;
      buffer.resize(12000);
      a[11999] = 7;
      var after = [a.length, a[9999], a[5000], a[11999]].join(' ');
      buffer.resize(6000);
      var shrunk = [a.length, a[5000], a[5999]].join(' ');
      var moved = buffer.transfer(8000);
      var b = new Uint8Array(moved);
      [after, shrunk, buffer.detached, a.length, b.length, b[5000], b[7999]].join()",
    )
    == "'12000 5 6 7,6000 6 0,true,0,8000,6,0'"
}

pub fn a_small_buffer_behaves_the_same_test() {
  assert eval_string(
      "var a = new Uint8Array(64);
      for (var i = 0; i < 64; i++) a[i] = i * 3;
      [a[0], a[21], a[63], hex(a.subarray(1, 4))].join()",
    )
    == "'0,63,189,030609'"
}

pub fn range_methods_on_a_buffer_in_pieces_match_node_test() {
  assert eval_string(big <> "var v = a.subarray(1020, 1030);
      v.set([5, 3, 9, 1, 8, 2, 7, 0, 6, 4]);
      v.reverse();
      var reversed = Array.from(v).join(' ');
      v.sort();
      var sorted = Array.from(v).join(' ');
      var w = new Uint16Array(a.buffer, 2046, 4);
      w.set([3, 1, 2, 0]);
      w.sort();
      var wide = Array.from(w).join(' ');
      var other = new Uint8Array(6000);
      other[1] = 9;
      other.fill(2, 1000, 5000);
      other.copyWithin(900, 4990, 5010);
      var copied = Array.from(other.subarray(898, 922)).join('');
      var same = new Uint8Array(6000);
      same[0] = 1;
      same.set(same.subarray(0, 3000), 2);
      var overlap = [same[2], same[3], same[3001]].join(' ');
      var hex = [a.subarray(1020, 1026).toHex(), JSON.stringify(a.subarray(2000, 2004).setFromHex('cafebabe')),
        a.subarray(1999, 2005).toHex(), a.subarray(1022, 1027).toBase64()].join();
      [reversed, sorted, wide, copied, overlap, hex, a.toReversed()[100000 - 1 - 997], a.toSorted()[99999],
        a.with(1024, 250)[1024], a[1024]].join(' | ')") == "'4 6 0 7 2 8 1 9 3 5 | 0 1 2 3 4 5 6 7 8 9 | 0 1 2 3 | 002222222222000000000000 | 1 0 0 | 000102030405,{\"read\":8,\"written\":4},00cafebabe00,AgMEBQY= | 1 | 254 | 250 | 4'"
}
