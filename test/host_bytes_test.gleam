import arc/engine.{type Engine, Returned}
import arc/host
import arc/rt/types.{mk_string}
import gleam/bit_array
import gleam/option.{None, Some}

fn new_engine() -> Engine(Nil) {
  engine.new()
  |> engine.define_fn("fromBytes", 0, fn(ctx, _args, _this) {
    let #(buffer, ctx) = host.array_buffer(ctx, <<1, 2, 250>>)
    #(Ok(buffer), ctx)
  })
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

pub fn array_buffer_holds_the_given_bytes_test() {
  assert eval_string(
      "var b = fromBytes(); [b instanceof ArrayBuffer, b.byteLength, Array.from(new Uint8Array(b))].join()",
    )
    == "'true,3,1,2,250'"
}

pub fn bytes_reads_buffers_and_views_test() {
  assert eval_string(
      "
      var bytes = new Uint8Array([0, 17, 34, 51, 68, 85]);
      [
        hex(bytes.buffer),
        hex(bytes),
        hex(bytes.subarray(1, 3)),
        hex(new Uint16Array(bytes.buffer, 2, 2)),
        hex(new DataView(bytes.buffer, 4)),
        hex(new DataView(bytes.buffer, 1, 2)),
        hex(new Uint8Array(0)),
      ].join()
      ",
    )
    == "'001122334455,001122334455,1122,22334455,4455,1122,'"
}

pub fn bytes_is_none_for_other_values_test() {
  assert eval_string("[hex('text'), hex({}), hex([1, 2]), hex(5)].join()")
    == "'none,none,none,none'"
}

pub fn bytes_is_none_for_an_out_of_bounds_view_test() {
  assert eval_string(
      "
      var buffer = new ArrayBuffer(8, { maxByteLength: 16 });
      var view = new Uint8Array(buffer, 4, 4);
      buffer.resize(6);
      hex(view)
      ",
    )
    == "'none'"
}
