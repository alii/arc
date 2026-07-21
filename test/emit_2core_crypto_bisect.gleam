//// Bisect where crypto's encrypt/decrypt diverges between compiled and
//// interpreted paths. The full bench throws "Crypto operation failed" —
//// meaning encrypt+decrypt both RUN but the round-trip fails, so this is a
//// value-level bug not a crash. Strategy: run crypto with the entry replaced
//// by progressively-later print points and diff stdout.
////
////     gleam run -m emit_2core_crypto_bisect

import emit_2core_harness as harness
import gleam/io
import gleam/string
import simplifile
import twocore/pipeline

fn diff(name: String, entry: String) {
  let assert Ok(src) = simplifile.read("bench/v8-v7/crypto.js")
  // crypto_run.js prefix (alert/print/BenchmarkSuite shims); tail is our probe.
  let full =
    "function alert(s){throw new Error(s)}\n"
    <> "function print(s){console.log(s)}\n"
    <> "function BenchmarkSuite(){}\n"
    <> "function Benchmark(){}\n"
    <> src
    <> "\n"
    <> entry
  let c = harness.run_compiled(full)
  let i = harness.run_interpreted(full)
  case c, i {
    pipeline.DiffRun(result: Ok(_), stdout: cs),
      pipeline.DiffRun(result: Ok(_), stdout: is)
    ->
      case cs == is {
        True ->
          io.println(
            "  same  " <> name <> " → " <> string.slice(show(cs), 0, 60),
          )
        False -> {
          io.println("  DIFF  " <> name)
          io.println("        compiled: " <> string.slice(show(cs), 0, 200))
          io.println("        interp:   " <> string.slice(show(is), 0, 200))
        }
      }
    pipeline.DiffRun(result: Error(e), ..), _ ->
      io.println("  CFAIL " <> name <> " → " <> string.slice(e, 0, 200))
    _, pipeline.DiffRun(result: Error(e), ..) ->
      io.println("  IFAIL " <> name <> " → " <> string.slice(e, 0, 200))
  }
}

fn show(b: BitArray) -> String {
  string.inspect(b)
}

pub fn main() {
  io.println("crypto bisect — compiled vs interpreted differential")
  // Globals seeded at load time.
  diff("dbits", "setupEngine(am3,28);console.log(String(dbits))")
  diff("BI_DV", "setupEngine(am3,28);console.log(String(BI_DV))")
  diff("BI_FV", "setupEngine(am3,28);console.log(String(BI_FV))")
  diff("nbv", "setupEngine(am3,28);console.log(nbv(5).array[0])")
  diff(
    "ctor_dispatch",
    "setupEngine(am3,28);var a='ff';console.log((a!=null)+' '+(\"number\"==typeof a)+' '+(16==null))",
  )
  diff("intAt", "setupEngine(am3,28);console.log(String(intAt('ff',0)))")
  diff("BI_RC_102", "setupEngine(am3,28);console.log(String(BI_RC[102]))")
  diff(
    "fromString_trace",
    "setupEngine(am3,28);var b=new BigInteger(null);b.fromString('ff',16);console.log(String(b.t)+' '+String(b.array[0]))",
  )
  diff(
    "fromString_manual",
    "setupEngine(am3,28);var arr=new Array();var t=0;var s='ff';var i=s.length;var sh=0;while(--i>=0){var x=intAt(s,i);if(sh==0)arr[t++]=x;else arr[t-1]|=x<<sh;sh+=4};console.log(String(t)+' '+String(arr[0]))",
  )
  diff("typeof_string", "console.log(typeof 'ff')")
  diff("s_length", "var s='ff';console.log(String(s.length))")
  diff(
    "predec_while",
    "var i=2;var n=0;while(--i>=0)n++;console.log(String(n))",
  )
  diff(
    "parse_hex",
    "setupEngine(am3,28);var b=new BigInteger('ff',16);console.log(String(b.array[0])+' t='+b.t)",
  )
  diff(
    "parse_long_hex",
    "setupEngine(am3,28);var b=new BigInteger('a5261939975948bb7a58dffe5ff54e65f0498f9175f5a09288810b8975871e99',16);console.log(String(b.t)+' '+String(b.array[0])+' '+String(b.array[1]))",
  )
  diff(
    "toString16",
    "setupEngine(am3,28);var b=new BigInteger('deadbeef',16);console.log(b.toString(16))",
  )
  diff(
    "add",
    "setupEngine(am3,28);var a=new BigInteger('ffff',16);var b=new BigInteger('1',16);console.log(a.add(b).toString(16))",
  )
  diff(
    "mul",
    "setupEngine(am3,28);var a=new BigInteger('ffff',16);var b=new BigInteger('ffff',16);console.log(a.multiply(b).toString(16))",
  )
  diff(
    "modpow",
    "setupEngine(am3,28);var a=new BigInteger('3',16);var e=new BigInteger('5',16);var m=new BigInteger('7',16);console.log(a.modPow(e,m).toString(16))",
  )
  diff("encrypt", "encrypt();console.log(encrypted.substring(0,40))")
  diff("roundtrip", "encrypt();decrypt();console.log('ok')")
}
