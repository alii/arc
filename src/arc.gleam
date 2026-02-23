import arc/eval
import gleam/io
import gleam/string

@external(erlang, "arc_ffi", "read_stdin")
fn read_stdin() -> String

pub fn main() -> Nil {
  let input = read_stdin()
  case string.trim(input) {
    "" -> io.println("arc: JavaScript engine for the BEAM")
    source ->
      case eval.eval(source) {
        Ok(result) -> io.println(result)
        Error(err) -> io.println_error(err)
      }
  }
}
