// a js error not yet thrown, shared by compiler, rt and builtins

pub type ErrorKind {
  TypeError
  RangeError
  ReferenceError
  SyntaxError
  UriError
  EvalError
}

pub type JsError {
  JsError(kind: ErrorKind, message: String)
}

pub fn name(kind: ErrorKind) -> String {
  case kind {
    TypeError -> "TypeError"
    RangeError -> "RangeError"
    ReferenceError -> "ReferenceError"
    SyntaxError -> "SyntaxError"
    UriError -> "URIError"
    EvalError -> "EvalError"
  }
}
