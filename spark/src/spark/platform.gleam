/// the posix world a runtime says it lives in; spark never works this out
/// from the machine it runs on
pub type Platform {
  Linux
  Darwin
}

/// what `process.platform` and `os.platform()` report
pub fn name(platform: Platform) -> String {
  case platform {
    Linux -> "linux"
    Darwin -> "darwin"
  }
}
