#!/bin/bash
# Strip the octane BenchmarkSuite wrapper and emit a self-contained script
# that runs the entry function ONCE and prints "ok" (or throws). The
# resulting *_run.js is what emit_2core / arc-interp / qjs / bun all execute.
set -euo pipefail
cd "$(dirname "$0")"

shim='function alert(s){throw new Error(s)}
function print(s){console.log(s)}
function BenchmarkSuite(){}
function Benchmark(){}
'

# richards: entry = runRichards()
{ echo "$shim"; cat richards.js; echo 'runRichards();console.log("ok");'; } > richards_run.js

# deltablue: entry = deltaBlue(). Replaces Object.defineProperty(...inheritsFrom...)
# (lines 49–57) with an equivalent Function.prototype method — smaller JS surface.
{
  echo "$shim"
  cat <<'EOF'
Function.prototype.inheritsFrom = function(shuper){
  function Inheriter(){}
  Inheriter.prototype = shuper.prototype;
  this.prototype = new Inheriter();
  this.superConstructor = shuper;
};
EOF
  sed '49,57d' deltablue.js
  echo 'deltaBlue();console.log("ok");'
} > deltablue_run.js

# crypto: entry = encrypt() then decrypt()
{ echo "$shim"; cat crypto.js; echo 'encrypt();decrypt();console.log("ok");'; } > crypto_run.js

# raytrace: entry = renderScene()
{ echo "$shim"; cat raytrace.js; echo 'renderScene();console.log("ok");'; } > raytrace_run.js

wc -l *_run.js
