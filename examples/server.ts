/// Arc Web REPL — browser-based JS console powered by Arc.
///
/// Usage: cd examples && bun run server.ts
/// Open http://localhost:3000

const PORT = 3000;
const PROJECT_ROOT = new URL("..", import.meta.url).pathname;

const HTML = /* html */ `<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Arc Console</title>
<style>
:root {
  --bg: #242424;
  --surface: #1e1e1e;
  --border: #3c3c3c;
  --text: #d4d4d4;
  --muted: #808080;
  --prompt: #4e9cff;
  --accent: #8ab4f8;
  --number: #b5cea8;
  --string: #ce9178;
  --bool: #569cd6;
  --symbol: #c586c0;
  --err: #f28b82;
  --err-icon: #ff6b6b;
  --err-border: #4e2020;
}
* { margin: 0; padding: 0; box-sizing: border-box; }
body {
  font: 12px/1.4 Menlo, "DejaVu Sans Mono", Consolas, monospace;
  background: var(--bg);
  color: var(--text);
  height: 100vh;
  display: flex;
  flex-direction: column;
}
.toolbar {
  height: 28px;
  background: var(--surface);
  border-bottom: 1px solid var(--border);
  display: flex;
  align-items: center;
  padding: 0 8px;
  font-size: 11px;
  color: var(--muted);
}
.toolbar .tab {
  padding: 4px 8px;
  border-bottom: 2px solid var(--accent);
  color: var(--accent);
  font-weight: 500;
}
.toolbar button {
  margin-left: auto;
  background: none;
  border: none;
  color: var(--muted);
  cursor: pointer;
  padding: 2px 6px;
  border-radius: 2px;
  font-size: 12px;
}
.toolbar button:hover { background: var(--border); color: var(--text); }
#output { flex: 1; overflow-y: auto; }
.row {
  display: flex;
  align-items: baseline;
  padding: 2px 12px 2px 4px;
  min-height: 20px;
  border-bottom: 1px solid var(--border);
}
.row.err {
  background: rgba(255, 0, 0, 0.07);
  border-color: var(--err-border);
}
.gutter {
  width: 20px;
  flex-shrink: 0;
  text-align: center;
  user-select: none;
  color: var(--muted);
}
.row.in .gutter { color: var(--prompt); }
.row.err .gutter { color: var(--err-icon); font-size: 10px; }
.row .body {
  flex: 1;
  white-space: pre-wrap;
  word-break: break-all;
}
.row.err .body { color: var(--err); }
#prompt {
  display: flex;
  align-items: baseline;
  padding: 3px 12px 3px 4px;
  border-top: 1px solid var(--border);
}
#prompt .gutter { color: var(--prompt); }
textarea {
  flex: 1;
  background: none;
  color: var(--text);
  border: none;
  font: inherit;
  resize: none;
  outline: none;
  min-height: 17px;
  max-height: 200px;
}
textarea::placeholder { color: #555; }
</style>
</head>
<body>
<div class="toolbar">
  <span class="tab">Console</span>
  <button id="clear" title="Clear console">\u2718</button>
</div>
<div id="output"></div>
<div id="prompt">
  <span class="gutter">&gt;</span>
  <textarea id="code" rows="1" placeholder="Expression" spellcheck="false" autofocus></textarea>
</div>
<script>
const $ = document.getElementById.bind(document);
const output = $("output");
const code = $("code");
$("clear").onclick = () => output.innerHTML = "";

const history = [];
let histIdx = -1;

const VALUE_COLORS = {
  undefined: "var(--muted)", null: "var(--muted)",
  true: "var(--bool)", false: "var(--bool)",
  NaN: "var(--number)", Infinity: "var(--number)", "-Infinity": "var(--number)",
};

function valueColor(val) {
  if (val in VALUE_COLORS) return VALUE_COLORS[val];
  if (val[0] === '"') return "var(--string)";
  if (val.startsWith("Symbol(")) return "var(--symbol)";
  if (/^-?\\d/.test(val)) return "var(--number)";
  return "";
}

function addRow(type, gutter, body, color) {
  const row = document.createElement("div");
  row.className = "row " + type;
  const g = document.createElement("span");
  g.className = "gutter";
  g.textContent = gutter;
  const b = document.createElement("span");
  b.className = "body";
  b.textContent = body;
  if (color) b.style.color = color;
  row.append(g, b);
  output.appendChild(row);
  return row;
}

code.onkeydown = (e) => {
  if (e.key === "Enter" && !e.shiftKey) { e.preventDefault(); evaluate(); return; }
  const singleLine = !code.value.includes("\\n");
  if (e.key === "ArrowUp" && singleLine) {
    if (histIdx < history.length - 1) code.value = history[history.length - 1 - ++histIdx];
    e.preventDefault();
  }
  if (e.key === "ArrowDown" && singleLine) {
    code.value = histIdx > 0 ? history[history.length - 1 - --histIdx] : (histIdx = -1, "");
    e.preventDefault();
  }
};

code.oninput = () => { code.style.height = "auto"; code.style.height = Math.min(code.scrollHeight, 200) + "px"; };

async function evaluate() {
  const src = code.value.trim();
  if (!src) return;
  history.push(src);
  histIdx = -1;
  code.value = "";
  code.style.height = "auto";

  addRow("in", ">", src);

  try {
    const res = await fetch("/eval", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ code: src }),
    });
    const data = await res.json();
    if (data.ok) {
      addRow("out", "\u21B5", data.result, valueColor(data.result));
    } else {
      addRow("err", "\u2715", data.error);
    }
  } catch (e) {
    addRow("err", "\u2715", "Network error: " + e.message);
  }
  output.scrollTop = output.scrollHeight;
  code.focus();
}
</script>
</body>
</html>`;

function parseGleamOutput(exitCode: number, stdout: string, stderr: string) {
  const result = stdout.trim();
  // Filter out gleam's own build output from stderr
  const appErrors = stderr
    .split("\n")
    .filter((l) => !/^\s*(Compil|Running|Download|$)/.test(l))
    .join("\n")
    .trim();

  if (appErrors) return { ok: false, error: appErrors };
  if (exitCode !== 0) return { ok: false, error: "Process exited with code " + exitCode };
  return { ok: true, result: result || "undefined" };
}

const server = Bun.serve({
  port: PORT,
  async fetch(req) {
    const url = new URL(req.url);

    if (url.pathname === "/" && req.method === "GET") {
      return new Response(HTML, { headers: { "Content-Type": "text/html; charset=utf-8" } });
    }

    if (url.pathname === "/eval" && req.method === "POST") {
      try {
        const { code = "" } = (await req.json()) as { code?: string };
        if (!code.trim()) return Response.json({ ok: true, result: "undefined" });

        const proc = Bun.spawn(["gleam", "run"], {
          cwd: PROJECT_ROOT,
          stdin: "pipe",
          stdout: "pipe",
          stderr: "pipe",
        });
        proc.stdin.write(code);
        proc.stdin.end();

        const [exitCode, stdout, stderr] = await Promise.all([
          proc.exited,
          new Response(proc.stdout).text(),
          new Response(proc.stderr).text(),
        ]);

        return Response.json(parseGleamOutput(exitCode, stdout, stderr));
      } catch (err) {
        return Response.json({ ok: false, error: String(err) }, { status: 500 });
      }
    }

    return new Response("Not Found", { status: 404 });
  },
});

console.log(`Arc REPL server running at http://localhost:${server.port}`);
