// sink.mjs — where verdicts land.
//
// The judging surface runs in a browser; the loop runs on a Mac. This is the
// seam between them: a tiny CORS-open endpoint that appends one JSON line per
// verdict and serves the queue back. In production this is a netlify function
// against Mongo. On a laptop it's a file, and a file is enough — the whole
// point of the prototype is that one person's taste fits in a text file.
//
//   node cancelok/sink.mjs            # → http://localhost:8901
//   POST /verdict  { code, verdict, dwellMs, taps, failed }
//   GET  /queue    → the candidates awaiting judgment
//   GET  /taste    → the pheromone block (what the generator will be told)

import { createServer } from "node:http";
import { appendFileSync, readFileSync, existsSync } from "node:fs";
import { VERDICTS, QUEUE, pheromone, latest } from "./taste.mjs";

const PORT = parseInt(process.env.PORT || "8901", 10);

const send = (res, code, body, type = "application/json") => {
  res.writeHead(code, {
    "content-type": type,
    "access-control-allow-origin": "*",
    "access-control-allow-headers": "content-type",
    "access-control-allow-methods": "GET,POST,OPTIONS",
  });
  res.end(typeof body === "string" ? body : JSON.stringify(body));
};

const body = (req) =>
  new Promise((ok, no) => {
    let s = "";
    req.on("data", (c) => (s += c));
    req.on("end", () => {
      try {
        ok(JSON.parse(s || "{}"));
      } catch (e) {
        no(e);
      }
    });
  });

createServer(async (req, res) => {
  const url = new URL(req.url, "http://x");
  if (req.method === "OPTIONS") return send(res, 204, "");

  if (req.method === "POST" && url.pathname === "/verdict") {
    let v;
    try {
      v = await body(req); // a browser is a boundary — guard it
    } catch {
      return send(res, 400, { error: "bad json" });
    }
    if (!v.code || !["ok", "cancel"].includes(v.verdict))
      return send(res, 400, { error: "need { code, verdict: ok|cancel }" });
    const line = {
      code: v.code,
      verdict: v.verdict,
      dwellMs: v.dwellMs ?? 0,
      taps: v.taps ?? 0,
      fps: v.fps ?? 0, // real fps on a real device — the gate can't see this
      fpsMin: v.fpsMin ?? 0,
      revisit: v.revisit ?? 0,
      session: v.session ?? null,
      failed: !!v.failed,
      at: new Date().toISOString(),
    };
    appendFileSync(VERDICTS, JSON.stringify(line) + "\n");
    console.log(
      `${line.verdict === "ok" ? "✅ ok    " : "🚫 cancel"} ${line.code}` +
        `  ${Math.round(line.dwellMs / 100) / 10}s${line.taps ? `  ${line.taps}▸` : ""}`,
    );
    return send(res, 200, { ok: true });
  }

  if (url.pathname === "/queue")
    return send(res, 200, existsSync(QUEUE) ? readFileSync(QUEUE, "utf8") : { items: [] });

  if (url.pathname === "/taste") return send(res, 200, await pheromone(), "text/plain");

  if (url.pathname === "/verdicts") return send(res, 200, latest());

  send(res, 404, { error: "not found" });
}).listen(PORT, () => console.log(`🗳️  cancelok sink → http://localhost:${PORT}`));
