#!/usr/bin/env node
// puppet-view.mjs — live fleet viewer for puppet screencast frames.
//
// puppet `watch` keeps the latest JPEG per window at
// ~/.local/share/puppet/frames/<name>.jpg (atomic rename). This serves:
//   GET /            a dark grid page, one <img> per frame, all live
//   GET /stream/<n>  multipart/x-mixed-replace MJPEG of that frame file
//   GET /sheet.jpg   one-shot contact sheet of ALL frames (ffmpeg xstack,
//                    ~30ms) — what a driving agent curls to see every window
//
// Push-based: fs.watch on the FRAMES DIR (file watchers chase the old inode
// across renames; directory watchers see every landing). Chrome renders
// MJPEG in <img> natively — sub-50ms from frame landing to glass.
//
//   node slab/bin/puppet-view.mjs [port]     # default 7777
//   open http://localhost:7777
//
// PUBLIC repo: no machine names here — frame names come from whatever the
// operator passes to `puppet watch`.

import { execFile } from "node:child_process";
import { readdirSync, readFileSync, watch, existsSync, mkdirSync } from "node:fs";
import http from "node:http";
import { homedir } from "node:os";
import { join } from "node:path";

const FRAMES_DIR = join(homedir(), ".local", "share", "puppet", "frames");
mkdirSync(FRAMES_DIR, { recursive: true });
const PORT = Number(process.argv[2] || 7777);

const names = () =>
  readdirSync(FRAMES_DIR)
    .filter(f => f.endsWith(".jpg"))
    .map(f => f.slice(0, -4));

// Contact sheet: montage every current frame into one JPEG (benchmarked
// ~30ms for 5×900w frames on an M-series). Grid is ~square; single frame
// passes through untouched.
function sheet(res) {
  const files = names().map(n => join(FRAMES_DIR, n + ".jpg"));
  if (!files.length) { res.writeHead(404); return res.end("no frames"); }
  if (files.length === 1) {
    res.writeHead(200, { "Content-Type": "image/jpeg", "Cache-Control": "no-store" });
    return res.end(readFileSync(files[0]));
  }
  // frames arrive at whatever width each watch chose — normalize every tile
  // (scale + pad to TW×TH) so xstack can use literal grid coordinates;
  // heterogeneous inputs otherwise scramble the w0/h0-relative layout.
  const TW = 640, TH = 400;
  const cols = Math.ceil(Math.sqrt(files.length));
  const norm = files
    .map((_, i) =>
      `[${i}:v]scale=${TW}:${TH}:force_original_aspect_ratio=decrease,` +
      `pad=${TW}:${TH}:(ow-iw)/2:(oh-ih)/2:black[t${i}]`)
    .join(";");
  const layout = files.map((_, i) => `${(i % cols) * TW}_${Math.floor(i / cols) * TH}`).join("|");
  const refs = files.map((_, i) => `[t${i}]`).join("");
  execFile(
    "ffmpeg",
    ["-y", "-loglevel", "error", ...files.flatMap(f => ["-i", f]),
     "-filter_complex", `${norm};${refs}xstack=inputs=${files.length}:layout=${layout}:fill=black`,
     "-q:v", "6", "-f", "image2", "pipe:1"],
    { encoding: "buffer", maxBuffer: 32 * 1024 * 1024 },
    (err, stdout) => {
      if (err) { res.writeHead(503); return res.end("ffmpeg failed: " + err.message); }
      res.writeHead(200, { "Content-Type": "image/jpeg", "Cache-Control": "no-store" });
      res.end(stdout);
    },
  );
}

http
  .createServer((req, res) => {
    if (req.url.startsWith("/sheet.jpg")) return sheet(res);
    const m = req.url.match(/^\/stream\/([\w.-]+)/);
    if (!m) {
      const tiles = names()
        .map(
          n =>
            `<figure style="margin:0"><figcaption style="color:#9a9;font:12px ui-monospace;padding:2px 6px">${n}</figcaption><img src="/stream/${n}" style="width:100%;display:block;border:1px solid #233"></figure>`,
        )
        .join("");
      res.writeHead(200, { "Content-Type": "text/html" });
      res.end(
        `<!doctype html><title>puppet fleet</title><body style="background:#0b0f10;margin:0;padding:8px"><div style="display:grid;grid-template-columns:repeat(auto-fit,minmax(440px,1fr));gap:8px">${tiles || '<p style="color:#677">no frames yet — start one with: puppet watch &lt;machine&gt; ~/.local/share/puppet/frames/&lt;name&gt;.jpg --target=…</p>'}</div><script>setInterval(()=>{if(document.images.length===0)location.reload()},3000)</script>`,
      );
      return;
    }
    const file = join(FRAMES_DIR, m[1] + ".jpg");
    if (!existsSync(file)) {
      res.writeHead(404);
      res.end("no such frame");
      return;
    }
    res.writeHead(200, {
      "Content-Type": "multipart/x-mixed-replace; boundary=F",
      "Cache-Control": "no-store",
    });
    const send = () => {
      try {
        const b = readFileSync(file);
        res.write(`--F\r\nContent-Type: image/jpeg\r\nContent-Length: ${b.length}\r\n\r\n`);
        res.write(b);
      } catch {}
    };
    send();
    // debounce: each atomic rename fires two dir events; one part per frame
    let timer = null;
    const w = watch(FRAMES_DIR, (_, f) => {
      if (f !== m[1] + ".jpg") return;
      clearTimeout(timer);
      timer = setTimeout(send, 10);
    });
    req.on("close", () => { clearTimeout(timer); w.close(); });
  })
  .listen(PORT, () => console.log(`puppet-view: http://localhost:${PORT} (frames: ${FRAMES_DIR})`));
