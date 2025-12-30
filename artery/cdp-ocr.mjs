#!/usr/bin/env node
import { execSync } from "child_process";
import WebSocket from "ws";
import fs from "fs";

const CDP_PORT = 9222;
const SCREENSHOT_PATH = "/tmp/cdp-screenshot.png";

const c = {reset: "\x1b[0m", bold: "\x1b[1m", dim: "\x1b[2m", red: "\x1b[31m", green: "\x1b[32m", yellow: "\x1b[33m", blue: "\x1b[34m", magenta: "\x1b[35m", cyan: "\x1b[36m", white: "\x1b[37m", bgBlue: "\x1b[44m"};

async function getTargets() { return (await fetch(`http://localhost:${CDP_PORT}/json`)).json(); }
async function findPage(p) { return (await getTargets()).find(t => t.type === "page" && t.url?.includes(p)); }

async function cdpCmd(wsUrl, method, params = {}) {
  return new Promise((resolve, reject) => {
    const ws = new WebSocket(wsUrl);
    const id = Math.floor(Math.random() * 100000);
    const timer = setTimeout(() => { ws.terminate(); reject(new Error("Timeout")); }, 15000);
    ws.on("open", () => ws.send(JSON.stringify({ id, method, params })));
    ws.on("message", data => {
      const msg = JSON.parse(data);
      if (msg.id === id) { clearTimeout(timer); ws.terminate(); msg.error ? reject(new Error(msg.error.message)) : resolve(msg.result); }
    });
    ws.on("error", e => { clearTimeout(timer); reject(e); });
  });
}

async function screenshot(wsUrl) {
  const r = await cdpCmd(wsUrl, "Page.captureScreenshot", { format: "png" });
  fs.writeFileSync(SCREENSHOT_PATH, Buffer.from(r.data, "base64"));
  return SCREENSHOT_PATH;
}

function ocr(img) {
  try {
    const tsv = execSync(`tesseract "${img}" stdout --psm 6 -c tessedit_create_tsv=1 2>/dev/null`, { encoding: "utf8" });
    const lines = tsv.trim().split("\n"), h = lines[0].split("\t");
    const idx = { text: h.indexOf("text"), left: h.indexOf("left"), top: h.indexOf("top"), width: h.indexOf("width"), height: h.indexOf("height") };
    return lines.slice(1).map(l => { const cols = l.split("\t"), text = cols[idx.text]?.trim(); return text ? { text, x: +cols[idx.left] || 0, y: +cols[idx.top] || 0, w: +cols[idx.width] || 0, h: +cols[idx.height] || 0 } : null; }).filter(Boolean);
  } catch { return []; }
}

function imgSize(img) {
  try { const m = execSync(`file "${img}"`, { encoding: "utf8" }).match(/(\d+)\s*x\s*(\d+)/); return m ? { width: +m[1], height: +m[2] } : { width: 800, height: 600 }; }
  catch { return { width: 800, height: 600 }; }
}

function render(items, sz, tw = 100, th = 35) {
  const sx = tw / sz.width, sy = th / sz.height;
  const grid = Array(th).fill(null).map(() => Array(tw).fill(" "));
  const cols = [c.cyan, c.yellow, c.green, c.magenta, c.blue, c.white];
  items.forEach((it, i) => { const tx = Math.floor(it.x * sx), ty = Math.floor(it.y * sy); for (let j = 0; j < it.text.length && tx + j < tw; j++) if (ty >= 0 && ty < th && tx + j >= 0) grid[ty][tx + j] = cols[i % cols.length] + it.text[j] + c.reset; });
  console.log(`\n${c.bgBlue}${c.white}${c.bold} CDP OCR ${c.reset}  ${c.dim}${sz.width}x${sz.height}${c.reset}\n`);
  console.log(c.dim + "+" + "-".repeat(tw) + "+" + c.reset);
  grid.forEach(r => console.log(c.dim + "|" + c.reset + r.join("") + c.dim + "|" + c.reset));
  console.log(c.dim + "+" + "-".repeat(tw) + "+" + c.reset);
  const phrases = []; let cur = null;
  items.forEach(it => { if (!cur || it.x - cur.ex > 20 || Math.abs(it.y - cur.y) > 10) { if (cur) phrases.push(cur); cur = { text: it.text, x: it.x, y: it.y, ex: it.x + it.w, cx: it.x + it.w / 2, cy: it.y + it.h / 2 }; } else { cur.text += " " + it.text; cur.ex = it.x + it.w; cur.cx = (cur.x + cur.ex) / 2; } });
  if (cur) phrases.push(cur);
  console.log(`\n${c.bold}Clickable:${c.reset}\n`);
  phrases.forEach((p, i) => console.log(`  ${cols[i % cols.length]}*${c.reset} ${c.bold}${p.text.substring(0, 40).padEnd(40)}${c.reset} ${c.dim}(${Math.round(p.cx)}, ${Math.round(p.cy)})${c.reset}`));
  return phrases;
}

async function click(wsUrl, x, y) {
  await cdpCmd(wsUrl, "Input.dispatchMouseEvent", { type: "mousePressed", x, y, button: "left", clickCount: 1 });
  await cdpCmd(wsUrl, "Input.dispatchMouseEvent", { type: "mouseReleased", x, y, button: "left", clickCount: 1 });
  console.log(`${c.green}Clicked (${x}, ${y})${c.reset}`);
}

async function clickText(wsUrl, search, phrases) {
  const m = phrases.find(p => p.text.toLowerCase().includes(search.toLowerCase()));
  if (m) { console.log(`${c.cyan}Found "${m.text}"${c.reset}`); await click(wsUrl, Math.round(m.cx), Math.round(m.cy)); return true; }
  console.log(`${c.red}"${search}" not found${c.reset}`);
  return false;
}

async function main() {
  const args = process.argv.slice(2);
  const clickArg = args.find((_, i) => args[i - 1] === "--click");
  const url = args.find((_, i) => args[i - 1] === "--url") || "localhost:8888";
  const page = await findPage(url);
  if (!page) { console.error(`${c.red}No page matching "${url}"${c.reset}`); process.exit(1); }
  console.log(`${c.green}${page.title}${c.reset}\n${c.dim}  ${page.url}${c.reset}`);
  await screenshot(page.webSocketDebuggerUrl);
  const items = ocr(SCREENSHOT_PATH);
  const phrases = render(items, imgSize(SCREENSHOT_PATH));
  if (clickArg) { console.log(`\n${c.yellow}Clicking: "${clickArg}"${c.reset}`); await clickText(page.webSocketDebuggerUrl, clickArg, phrases); }
  console.log(`\n${c.dim}Screenshot: ${SCREENSHOT_PATH}${c.reset}`);
}

main().catch(e => { console.error(`${c.red}Error: ${e.message}${c.reset}`); process.exit(1); });
