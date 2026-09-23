// Decode the canonical AC WebM cues to native PCM, using installed Chrome.
// No network or third-party package dependency; outputs are build artifacts.
import { readFile, writeFile, mkdir, stat } from "node:fs/promises";
import { spawn } from "node:child_process";
import { fileURLToPath } from "node:url";
const root = fileURLToPath(new URL(".", import.meta.url));
const output = `${root}.build/No Paint Brushes.app/Contents/Resources/Sounds`;
await mkdir(output, { recursive: true });
const source = await readFile(`${root}../../system/public/aesthetic.computer/disks/nopaint.mjs`, "utf8");
const block = source.match(/const LEGACY_CUES = Object\.freeze\(\{([\s\S]*?)\}\);/)[1];
const cues = [...block.matchAll(/(?:"([^"]+)"|(\w+)):\s*"([^"]+)"/g)].map(m => [m[1] || m[2], m[3]]);
const pending = [];
for (const [name, file] of cues) {
  const input = `${root}../../system/public/nopaint.art/media/${file}`;
  const dest = `${output}/${name}.wav`;
  if ((await stat(dest).catch(() => null))?.mtimeMs >= (await stat(input)).mtimeMs) continue;
  pending.push({ input, dest });
}
if (!pending.length) process.exit(0);
const chrome = spawn(process.env.CHROME_PATH || "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome", [
  "--headless", "--disable-gpu", "--no-first-run", "--no-default-browser-check",
  "--remote-debugging-port=0", `--user-data-dir=${root}.build/audio-decoder`, "about:blank",
], { stdio: ["ignore", "ignore", "pipe"] });
let socket;
const timeout = setTimeout(() => { socket?.close(); chrome.kill(); process.exitCode = 1; }, 45000);
try {
  const url = await new Promise((resolve, reject) => {
    let text = "";
    chrome.stderr.on("data", b => { text += b; const match = text.match(/DevTools listening on (ws:\/\/[^\s]+)/); if (match) resolve(match[1]); });
    chrome.on("error", reject); chrome.on("exit", () => reject(new Error("Audio decoder exited")));
  });
  socket = new WebSocket(url);
  await new Promise(resolve => socket.addEventListener("open", resolve, { once: true }));
  let sequence = 0; const requests = new Map();
  socket.addEventListener("message", event => {
    const msg = JSON.parse(event.data), request = requests.get(msg.id);
    if (request) { requests.delete(msg.id); msg.error ? request.reject(new Error(JSON.stringify(msg.error))) : request.resolve(msg.result); }
  });
  function call(method, params = {}, sessionId) {
    return new Promise((resolve, reject) => { const id = ++sequence; requests.set(id, { resolve, reject }); socket.send(JSON.stringify({ id, method, params, sessionId })); });
  }
  const { targetId } = await call("Target.createTarget", { url: "about:blank" });
  const { sessionId } = await call("Target.attachToTarget", { targetId, flatten: true });
  for (const { input, dest } of pending) {
    const bytes = (await readFile(input)).toString("base64");
    const result = await call("Runtime.evaluate", { awaitPromise: true, returnByValue: true, expression: `(async () => {
      const encoded = Uint8Array.from(atob(${JSON.stringify(bytes)}), c => c.charCodeAt(0));
      const audio = await new OfflineAudioContext(1, 1, 44100).decodeAudioData(encoded.buffer);
      const channels = audio.numberOfChannels, count = audio.length * channels;
      const buffer = new ArrayBuffer(44 + count * 2), view = new DataView(buffer);
      const text = (at, s) => { for (let i=0; i<s.length; i++) view.setUint8(at+i, s.charCodeAt(i)); };
      text(0,'RIFF'); view.setUint32(4,36+count*2,true); text(8,'WAVE'); text(12,'fmt ');
      view.setUint32(16,16,true); view.setUint16(20,1,true); view.setUint16(22,channels,true);
      view.setUint32(24,audio.sampleRate,true); view.setUint32(28,audio.sampleRate*channels*2,true);
      view.setUint16(32,channels*2,true); view.setUint16(34,16,true); text(36,'data'); view.setUint32(40,count*2,true);
      for(let c=0;c<channels;c++) { const data=audio.getChannelData(c); for(let i=0;i<audio.length;i++)
        view.setInt16(44+(i*channels+c)*2,Math.round(Math.max(-1,Math.min(1,data[i]))*32767),true); }
      const raw=new Uint8Array(buffer); let binary='';
      for(let i=0;i<raw.length;i+=32768) binary+=String.fromCharCode(...raw.subarray(i,i+32768));
      return btoa(binary);
    })()` }, sessionId);
    if (result.exceptionDetails || !result.result?.value) throw new Error(`Audio decode failed: ${input}`);
    await writeFile(dest, Buffer.from(result.result.value, "base64"));
  }
  console.log(`Prepared ${pending.length} canonical No Paint audio cues.`);
} finally { clearTimeout(timeout); socket?.close(); chrome.kill(); }
