import { validateGame, starterGame } from "./format.mjs";
import { sandboxDocument } from "./runtime.mjs";
const $ = (id) => document.getElementById(id),
  id = /\/oskie\/([a-f0-9]{32})\/?$/.exec(location.pathname)?.[1];
const token = new URLSearchParams(location.hash.slice(1)).get("preview");
// Never put the read-only preview grant into a request URL or a referrer.
let current,
  port,
  iframe,
  localURL,
  timer,
  capturing = false,
  loading = false,
  disposed = false,
  health = "waiting",
  generation = 0,
  local = false;
const headers = token ? { Authorization: "Preview " + token } : {};
const say = (text, error = false) => {
  $("status").textContent = text;
  health = error ? "error" : health;
};
const fail = (e) => {
  $("error").hidden = false;
  $("error").textContent = String(e.message || e);
};
async function report(data) {
  if (!id || !token || !current || local) return;
  await fetch(`/api/oskie/${id}/feedback`, {
    method: "POST",
    headers: { ...headers, "Content-Type": "application/json" },
    body: JSON.stringify({ ...data, revision: current.revision }),
    credentials: "omit",
  }).catch(() => {});
}
function stop() {
  generation++;
  disposed = true;
  capturing = false;
  clearInterval(timer);
  port?.close();
  port = null;
  iframe?.remove();
  iframe = null;
  health = "stopped";
  say("Stopped.");
}
function play(value) {
  stop();
  disposed = false;
  current = value;
  const game = validateGame(value.bundle);
  $("error").hidden = true;
  $("welcome").hidden = true;
  $("game").hidden = false;
  $("title").textContent = game.manifest.title;
  $("identity").textContent = game.manifest.author || "";
  document.title = game.manifest.title + " · Aesthetic.Computer";
  if (localURL) URL.revokeObjectURL(localURL);
  localURL = URL.createObjectURL(
    new Blob([JSON.stringify(game, null, 2)], {
      type: "application/vnd.aesthetic.oskie+json",
    }),
  );
  $("download").href = localURL;
  $("download").download = game.manifest.slug + ".oskie";
  $("download").hidden = false;
  iframe = document.createElement("iframe");
  iframe.title = game.manifest.title;
  iframe.setAttribute("sandbox", "allow-scripts");
  iframe.allow = "autoplay; gamepad";
  iframe.referrerPolicy = "no-referrer";
  const channel = new MessageChannel();
  port = channel.port1;
  const revision = value.revision,
    run = generation,
    frameElement = iframe;
  port.onmessage = async ({ data: d }) => {
    if (disposed || run !== generation || current.revision !== revision) return;
    if (d.type === "ready") {
      health = "running";
      say(
        (token ? "Private preview · " : "") +
          (value.revision
            ? "Revision " + value.revision.slice(0, 8)
            : "Local game"),
      );
      void report({ status: "running" });
    }
    if (d.type === "error") {
      health = "error";
      say(String(d.error).slice(0, 2000), true);
      void report({ status: "error", error: String(d.error).slice(0, 2000) });
    }
    if (
      d.type === "frame" &&
      d.bytes instanceof ArrayBuffer &&
      d.bytes.byteLength < 650000
    ) {
      const capturedRevision = revision;
      const reader = new FileReader();
      reader.onload = () => {
        capturing = false;
        if (run === generation && current.revision === capturedRevision)
          void report({
            status: health === "error" ? "error" : "running",
            frame: reader.result,
          });
      };
      reader.readAsDataURL(new Blob([d.bytes], { type: "image/png" }));
    }
  };
  iframe.addEventListener(
    "load",
    () =>
      frameElement.contentWindow?.postMessage(
        { type: "start", bundle: game },
        "*",
        [channel.port2],
      ),
    { once: true },
  );
  const nonce = crypto.randomUUID().replaceAll("-", "");
  iframe.srcdoc = sandboxDocument(nonce);
  $("stage").replaceChildren(iframe);
  say("Starting…");
  timer = setInterval(() => {
    if (health === "running" && port && !capturing) {
      capturing = true;
      port.postMessage({ type: "capture" });
      setTimeout(() => {
        capturing = false;
      }, 3000);
    }
  }, 5000);
}
async function poll() {
  if (!id || loading || local) return;
  loading = true;
  try {
    const r = await fetch("/api/oskie/" + id, {
      headers,
      cache: "no-store",
      credentials: "omit",
    });
    const value = await r.json();
    if (!r.ok) throw Error(value.error);
    if (!current || value.revision !== current.revision) play(value);
  } catch (e) {
    fail(e);
  } finally {
    loading = false;
  }
}
async function file(file) {
  if (!file) return;
  if (file.size > 3 * 1024 * 1024)
    throw Error("This player accepts .oskie files up to 2 MB of bundled data.");
  const bundle = validateGame(JSON.parse(await file.text()));
  local = true;
  play({ bundle, revision: "local-" + crypto.randomUUID() });
}
$("file").addEventListener("change", (e) =>
  file(e.target.files[0]).catch(fail),
);
$("example").onclick = () =>
  play({ bundle: starterGame(), revision: "example" });
$("restart").onclick = () => current && play(current);
$("stop").onclick = stop;
addEventListener("dragover", (e) => e.preventDefault());
addEventListener("drop", (e) => {
  e.preventDefault();
  file(e.dataTransfer.files[0]).catch(fail);
});
addEventListener("pagehide", () => {
  stop();
  if (localURL) URL.revokeObjectURL(localURL);
});
const dropped = sessionStorage.getItem("ac:oskie:drop");
if (dropped) {
  sessionStorage.removeItem("ac:oskie:drop");
  try {
    play({
      bundle: validateGame(JSON.parse(dropped)),
      revision: "local-" + crypto.randomUUID(),
    });
  } catch (e) {
    fail(e);
  }
}
if (id) {
  await poll();
  setInterval(poll, 2000);
}

$("open-file").onclick = () => $("file").click();
