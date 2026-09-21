import { execFile } from "node:child_process";
import { mkdir, writeFile } from "node:fs/promises";
import { dirname } from "node:path";
import { pathToFileURL } from "node:url";
import { promisify } from "node:util";

const exec = promisify(execFile);

// Keep custom CAPTUTOR_FRAME executables compatible. The house module is
// imported once and shares Frame's lease and transport without blocking CDP,
// pointer animation, or director messages behind a child process.
export function createFrameClient(path, { cli = false } = {}) {
  let module;
  return async function capture({ out, screen = false, clearOverlays = false } = {}) {
    let env;
    if (cli) {
      const args = [path, "local", "--no-ocr", "--quiet-overlay", "--json"];
      if (screen) args.push("--screen");
      if (clearOverlays) args.push("--clear-overlays");
      if (out) args.push("--out", out);
      const { stdout } = await exec(process.execPath, args, { timeout: 15000, maxBuffer: 4 * 1024 * 1024 });
      env = JSON.parse(stdout);
    } else {
      module ||= import(pathToFileURL(path).href);
      const { captureFrame } = await module;
      const result = await captureFrame("local", {
        memory: true, noOCR: true, noVisual: true, quietOverlay: true, screen, clearOverlays,
      });
      env = result.env;
      if (out && env.capture === "ok" && result.jpg?.length) {
        await mkdir(dirname(out), { recursive: true });
        await writeFile(out, result.jpg);
      } else if (out && env.capture === "ok") {
        throw new Error("Frame returned no pixels");
      }
    }
    if (out && env.capture !== "ok") throw new Error(`Frame capture failed: ${env.capture}`);
    if (clearOverlays && env.capture !== "action") throw new Error(`Frame overlay clear failed: ${env.capture}`);
    return env;
  };
}
