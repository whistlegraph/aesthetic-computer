// Render the same AC object pixels as the browser, at exact turntable angles.
// The deployed oven keeps public runtime modules in ac-source/.
import { existsSync } from "node:fs";
import { setImmediate } from "node:timers/promises";
import sharp from "sharp";
const deployedViewer = new URL("./ac-source/lib/object/viewer.mjs", import.meta.url);
const { objectPiece, isObjectLisp, OBJECT_PREVIEW_FPS, OBJECT_TURN_SECONDS } = await import(existsSync(deployedViewer)
  ? deployedViewer.href
  : new URL("../system/public/aesthetic.computer/lib/object/viewer.mjs", import.meta.url).href);

export async function captureObjectFrames(piece, options = {}) {
  if (!/^\$[A-Za-z0-9]{3,64}$/.test(piece)) throw new Error("Object previews require a saved $code");
  const { width = 800, height = 420, duration = OBJECT_TURN_SECONDS * 1000, fps = OBJECT_PREVIEW_FPS } = options;
  const count = options.frames ?? Math.ceil(duration / 1000 * fps);
  if (![width, height, count].every(Number.isInteger) || width < 32 || height < 32 ||
      width > 1920 || height > 1080 || count < 1 || count > 120) throw new Error("Invalid object capture dimensions or frame count");
  const response = await fetch("https://aesthetic.computer/api/store-kidlisp?code=" + piece.slice(1),
    { signal: AbortSignal.timeout(15000) });
  if (!response.ok) throw new Error(`Object source HTTP ${response.status}`);
  const { source } = await response.json();
  if (!isObjectLisp(source)) throw new Error("This piece is not a 3D object");
  // A fixed pose makes the turn seamless even when the original piece dances.
  const view = { preview: true, time: 0, yaw: 0.6 };
  const object = objectPiece(source, "", view);
  const screen = { width, height, pixels: new Uint8ClampedArray(width * height * 4) };
  const api = { screen, ink() { return api; }, write() { return api; } };
  object.boot(api);
  const frames = [];
  for (let i = 0; i < count; i++) {
    view.yaw = 0.6 + i / count * Math.PI * 2;
    object.paint(api);
    frames.push(await sharp(screen.pixels, { raw: { width, height, channels: 4 } }).png().toBuffer());
    await setImmediate(); // Keep the oven responsive between frames.
  }
  return frames;
}

export { OBJECT_PREVIEW_FPS, OBJECT_TURN_SECONDS };
