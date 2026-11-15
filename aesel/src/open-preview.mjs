// Previews open in whatever this machine already uses for that kind of file.
// Aesel does not pick an app for anyone: when nothing here opens the file, it
// says so and names players that would.
import { execFile } from "node:child_process";
import { copyFile, mkdir, access } from "node:fs/promises";
import { join, extname, dirname } from "node:path";

/** The operating system's own "open this" command for a file path or URL. */
export function previewCommand(target, platform = process.platform) {
  return [platform === "darwin" ? "open" : platform === "win32" ? "explorer" : "xdg-open", [target]];
}

// Players for the kinds of file an operating system does not open by itself.
export const PLAYERS = {
  ".nes": { darwin: ["OpenEmu", "Mesen", "Nestopia UE"], win32: ["Mesen", "Nestopia UE"], linux: ["Mesen", "FCEUX", "Nestopia UE"] },
  ".gb": { darwin: ["OpenEmu", "SameBoy", "mGBA"], win32: ["SameBoy", "mGBA", "BGB"], linux: ["SameBoy", "mGBA"] },
};
/** What to tell someone whose machine opens nothing for this file; '' when there is nothing to suggest. */
export function installHint(file, platform = process.platform) {
  const names = PLAYERS[extname(file).toLowerCase()]?.[platform];
  return names ? `Nothing on this computer opens ${extname(file)} files. ${names.join(", ")} can: install one, then /open again.` : "";
}

const run = (command, args) => new Promise((resolve) => {
  execFile(command, args, { timeout: 4000 }, (error, stdout) => resolve(error ? null : String(stdout).trim()));
});
/** The app this machine opens `file` with: its name, '' when there is none, null when that cannot be told. */
export async function defaultOpener(file, platform = process.platform) {
  if (platform === "darwin")
    return run("/usr/bin/osascript", ["-l", "JavaScript", "-e",
      'ObjC.import("AppKit"); function run(a){const u=$.NSWorkspace.sharedWorkspace.URLForApplicationToOpenURL($.NSURL.fileURLWithPath(a[0])); return u.isNil()?"":ObjC.unwrap(u.path)}', file]);
  if (platform === "linux") {
    const type = await run("xdg-mime", ["query", "filetype", file]);
    return type ? run("xdg-mime", ["query", "default", type]) : null;
  }
  return null;
}

// One steady path a viewer can keep open: each new version is copied over it,
// so an app that watches its file (Preview, most editors) shows the change.
export const openedPath = (root, artifact, file) => join(root, "open", `${artifact.kind}-${artifact.id.slice(0, 8)}${extname(file)}`);
export async function refreshOpened(root, artifact, file, { create = false } = {}) {
  const target = openedPath(root, artifact, file);
  if (!create) { try { await access(target); } catch { return null; } }
  await mkdir(dirname(target), { recursive: true });
  await copyFile(file, target);
  return target;
}
