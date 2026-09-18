// A virtual filesystem with exactly three operations, because that is all the
// `ac` bridge performs: it reads four guides out of `easel/context`, reads the
// piece before a turn, and writes the piece after one.
//
// Everything lives in a Map keyed by URL pathname. Two things fill it:
// `preload()` fetches the guides from the server at boot, and the host (the
// page, and later Swift) mounts the piece. Writes go to `onWrite`, which is
// where persistence actually happens — localStorage in the browser, the app's
// Documents container on iOS. Keeping that a callback rather than a hard-coded
// backend is the seam the native shell will use without touching this file.

const files = new Map();

let onWrite = () => {};

export function mount(path, contents) {
  files.set(path, contents);
}

export function unmount(path) {
  files.delete(path);
}

export function setWriteHandler(fn) {
  onWrite = typeof fn === "function" ? fn : () => {};
}

export function snapshot() {
  return new Map(files);
}

// The guides are fetched under the same pathnames `bundledContext()` builds, so
// a miss here is a genuinely missing file rather than a path mismatch. It skips
// rather than throws for the same reason the real `bundledContext()` does: a
// guide that fails to load should cost the model context, not the session.
export async function preload(paths) {
  const missing = [];
  await Promise.all(
    paths.map(async (path) => {
      try {
        if (typeof globalThis.__aeselGuides?.[path] === "string") {
          files.set(path, globalThis.__aeselGuides[path]);
          return;
        }
        const response = await fetch(path);
        if (!response.ok) throw new Error(`HTTP ${response.status}`);
        files.set(path, await response.text());
      } catch (error) {
        missing.push(`${path} (${error.message})`);
      }
    }),
  );
  return missing;
}

export function existsSync(path) {
  return files.has(path);
}

export function readFileSync(path) {
  const contents = files.get(path);
  if (contents === undefined) {
    const error = new Error(`ENOENT: no such file or directory, open '${path}'`);
    error.code = "ENOENT";
    throw error;
  }
  return contents;
}

export function writeFileSync(path, contents) {
  files.set(path, contents);
  onWrite(path, contents);
}

// Imported by `ac-session.mjs`, never reached: the phone does not keep a
// ~/.ac-token to create, delete or watch.
export function mkdirSync() {}
export function unlinkSync(path) {
  files.delete(path);
}
export function watch() {
  return { close() {} };
}

export default { existsSync, readFileSync, writeFileSync, mkdirSync, unlinkSync, watch };

// Read-only structural tools can inspect only explicitly mounted virtual files.
export const realpathSync = path => path;
export function statSync(path) {
  const contents = readFileSync(path);
  return {isFile:()=>true,isDirectory:()=>false,isSymbolicLink:()=>false,size:new TextEncoder().encode(contents).length};
}
export const lstatSync = statSync;
export function readdirSync(path) {
  const prefix=path.replace(/\/$/, '')+'/';
  return [...files.keys()].filter(p=>p.startsWith(prefix)&&!p.slice(prefix.length).includes('/')).map(p=>p.slice(prefix.length));
}
export function renameSync(from, to) { const value=readFileSync(from); files.delete(from); writeFileSync(to,value); }
