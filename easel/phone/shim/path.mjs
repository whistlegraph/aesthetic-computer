// POSIX `join` and `dirname` over the three call sites in the `ac` bridge.
//
// The paths this sees are not really filesystem paths — they are the URL
// pathnames the import map hands back, so `/easel/src/ac-server.mjs` rather
// than `/Users/jas/…`. That is deliberate: it makes the virtual filesystem's
// keys the same strings the page can fetch, so the guides load from the server
// under exactly the path `bundledContext()` asks for.

export function join(...parts) {
  const joined = parts.filter((part) => part !== "" && part != null).join("/");
  return normalize(joined);
}

export function normalize(path) {
  const absolute = path.startsWith("/");
  const out = [];
  for (const segment of path.split("/")) {
    if (segment === "" || segment === ".") continue;
    if (segment === "..") {
      // A leading `..` on a relative path has to survive; on an absolute one it
      // is meaningless and Node drops it too.
      if (out.length && out.at(-1) !== "..") out.pop();
      else if (!absolute) out.push("..");
      continue;
    }
    out.push(segment);
  }
  const body = out.join("/");
  if (absolute) return `/${body}`;
  return body || ".";
}

export function dirname(path) {
  const at = path.lastIndexOf("/");
  if (at === -1) return ".";
  if (at === 0) return "/";
  return path.slice(0, at);
}

export function basename(path) {
  return path.slice(path.lastIndexOf("/") + 1);
}

export function extname(path) {
  const name = basename(path);
  const at = name.lastIndexOf(".");
  return at <= 0 ? "" : name.slice(at);
}

export function resolve(...parts) {
  let out = "";
  for (const part of parts) {
    if (!part) continue;
    out = part.startsWith("/") ? part : out ? `${out}/${part}` : part;
  }
  return normalize(out.startsWith("/") ? out : `/${out}`);
}

export default { join, normalize, dirname, basename, extname, resolve };
