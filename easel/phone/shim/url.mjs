// `fileURLToPath` over http(s) URLs.
//
// `ac-server.mjs` calls it on `import.meta.url` to find its own directory. In a
// webview that URL is `https://host/easel/src/ac-server.mjs`, so the useful
// answer is the pathname — which keeps every derived path fetchable, the same
// convention the fs shim relies on.

export function fileURLToPath(url) {
  const text = String(url);
  if (text.startsWith("file://")) return new URL(text).pathname;
  try {
    return new URL(text).pathname;
  } catch {
    return text;
  }
}

export function pathToFileURL(path) {
  return new URL(path, globalThis.location?.href ?? "file:///");
}

export default { fileURLToPath, pathToFileURL };
