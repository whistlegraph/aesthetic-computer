// A classic script, loaded before any module, because `process` is referenced
// at module scope in `ac-server.mjs` (`process.env.EASEL_SITE`) and an import
// map cannot supply a bare global.
//
// Deliberately tiny and deliberately not `process`-shaped beyond what is read:
// an `env` to look in, a `cwd()` that answers with the piece root, and a
// `platform` that says what this really is. Anything else should fail loudly
// rather than quietly pretend to be Node.
globalThis.process = globalThis.process || {
  env: {},
  argv: ["aesel", "phone"],
  platform: "webview",
  version: "",
  cwd: () => "/piece",
};

// `publish.mjs` measures the upload with `Buffer.byteLength`. That is the only
// Buffer call anywhere in the shared path, and TextEncoder already answers it
// correctly for UTF-8 — which is the encoding every piece is stored in.
globalThis.Buffer = globalThis.Buffer || {
  byteLength: (value) => new TextEncoder().encode(String(value)).length,
};
