// Aesel watching its own source, for `/reify watch`: save a file under src/
// and the window reifies onto it — same thread, draft and preview. The
// build outputs are skipped, or writing a bundle would reify again.
import { watch } from "node:fs";

const SOURCE = /\.(mjs|json|txt)$/;
const BUILT = /(^|\/)\.tui-(built|bun)/;

export const isSourceChange = (name) => !!name && SOURCE.test(name) && !BUILT.test(name);

// Calls onChange with the changed paths once saves settle (an editor writes a
// file in several steps). Returns a function that stops watching.
export function watchSource(dir, onChange, { settleMs = 300 } = {}) {
  let timer = null, changed = new Set(), watcher;
  try {
    watcher = watch(dir, { recursive: true }, (event, name) => {
      if (!isSourceChange(String(name || ""))) return;
      changed.add(String(name));
      clearTimeout(timer);
      timer = setTimeout(() => { const files = [...changed]; changed = new Set(); onChange(files); }, settleMs);
    });
  } catch { return null; }
  watcher.on("error", () => {});
  return () => { clearTimeout(timer); watcher.close(); };
}
