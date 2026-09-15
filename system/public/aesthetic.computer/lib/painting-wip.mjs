import { encodePaintingState } from "./painting-state.mjs";
import { createNoPaintPiece, reconcileNoPaintPiece, sameNoPaintPixels } from "./nopaint-pieces.mjs";

const STORE_KEY = "painting:wip-editors";
const randomKey = () => Array.from(crypto.getRandomValues(new Uint8Array(24)), (n) => n.toString(16).padStart(2, "0")).join("");

export async function loadPaintingWipEditors(store) {
  if (!store[STORE_KEY]) store[STORE_KEY] = await store.retrieve(STORE_KEY, "local:db") || {};
}

export function paintingWipEditor(store, codeOrPiece) {
  const editors = store[STORE_KEY] || {};
  return editors[codeOrPiece] || Object.values(editors).find((entry) => entry.code === codeOrPiece);
}

export async function requestPaintingWip(api, body) {
  const token = await api.net.getToken();
  const response = await fetch("/api/painting-wip", {
    method: "POST",
    headers: { "Content-Type": "application/json", ...(token ? { Authorization: `Bearer ${token}` } : {}) },
    body: JSON.stringify(body),
  });
  let result;
  try { result = await response.json(); }
  catch { throw new Error("The painting server could not be reached"); }
  if (!response.ok) {
    const error = new Error(result.error || "Could not save this painting");
    error.status = response.status;
    throw error;
  }
  return result;
}

export async function readPaintingWip(api, code, state = true) {
  await loadPaintingWipEditors(api.store);
  return requestPaintingWip(api, { action: "read", code, state, key: paintingWipEditor(api.store, code)?.key });
}

export class PaintingWipSync {
  constructor(api, piece, onChange = () => {}, restored) {
    this.api = api;
    this.pieceId = piece.id;
    this.onChange = onChange;
    this.editor = restored || paintingWipEditor(api.store, piece.id) || {
      id: randomKey(), key: randomKey(), revision: 0, status: "wip", parent: piece.parent || null,
    };
    this.status = "saving";
    this.persist();
    this.create = () => this.editor.code ? Promise.resolve(this.editor) : requestPaintingWip(api, {
      action: "create", ...this.editor, width: piece.width, height: piece.height, initialLayers: 1,
    }).then((result) => { Object.assign(this.editor, result); this.persist(); return this.editor; });
    this.ready = this.create();
    // flush() reports creation errors in the same UI as snapshot errors.
    this.ready.catch(() => { this.ready = null; });
    this.queue(piece);
  }

  persist() {
    const store = this.api.store;
    store[STORE_KEY] = { ...(store[STORE_KEY] || {}), [this.pieceId]: { ...this.editor } };
    store.persist(STORE_KEY, "local:db");
    this.onChange(this);
  }

  queue(piece) {
    if (this.editor.status === "done") return;
    this.pending = piece;
    this.status = "saving";
    this.onChange(this);
    clearTimeout(this.timer);
    this.timer = setTimeout(() => this.flush().catch(() => {}), 600);
  }

  async flush(piece) {
    if (this.editor.status === "done") return this.editor;
    if (piece) this.pending = piece;
    clearTimeout(this.timer);
    if (this.running) {
      await this.running;
      return this.pending ? this.flush() : this.editor;
    }
    this.running = (async () => {
      try {
        await (this.ready ||= this.create());
        while (this.pending) {
          const snapshot = this.pending;
          this.pending = null;
          try {
            const state = await encodePaintingState(snapshot);
            const result = await requestPaintingWip(this.api, {
              action: "save", code: this.editor.code, key: this.editor.key,
              revision: this.editor.revision, state,
            });
            Object.assign(this.editor, result);
            this.persist();
          } catch (error) { this.pending ||= snapshot; throw error; }
        }
        this.status = "saved";
        this.error = null;
        return this.editor;
      } catch (error) {
        if (!this.editor.code) this.ready = null;
        this.status = "error";
        this.error = error.message;
        throw error;
      } finally { this.onChange(this); }
    })();
    try { return await this.running; }
    finally { this.running = null; }
  }

  sealed() {
    clearTimeout(this.timer);
    this.pending = null;
    this.editor.status = "done";
    this.status = "saved";
    this.persist();
  }

  reference() {
    return { code: this.editor.code, key: this.editor.key, revision: this.editor.revision };
  }
}

// Regular AC brushes share the same accepted canvas and recording as No Paint.
export async function syncACPaintingWip(api) {
  const system = api.system;
  if (!system.painting?.pixels) return null;
  const source = system.painting;
  const canvas = { width: source.width, height: source.height, pixels: new Uint8ClampedArray(source.pixels) };
  const record = system.nopaint.record.slice();
  const previous = system.nopaint.piece;
  await loadPaintingWipEditors(api.store);
  const editor = previous && paintingWipEditor(api.store, previous.id);
  let piece;
  if (editor?.status === "done") {
    if (sameNoPaintPixels(previous.composite, canvas)) return system.nopaint.wipSync || { editor };
    piece = createNoPaintPiece({ seed: randomKey(), ...previous.composite, role: "fork" });
    piece.parent = editor.code;
    piece = reconcileNoPaintPiece(piece, canvas, record, null, api.num.timestamp());
  } else {
    piece = reconcileNoPaintPiece(previous, canvas, record, randomKey(), api.num.timestamp());
  }
  let sync = system.nopaint.wipSync;
  if (source !== system.painting) {
    // A different painting opened while local storage was loading.
    if (sync?.pieceId === piece.id && sync.editor.status === "wip") sync.queue(piece);
    return null;
  }
  system.nopaint.piece = piece;
  api.store["painting:piece"] = piece;
  api.store.persist("painting:piece", "local:db");
  if (sync?.pieceId !== piece.id || sync.editor.status !== "wip") {
    sync = new PaintingWipSync(api, piece);
    system.nopaint.wipSync = sync;
  }
  sync.queue(piece);
  return sync;
}
