// backends.mjs — the engine bridges aesel can drive.
//
// A bridge is a subprocess speaking a line protocol over stdio. The interface
// holds the same conversation over either of them — a thread, turns inside it,
// streamed text, tool activity, and approvals answered in this terminal — so a
// bridge is chosen at launch with `--backend`, or swapped mid-session with
// `/backend`, and nothing else in the interface changes.
//
// Claude is the default. Codex was the first and still works exactly as it
// did; the two differ mainly in where the model name comes from, which is why
// each one carries its own answer for `/model`.
import {AC_MODELS,DEFAULT_CLAUDE_MODEL} from './provider-defaults.mjs';
import {DEFAULT_OPEN_MODEL,OPEN_MODELS} from './open-models.mjs';
import {deferredEngine} from './deferred-engine.mjs';

export const BACKENDS = {
  claude: {
    id: "claude",
    label: "claude",
    // The executable that has to be on PATH for this bridge to start.
    command: "claude",
    defaultModel: DEFAULT_CLAUDE_MODEL,
    // Where a model comes from when the interface does not name one.
    modelSource: "the --model flag",
    Engine: deferredEngine(async()=> (await import('./claude-server.mjs')).ClaudeServer),
  },
  // The only bridge that needs nothing installed. It talks to
  // aesthetic.computer, which buys the inference and meters it against the
  // caller's @handle in braincells — so `command` is empty, because there is
  // no binary to find and a missing one is not why this bridge would fail. It
  // offers the same open models as `open`.
  ac: {
    id: "ac",
    label: "aesthetic",
    command: "",
    // Empty is Automatic: whatever the relay runs by default.
    defaultModel: "",
    modelSource: "aesthetic.computer",
    hosted: true,
    models: AC_MODELS,
    Engine: deferredEngine(async()=> (await import('./open-server.mjs')).HostedServer),
  },
  codex: {
    id: "codex",
    label: "codex",
    command: "codex",
    // Empty: Codex reads its own configuration unless a name is given.
    defaultModel: "",
    modelSource: "~/.codex/config.toml",
    Engine: deferredEngine(async()=> (await import('./app-server.mjs')).AppServer),
  },
  // Aesel's own loop on an open-weight model through OpenRouter, paid by the
  // person's own key. Nothing to install, like `ac`, but no handle budget.
  open: {
    id: "open",
    label: "open",
    command: "",
    defaultModel: DEFAULT_OPEN_MODEL,
    modelSource: "OpenRouter",
    models: OPEN_MODELS,
    Engine: deferredEngine(async()=> (await import('./open-server.mjs')).OpenServer),
  },
};

// The names people reach for for the same two bridges.
export const BACKEND_ALIASES = {
  aesthetic: "ac",
  hosted: "ac",
  free: "ac",
  anthropic: "claude",
  fable: "claude",
  openai: "codex",
  gpt: "codex",
  openrouter: "open",
};

// A first launch with nothing remembered: the hosted provider, which needs
// no vendor CLI or subscription — only an @handle.
export const DEFAULT_BACKEND = "ac";

// A model for the hosted provider: one of the open models, by name or id, or
// "" (Automatic) for anything else — a remembered older model included.
export function hostedModel(model) {
  const id = OPEN_MODELS[model] || model;
  return Object.values(OPEN_MODELS).includes(id) ? id : "";
}

export function backendIds() {
  return Object.keys(BACKENDS);
}

export function backendMenu() {
  return backendIds().join(", ");
}

export function backendFor(id) {
  const wanted = String(id || "").trim().toLowerCase();
  const backend = BACKENDS[BACKEND_ALIASES[wanted] || wanted];
  if (!backend) {
    throw new Error(`unknown backend "${id}" — try ${backendMenu()}`);
  }
  return backend;
}
