// backends.mjs — the engine bridges Easel can drive.
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
import { AcServer, AC_MODELS, DEFAULT_AC_MODEL } from "./ac-server.mjs";
import { AppServer } from "./app-server.mjs";
import { ClaudeServer, DEFAULT_CLAUDE_MODEL } from "./claude-server.mjs";

export const BACKENDS = {
  claude: {
    id: "claude",
    label: "claude",
    // The executable that has to be on PATH for this bridge to start.
    command: "claude",
    defaultModel: DEFAULT_CLAUDE_MODEL,
    // Where a model comes from when the interface does not name one.
    modelSource: "the --model flag",
    Engine: ClaudeServer,
  },
  // The only bridge that needs nothing installed. It talks to
  // aesthetic.computer, which buys the inference and meters it against the
  // caller's @handle — so `command` is empty, because there is no binary to
  // find and a missing one is not why this bridge would fail.
  ac: {
    id: "ac",
    label: "aesthetic",
    command: "",
    defaultModel: DEFAULT_AC_MODEL,
    modelSource: "aesthetic.computer",
    hosted: true,
    models: AC_MODELS,
    Engine: AcServer,
  },
  codex: {
    id: "codex",
    label: "codex",
    command: "codex",
    // Empty: Codex reads its own configuration unless a name is given.
    defaultModel: "",
    modelSource: "~/.codex/config.toml",
    Engine: AppServer,
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
};

export const DEFAULT_BACKEND = "claude";

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
