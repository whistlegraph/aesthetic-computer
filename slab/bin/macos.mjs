// macos.mjs — OS-level UI automation for puppet: drive Terminal tabs and the
// frontmost app on any registered machine (here, or over ssh). This is what
// makes puppet the default for macOS automation, not just browser/CDP control.
//
// Why it sits beside puppet's existing `key`/`stroke` (which inject into a
// browser *page* via an open DevTools socket): this layer needs nothing but an
// ssh hop + `osascript`, so it reaches machines with no browser at all — e.g.
// poking a `claude`/`codex` TUI that's running in a plain Terminal tab.
//
// The two hard-won bits, lifted out of fuser dev-tui's ptyInject so they stop
// being siloed in a private repo:
//
//   • A submit is TWO events, not one: paste the body (bracketed paste, so
//     embedded newlines stay CONTENT), then send Return as a SEPARATE keystroke
//     after a short delay. Bundling them makes Claude Code's TUI consume the
//     Return as paste-finalisation and strand the text as an unsent
//     "[Pasted text]". 50ms is the floor that's reliable; we use 60 for ssh.
//
//   • Terminal's `do script <text> in <tab>` is the no-focus path, but it
//     APPENDS to the input box and ALWAYS submits — perfect for a one-line
//     poke (the "..." nudge), useless for multi-line. Paste-mode is the
//     multi-line-safe path, at the cost of having to bring the tab frontmost.
//
// Transport comes from the machine's (untracked) puppet.json spec:
//   { "local": true }                       → run on this host
//   { "ssh": "jas@host -i ~/.ssh/id_ed25519" } → ssh hop (flags allowed)

import { execFileSync } from "node:child_process";

// ptyInject uses 50ms; 60 leaves headroom for ssh round-trip jitter.
const SUBMIT_DELAY_MS = 60;
const MAX_BUFFER = 16 * 1024 * 1024;

// Run a shell command on the machine (optional stdin); returns trimmed stdout.
// Local runs through `bash -lc`; remote prefixes the same with `ssh <dest>` so
// quoting behaves identically either way.
export function sh(spec, command, { stdin } = {}) {
  if (!spec) throw new Error("no machine spec");
  const opts = { input: stdin, encoding: "utf8", maxBuffer: MAX_BUFFER };
  if (spec.local) return execFileSync("bash", ["-lc", command], opts).trim();
  if (spec.ssh) {
    const q = `'${command.replace(/'/g, `'\\''`)}'`;
    const argv = [...spec.ssh.split(/\s+/), `bash -lc ${q}`];
    return execFileSync("ssh", argv, opts).trim();
  }
  if (spec.sshHost) {
    const q = `'${command.replace(/'/g, `'\\''`)}'`;
    return execFileSync("ssh", [spec.sshHost, `bash -lc ${q}`], opts).trim();
  }
  throw new Error("machine spec needs `local: true` or `ssh: \"...\"`");
}

// Click a whole-screen point in macOS's top-left coordinate space. This is the
// native counterpart to puppet's CDP-only stroke: frame OCR/AX coordinates can
// be used directly, including for apps with no browser DOM.
export function clickPoint(spec, x, y, { count = 1 } = {}) {
  if (!Number.isFinite(x) || !Number.isFinite(y)) throw new Error("click coordinates must be numbers");
  const n = Math.max(1, Math.min(3, Math.round(count)));
  return jxa(spec, `ObjC.import("CoreGraphics");
const p = $.CGPointMake(${Math.round(x)}, ${Math.round(y)});
for (let i = 0; i < ${n}; i++) {
  $.CGEventPost($.kCGHIDEventTap, $.CGEventCreateMouseEvent(null, $.kCGEventLeftMouseDown, p, $.kCGMouseButtonLeft));
  delay(0.04);
  $.CGEventPost($.kCGHIDEventTap, $.CGEventCreateMouseEvent(null, $.kCGEventLeftMouseUp, p, $.kCGMouseButtonLeft));
  if (i + 1 < ${n}) delay(0.08);
}`);
}

// Run AppleScript by piping it to `osascript -` (reads the script from stdin),
// which sidesteps every layer of shell-quoting the script body would face.
export function osa(spec, script) {
  return sh(spec, "osascript -", { stdin: script });
}

export function jxa(spec, script) {
  return sh(spec, "osascript -l JavaScript -", { stdin: script });
}

// Post a genuine no-click mouse move. A simple cursor warp is ignored from
// some SSH sessions; the HID event reliably activates contextual hover UI.
export function hoverPoint(spec, x, y) {
  if (!Number.isFinite(x) || !Number.isFinite(y)) throw new Error("hover coordinates must be numbers");
  return jxa(spec, `ObjC.import("CoreGraphics");
const p = $.CGPointMake(${Math.round(x)}, ${Math.round(y)});
const e = $.CGEventCreateMouseEvent(null, $.kCGEventMouseMoved, p, $.kCGMouseButtonLeft);
$.CGEventPost($.kCGHIDEventTap, e);`);
}

// AppleScript string literal: quote it, escape backslash + quote.
function aslit(s) {
  return '"' + String(s).replace(/\\/g, "\\\\").replace(/"/g, '\\"') + '"';
}

// AppleScript fragment: run `body` against the Terminal tab whose tty matches.
// `body` is AppleScript with `t` bound to the tab and `w` to its window.
function inTtyTab(tty, body) {
  return `tell application "Terminal"
    repeat with w in windows
      repeat with t in tabs of w
        try
          if (tty of t) is ${aslit(tty)} then
            ${body}
            return "ok"
          end if
        end try
      end repeat
    end repeat
    return "tty-not-found: ${tty}"
  end tell`;
}

// List Terminal tabs: tty, busy flag, and the processes running in each — the
// discovery step ("which tab is which agent?") made repeatable.
//
// Note: inside `tell application "Terminal"`, `tab` resolves to Terminal's tab
// CLASS, not the AppleScript tab character — so fields are space-delimited and
// the process list is joined with explicit text-item-delimiters.
export function termList(spec) {
  return osa(
    spec,
    `with timeout of 20 seconds
  set LF to linefeed
  set out to ""
  tell application "Terminal"
    repeat with w in windows
      repeat with t in tabs of w
        try
          set theTty to tty of t
          set theBusy to busy of t
          set AppleScript's text item delimiters to ","
          set procs to (processes of t) as text
          set AppleScript's text item delimiters to ""
          set out to out & theTty & "  busy=" & theBusy & "  [" & procs & "]" & LF
        end try
      end repeat
    end repeat
  end tell
  if out is "" then set out to "(no terminal tabs)"
  return out
end timeout`,
  );
}

// AppleScript fragment: bring the Terminal tab with this tty frontmost so
// System Events keystrokes land in it.
function focusTtyTab(tty) {
  return `tell application "Terminal"
    repeat with w in windows
      repeat with t in tabs of w
        try
          if (tty of t) is ${aslit(tty)} then
            set frontmost of w to true
            set selected tab of w to t
          end if
        end try
      end repeat
    end repeat
  end tell
  delay 0.1`;
}

// Inject text into a Terminal tab (by tty) or the frontmost app.
//   tty   — target this Terminal tab; omit to hit the frontmost app
//   paste — clipboard + Cmd+V (multi-line safe; brings the tab frontmost)
//   clear — wipe the input box first (Ctrl-U) so the text doesn't append to a
//           stale buffer; forces the focus path even for single-line pokes
//   enter — send Return as a separate delayed keystroke (the submit)
export function typeText(spec, text, { tty, paste, enter, clear } = {}) {
  // Fast path: one-line poke into a tab without stealing focus. `do script`
  // appends + submits — only valid when we're not clearing or pasting.
  if (tty && !paste && !clear) {
    return osa(
      spec,
      `with timeout of 20 seconds
  ${inTtyTab(tty, `do script ${aslit(text)} in t`)}
end timeout`,
    );
  }

  // Focus path. Paste loads the clipboard on the machine itself so the text
  // never has to survive AppleScript escaping; otherwise we type it literally.
  if (paste) sh(spec, "pbcopy", { stdin: text });
  const focus = tty ? focusTtyTab(tty) : "";
  // Ctrl-U clears the line in Claude/codex's input (and most line editors),
  // so a fresh message replaces whatever was sitting in the box.
  const wipe = clear
    ? `tell application "System Events" to keystroke "u" using control down
  delay 0.05`
    : "";
  const emit = paste
    ? `tell application "System Events" to keystroke "v" using command down`
    : `tell application "System Events" to keystroke ${aslit(text)}`;
  const submit = enter
    ? `delay ${SUBMIT_DELAY_MS / 1000}
  tell application "System Events" to key code 36` // 36 = Return
    : "";
  return osa(
    spec,
    `with timeout of 30 seconds
  ${focus}
  ${wipe}
  ${emit}
  ${submit}
  return "ok"
end timeout`,
  );
}

// Special keys addressable by macOS key code (everything else is a keystroke).
const KEY_CODES = {
  enter: 36, return: 36, tab: 48, space: 49, escape: 53, esc: 53,
  delete: 51, up: 126, down: 125, left: 123, right: 124,
};
const MOD_NAMES = {
  cmd: "command down", command: "command down",
  opt: "option down", option: "option down", alt: "option down",
  ctrl: "control down", control: "control down",
  shift: "shift down",
};

// Send a single key / chord to the frontmost app: `enter`, `escape`, a letter,
// etc., with optional modifiers (e.g. mods=["cmd","shift"]).
export function sendKeys(spec, key, mods = []) {
  const using = mods
    .map(m => MOD_NAMES[m.toLowerCase()])
    .filter(Boolean)
    .join(", ");
  const usingClause = using ? ` using {${using}}` : "";
  const code = KEY_CODES[String(key).toLowerCase()];
  const action =
    code !== undefined
      ? `key code ${code}${usingClause}`
      : `keystroke ${aslit(key)}${usingClause}`;
  return osa(
    spec,
    `with timeout of 15 seconds
  tell application "System Events" to ${action}
  return "ok"
end timeout`,
  );
}
