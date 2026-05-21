#!/usr/bin/env node
// sticky.mjs — drop text into a macOS Stickies note: translucent,
// larger text, sized to fit the content closely, centered on screen.
//
// This is the canonical "sticky the X" workflow. When @jeffrey says
// "sticky the <thing>" on a macOS host, run this on that thing.
//
// Usage:
//   node toolchain/macos/sticky.mjs <file>          # note from a file
//   node toolchain/macos/sticky.mjs --text "hello"  # note from a string
//   echo "hello" | node toolchain/macos/sticky.mjs  # note from stdin
//
// Flags:
//   --bigger N    font-size bumps above the Stickies default (default 3)
//   --opaque      skip the translucency toggle (leave it solid)
//   --no-center   leave the window where Stickies dropped it
//
// macOS only. Needs Accessibility permission for the host terminal —
// System Events drives the ⌘N / ⌘V / font / translucency keystrokes.
// If the paste lands empty, the content is still on the clipboard
// (this script set it) — just ⌘V into the note by hand.

import { readFileSync } from "node:fs";
import { spawnSync } from "node:child_process";

if (process.platform !== "darwin") {
  console.error("✗ sticky.mjs is macOS-only (needs the Stickies app).");
  process.exit(1);
}

// ── args ──────────────────────────────────────────────────────────────
const argv = process.argv.slice(2);
let bumps = 3, opaque = false, center = true, text = null, file = null;
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a === "--bigger") bumps = Math.max(0, parseInt(argv[++i], 10) || 0);
  else if (a === "--opaque") opaque = true;
  else if (a === "--no-center") center = false;
  else if (a === "--text") text = argv[++i] ?? "";
  else if (!a.startsWith("--") && !file) file = a;
}

// ── content: --text, file arg, or stdin ───────────────────────────────
let content = text;
if (content == null && file) {
  try { content = readFileSync(file, "utf8"); }
  catch (e) { console.error(`✗ cannot read ${file}: ${e.message}`); process.exit(1); }
}
if (content == null && !process.stdin.isTTY) {
  content = readFileSync(0, "utf8");
}
if (content == null || content.trim() === "") {
  console.error("usage: node toolchain/macos/sticky.mjs <file> | --text \"…\" | (stdin)");
  process.exit(1);
}
content = content.replace(/\s+$/, "");

// ── size estimate — fit the text content closely ──────────────────────
// Stickies' default note text is ~14 pt; each ⌘+ bump adds ~2 pt.
// Width/height fall out of the rendered metrics; the AppleScript clamps
// the result to the screen.
const lines   = content.split("\n");
const maxLen  = lines.reduce((m, l) => Math.max(m, l.length), 0);
const fontPt  = 14 + 2 * bumps;
const charW   = fontPt * 0.58;   // proportional-font average advance
const lineH   = fontPt * 1.42;   // line box height
const desiredW = Math.round(maxLen * charW + 36);   // + L/R padding
const desiredH = Math.round(lines.length * lineH + 56); // + titlebar/pad

// ── set the clipboard (pbcopy handles unicode cleanly) ────────────────
const cp = spawnSync("pbcopy", { input: content });
if (cp.status !== 0) { console.error("✗ pbcopy failed"); process.exit(1); }

// ── drive Stickies via osascript ──────────────────────────────────────
const osa = `
on run argv
  set desiredW to (item 1 of argv) as integer
  set desiredH to (item 2 of argv) as integer
  set bumps to (item 3 of argv) as integer
  set doOpaque to ((item 4 of argv) is "1")
  set doCenter to ((item 5 of argv) is "1")

  tell application "Finder" to set sb to bounds of window of desktop
  set scrW to (item 3 of sb)
  set scrH to (item 4 of sb)

  set w to desiredW
  if w > scrW - 40 then set w to (scrW - 40)
  if w < 260 then set w to 260
  set h to desiredH
  if h > scrH - 60 then set h to (scrH - 60)
  if h < 160 then set h to 160

  tell application "Stickies" to activate
  delay 0.5
  tell application "System Events" to tell process "Stickies"
    keystroke "n" using command down
    delay 0.4
    keystroke "v" using command down
    delay 0.3
    keystroke "a" using command down
    delay 0.15
    repeat bumps times
      keystroke "+" using command down
      delay 0.12
    end repeat
    key code 123 -- collapse the selection (caret to start)
    if not doOpaque then keystroke "t" using {command down, option down}
    delay 0.2
    set size of window 1 to {w, h}
    if doCenter then
      set position of window 1 to {(scrW - w) / 2, (scrH - h) / 2}
    end if
  end tell
  return ("sticky " & w & "x" & h & " on " & scrW & "x" & scrH)
end run
`;

const r = spawnSync("osascript", [
  "-", String(desiredW), String(desiredH), String(bumps),
  opaque ? "1" : "0", center ? "1" : "0",
], { input: osa, encoding: "utf8" });

if (r.status !== 0) {
  console.error("✗ osascript failed:", (r.stderr || "").trim());
  console.error("  (the content is on your clipboard — ⌘V into a note by hand)");
  process.exit(1);
}
console.log(`✓ ${(r.stdout || "").trim()}`);
