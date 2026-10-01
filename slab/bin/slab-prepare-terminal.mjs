#!/usr/bin/env node
// Prepare the page before a TUI caches Terminal's foreground/background.
import { readFileSync } from "node:fs";
import { execFileSync } from "node:child_process";
import { homedir } from "node:os";

const tty = process.argv[2];
let dark = null;
try {
  if (process.env.TERM_PROGRAM !== "Apple_Terminal" || !/^ttys[0-9]+$/.test(tty)) throw 0;
  const root = process.env.SLAB_HOME || `${homedir()}/.local/share/slab`;
  const theme = JSON.parse(readFileSync(`${root}/state/theme.json`, "utf8"));
  if (!theme.enabled || typeof theme.dark !== "boolean") throw 0;
  const p = theme.palettes.blank;
  const properties = { background: "background color", foreground: "normal text color",
    bold: "bold text color", cursor: "cursor color" };
  const assignments = Object.entries(properties).map(([key, property]) => {
    const rgb = p[key];
    if (!Array.isArray(rgb) || rgb.length !== 3 || !rgb.every(n => Number.isInteger(n) && n >= 0 && n <= 255)) throw 0;
    return `set ${property} of t to {${rgb.map(n => n * 257).join(",")}}`;
  }).join("\n");
  const result = execFileSync("/usr/bin/osascript", ["-e", `
    if application "Terminal" is not running then return "missing"
    tell application "Terminal"
      repeat with w in windows
        repeat with t in tabs of w
          if tty of t is "/dev/${tty}" then
            ${assignments}
            return "ready"
          end if
        end repeat
      end repeat
    end tell
    return "missing"`], { encoding: "utf8", timeout: 3000, stdio: ["ignore", "pipe", "ignore"] });
  if (result.trim() === "ready") dark = theme.dark;
} catch { /* A stopped menubar, SSH or missing permission must not block launch. */ }
console.log(JSON.stringify(dark));
