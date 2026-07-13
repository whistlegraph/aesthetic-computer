#!/usr/bin/env node
// ableton-tiny.mjs — shrink Ableton Live to a tiny concern: small UI zoom + small window.
//
//   node ableton-tiny.mjs                 # every mapped host: 50% zoom, 560x380 window
//   node ableton-tiny.mjs blueberry        # just blueberry
//   node ableton-tiny.mjs neo --zoom 75 --size 720x480
//   node ableton-tiny.mjs blueberry --reset # back to 100% zoom, roomy 1200x800 window
//
// Live 12's UI scale is Settings > Display & Input > Zoom (50-200%, native). Like the theme
// sliders, it rejects `set value` but honours AXIncrement/AXDecrement in steps of 1, so this
// drives it over Accessibility. The window is then resized directly. Companion to
// ableton-theme.mjs (colour) — same requirements: Live must be RUNNING and the Mac UNLOCKED,
// and the zoom lives in Preferences.cfg, so an ableton-sync.sh run carries the source's zoom to
// the target; re-run this afterwards.
import { execFile } from "node:child_process";
import { promisify } from "node:util";
const run = promisify(execFile);

const HOSTS = ["neo", "blueberry"];
// 720x579 is Live's actual minimum window at 50% zoom — measured by asking for {1,1} and reading
// back what it clamped to. Anything smaller is silently ignored, so this IS "as small as possible".
// Zoom's own floor is 50%. Ask for less than either and you just get these numbers back.
const TINY = { zoom: 50, w: 720, h: 579 };
const ROOMY = { zoom: 100, w: 1200, h: 800 };

// Sets Zoom (Display page) then resizes the main window. args: zoom width height
const SCPT = `on run argv
  set targetZoom to (item 1 of argv) as number
  set winW to (item 2 of argv) as number
  set winH to (item 3 of argv) as number
  tell application "Ableton Live 12 Suite" to activate
  delay 1
  tell application "System Events"
    tell process "Live"
      my dismissModal()
      if not (exists window "Settings") then
        click menu item "Settings..." of menu 1 of menu bar item "Live" of menu bar 1
        delay 2
      end if
      tell window "Settings"
        click radio button 1 of radio group 1 of group 1
        delay 1
        my drive(group "Display, Language, and Input Settings" of group 1, "Zoom", targetZoom)
      end tell
      key code 53
      delay 0.5
      set out to "zoom=" & targetZoom & " "
      repeat with w in windows
        if name of w is not "Settings" then
          try
            set position of w to {60, 60}
            set size of w to {winW, winH}
            set out to out & "window=" & winW & "x" & winH
          on error e
            set out to out & "resize failed: " & e
          end try
        end if
      end repeat
      return out
    end tell
  end tell
end run

on drive(g, sliderName, target)
  tell application "System Events"
    set sl to missing value
    repeat with s in sliders of g
      if description of s is sliderName then set sl to s
    end repeat
    if sl is missing value then return
    set v to value of sl
    set guard to 0
    repeat while (v < target) and (guard < 400)
      perform action "AXIncrement" of sl
      set v to value of sl
      set guard to guard + 1
    end repeat
    repeat while (v > target) and (guard < 400)
      perform action "AXDecrement" of sl
      set v to value of sl
      set guard to guard + 1
    end repeat
  end tell
end drive

-- Live's global alerts ("Audio is disabled...") are app-modal and BLOCK every other AX call, so
-- anything that runs before them just hangs. They are not addressable as a normal dialog either
-- (\`button "OK" of window 1\` errors -1728) — the OK sits at button 1 of group 1 of window 1.
on dismissModal()
  tell application "System Events"
    tell process "Live"
      try
        if exists (button 1 of group 1 of window 1) then
          click button 1 of group 1 of window 1
          delay 0.5
        end if
      end try
    end tell
  end tell
end dismissModal
`;

const ssh = (host, cmd) => run("ssh", ["-o", "ConnectTimeout=8", host, cmd], { maxBuffer: 1 << 22 });

async function liveRunning(host) {
  const { stdout } = await ssh(
    host,
    `osascript -e 'tell application "System Events" to return (exists process "Live")'`,
  );
  return stdout.trim() === "true";
}

const argv = process.argv.slice(2);
const flag = (n) => argv.includes(n);
const valueOf = (n) => (argv.includes(n) ? argv[argv.indexOf(n) + 1] : null);
const positional = argv.filter(
  (a, i) => !a.startsWith("--") && !["--zoom", "--size"].includes(argv[i - 1]),
);

const base = flag("--reset") ? ROOMY : TINY;
const zoom = valueOf("--zoom") ? Number(valueOf("--zoom")) : base.zoom;
const [w, h] = valueOf("--size")
  ? valueOf("--size").split("x").map(Number)
  : [base.w, base.h];
const hosts = positional.length ? [positional[0]] : HOSTS;

const b64 = Buffer.from(SCPT).toString("base64");
for (const host of hosts) {
  console.log(`=== ${host} ===`);
  try {
    if (!(await liveRunning(host))) {
      console.log("  ✗ Live is not running (or the Mac is locked) — start it and re-run");
      continue;
    }
    const { stdout } = await ssh(
      host,
      `printf %s '${b64}' | base64 -d > /tmp/ac-ableton-tiny.scpt && ` +
        `osascript /tmp/ac-ableton-tiny.scpt ${zoom} ${w} ${h}`,
    );
    console.log(`  ✓ ${stdout.trim()}`);
  } catch (err) {
    console.log(`  ✗ ${err.message.split("\n")[0]}`);
  }
}
