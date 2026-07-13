#!/usr/bin/env node
// ableton-theme.mjs — tint each fleet Mac's Ableton Live to that machine's macOS accent colour.
//
//   node ableton-theme.mjs                  # every mapped host, matched to its own accent
//   node ableton-theme.mjs neo              # just neo
//   node ableton-theme.mjs --set-accent     # also write each Mac's fleet identity accent first
//   node ableton-theme.mjs blueberry 300    # one-off hue override (degrees, 0-360)
//   node ableton-theme.mjs neo --intensity 80
//
// THE macOS ACCENT COLOUR IS THE SOURCE OF TRUTH. Each host's Live is tinted to the hue of
// whatever `AppleAccentColor` that Mac is set to — change it in System Settings > Appearance,
// re-run this, and Live follows. Combined with cursor-color.sh (neo=green, blueberry=blue), a
// glance at a screen share tells you which Mac you are looking at.
//
// WHY IT DRIVES THE UI INSTEAD OF WRITING A FILE
// Live 12 dropped loadable custom themes: dropping a .ask into ~/Music/Ableton/User Library/
// Themes/ does nothing — the Theme menu lists only Live's seven built-ins, even after a restart
// (verified). What Live 12 gives you instead is Settings > Theme & Colors > Customization, with
// native Color Hue (0-360) and Color Intensity (0-100) sliders. Those are the real knobs, and
// the only way to reach them is Live's UI. So this drives them over Accessibility.
//
// Two consequences worth knowing:
//   * Live must be RUNNING and the Mac UNLOCKED. A locked Mac has no UI to drive.
//   * Those sliders live in Preferences.cfg — the very file ableton-sync.sh copies. A sync
//     therefore carries the SOURCE machine's hue to the target. Re-run this after any sync;
//     ableton-sync.sh prints a reminder to do exactly that.
import { execFile } from "node:child_process";
import { promisify } from "node:util";
const run = promisify(execFile);

// macOS accent colours, keyed by the AppleAccentColor value System Settings writes.
// `hex` is Apple's own accent swatch; its hue is what Live gets tinted to.
const ACCENTS = {
  "-1": { name: "graphite", hex: "#8e8e93", highlight: "0.847059 0.847059 0.862745 Graphite" },
  0: { name: "red", hex: "#ff3b30", highlight: "1.000000 0.733333 0.721569 Red" },
  1: { name: "orange", hex: "#ff9500", highlight: "1.000000 0.874510 0.701961 Orange" },
  2: { name: "yellow", hex: "#ffcc00", highlight: "1.000000 0.937255 0.690196 Yellow" },
  3: { name: "green", hex: "#28cd41", highlight: "0.752941 0.964706 0.678431 Green" },
  4: { name: "blue", hex: "#007aff", highlight: "0.698039 0.843137 1.000000 Blue" },
  5: { name: "purple", hex: "#af52de", highlight: "0.968627 0.831373 1.000000 Purple" },
  6: { name: "pink", hex: "#ff2d55", highlight: "1.000000 0.749020 0.823529 Pink" },
};
const DEFAULT_ACCENT = 4; // an unset AppleAccentColor is macOS "multicolour", which draws blue

// host -> fleet identity accent, written to macOS only with --set-accent. Mirrors cursor-color.sh.
const HOST_ACCENT = { neo: 3, blueberry: 4 };

const INTENSITY = 60; // how hard Live leans into the hue; enough to read across a room

// Graphite is a grey — it has no hue, so there is nothing meaningful to tint Live toward.
// Zeroing Color Intensity returns Live to its neutral greyscale, which is the honest answer.
const GREY = -1;

const hueOfHex = (hex) => {
  const [r, g, b] = [0, 2, 4].map((i) => parseInt(hex.slice(1 + i, 3 + i), 16) / 255);
  const max = Math.max(r, g, b), min = Math.min(r, g, b), d = max - min;
  if (!d) return null;
  const h = max === r ? ((g - b) / d + (g < b ? 6 : 0)) : max === g ? (b - r) / d + 2 : (r - g) / d + 4;
  return (h * 60 + 360) % 360;
};

// Live's sliders refuse `set value` but honour AXIncrement/AXDecrement in steps of 1.
const TINT_SCPT = `on run argv
  set targetHue to (item 1 of argv) as number
  set targetIntensity to (item 2 of argv) as number
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
        click radio button 2 of radio group 1 of group 1
        delay 1
        set g to group "Themes and Colors Settings" of group 1
        my drive(g, "Color Hue", targetHue)
        my drive(g, "Color Intensity", targetIntensity)
        delay 0.5
        set out to ""
        repeat with s in sliders of g
          if (description of s is "Color Hue") or (description of s is "Color Intensity") then
            set out to out & (description of s) & "=" & (value of s) & " "
          end if
        end repeat
        return out
      end tell
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

// Ask the window server, not pgrep: `pgrep -f "Ableton Live"` matches the very shell ssh spawns
// to run it (its own command line contains the pattern), so it always reports a false hit.
async function liveRunning(host) {
  const { stdout } = await ssh(
    host,
    `osascript -e 'tell application "System Events" to return (exists process "Live")'`,
  );
  return stdout.trim() === "true";
}

async function readAccent(host) {
  try {
    const { stdout } = await ssh(host, "defaults read -g AppleAccentColor");
    const n = parseInt(stdout.trim(), 10);
    return ACCENTS[n] ? n : DEFAULT_ACCENT;
  } catch {
    return DEFAULT_ACCENT; // key absent => multicolour
  }
}

async function setAccent(host, accent) {
  const { highlight, name } = ACCENTS[accent];
  await ssh(
    host,
    `defaults write -g AppleAccentColor -int ${accent}; ` +
      `defaults write -g AppleHighlightColor -string '${highlight}'`,
  );
  console.log(`  ✓ macOS accent set to ${name} (shows on next app launch / login)`);
}

async function apply(host, { hueOverride, intensity, setAccentFirst }) {
  if (setAccentFirst && HOST_ACCENT[host] != null) await setAccent(host, HOST_ACCENT[host]);

  const accent = await readAccent(host);
  const { name, hex } = ACCENTS[accent];
  const hue = hueOverride ?? hueOfHex(hex);
  console.log(`  system accent: ${name} ${hex}`);

  if (!(await liveRunning(host))) {
    console.log("  ✗ Live is not running (or the Mac is locked) — start it and re-run");
    return;
  }

  const [h, i] = hue == null ? [0, 0] : [Math.round(hue), intensity];
  if (hue == null) console.log("  graphite has no hue — clearing Live's tint back to greyscale");

  const b64 = Buffer.from(TINT_SCPT).toString("base64");
  const { stdout } = await ssh(
    host,
    `printf %s '${b64}' | base64 -d > /tmp/ac-ableton-tint.scpt && ` +
      `osascript /tmp/ac-ableton-tint.scpt ${h} ${i}`,
  );
  console.log(`  ✓ Live tinted — ${stdout.trim()}`);
}

const argv = process.argv.slice(2);
const flag = (n) => argv.includes(n);
const valueOf = (n) => (argv.includes(n) ? Number(argv[argv.indexOf(n) + 1]) : null);
const positional = argv.filter((a, i) => !a.startsWith("--") && argv[i - 1] !== "--intensity");

const hosts = positional.length ? [positional[0]] : Object.keys(HOST_ACCENT);
const hueOverride = positional.length > 1 ? Number(positional[1]) : null;
const intensity = valueOf("--intensity") ?? INTENSITY;

for (const host of hosts) {
  console.log(`=== ${host} ===`);
  try {
    await apply(host, { hueOverride, intensity, setAccentFirst: flag("--set-accent") });
  } catch (err) {
    console.log(`  ✗ ${err.message.split("\n")[0]}`);
  }
}
