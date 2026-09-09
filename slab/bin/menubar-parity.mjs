#!/usr/bin/env node
// menubar-parity — keep the menu bar identical across the fleet's Macs.
//
// One reference Mac (the one you arranged by hand) is captured into
// slab/fleet/menubar-parity.json: the ⌘-drag positions of every status item
// (Menu Band, Slab, the Aesthetic Computer tray, Control Center's system
// items) plus Slab's toggle flags (auto-tile, theme-by-status, …). Any other
// Mac — including a brand-new one — is then brought to that layout with
// `apply`, and `audit` shows in one table whether the fleet's Menu Band,
// SlabMenubar and Aesthetic.Computer.app builds actually match (by Mach-O
// UUID, not just the version string). `deploy` ships this Mac's installed
// bundles to the others so the bytes match too.
//
//   node slab/bin/menubar-parity.mjs audit   [host…]        # local + hosts
//   node slab/bin/menubar-parity.mjs capture [--from host]  # write the canon
//   node slab/bin/menubar-parity.mjs apply   [host…]        # layout + flags
//   node slab/bin/menubar-parity.mjs deploy  host…          # bundles → hosts
//
// Hosts are ssh aliases; "local" (or no host) means this Mac. Dependency-free.
import { execFileSync, spawnSync } from "node:child_process";
import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { hostname } from "node:os";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = join(HERE, "..", "..");
const CANON = join(REPO, "slab", "fleet", "menubar-parity.json");

// Status-item owners. `defaults` domain → which keys are layout.
const DOMAINS = {
  "computer.aestheticcomputer.menuband": "Menu Band",
  "computer.slab.menubar": "Slab",
  "computer.aesthetic.app": "Aesthetic Computer",
  "com.apple.controlcenter": "Control Center",
};
const KEY_RE = /^NSStatusItem (Preferred Position|VisibleCC) /;

// Slab toggle flags (see Paths.swift) — presence of the file is the state.
const SLAB_FLAGS = [
  "auto-tile", "tile-near", "tile-tiny", "theme-by-status", "force-bright",
  "prefer-iterm", "muted", "resource-graph", "prompt-sigils-off", "zoom-lens-off",
];

const APPS = [
  { id: "menuband", label: "Menu Band", app: "$HOME/Applications/Menu Band.app", bin: "Contents/MacOS/MenuBand", agent: "computer.aestheticcomputer.menuband" },
  { id: "slab", label: "SlabMenubar", app: "$HOME/Applications/SlabMenubar.app", bin: "Contents/MacOS/slab-menubar", agent: "computer.slab.menubar" },
  { id: "acapp", label: "Aesthetic.Computer", app: "/Applications/Aesthetic.Computer.app", bin: "Contents/MacOS/Aesthetic.Computer", agent: "" },
];

// ---------------------------------------------------------------- shell

function sh(host, script, { check = true } = {}) {
  const local = !host || host === "local";
  const r = local
    ? spawnSync("bash", ["-s"], { input: script, encoding: "utf8", maxBuffer: 1 << 26 })
    : spawnSync("ssh", ["-o", "BatchMode=yes", "-o", "ConnectTimeout=8", host, "bash", "-s"], { input: script, encoding: "utf8", maxBuffer: 1 << 26 });
  if (check && r.status !== 0) throw new Error(`[${host || "local"}] ${r.stderr || r.stdout}`);
  return r.stdout;
}

// ---------------------------------------------------------------- probe

const PROBE = `
set -u
echo "host	$(hostname -s)"
echo "uid	$(id -u)"
echo "home	$HOME"
if [ -d "$HOME/aesthetic-computer/.git" ]; then echo "repo	$(git -C "$HOME/aesthetic-computer" rev-parse --short HEAD 2>/dev/null || echo -)"; else echo "repo	-"; fi
probe_app() { # id app bin agent
  local id="$1" app="$2" bin="$3" agent="$4" p ver build uuid signer pid state
  p="$app/Contents/Info.plist"
  if [ ! -d "$app" ]; then echo "app	$id	missing"; return; fi
  ver="$(/usr/libexec/PlistBuddy -c 'Print CFBundleShortVersionString' "$p" 2>/dev/null || echo -)"
  build="$(/usr/libexec/PlistBuddy -c 'Print CFBundleVersion' "$p" 2>/dev/null || echo -)"
  uuid="$(dwarfdump --uuid "$app/$bin" 2>/dev/null | awk '{print $2; exit}')"
  signer="$(codesign -dvv "$app" 2>&1 | awk -F= '/^Authority=/{print $2; exit}')"
  pid="$(pgrep -x "$(basename "$bin")" | head -1)"
  if [ -n "$agent" ]; then
    state="$(launchctl print "gui/$(id -u)/$agent" 2>/dev/null | awk '/state = /{print $3; exit}')"; [ -z "$state" ] && state="not-loaded"
  else state="-"; fi
  echo "app	$id	$ver	$build	\${uuid:--}	\${signer:-unsigned}	\${pid:--}	$state"
}
${APPS.map((a) => `probe_app ${a.id} "${a.app}" "${a.bin}" "${a.agent}"`).join("\n")}
for d in ${Object.keys(DOMAINS).join(" ")}; do
  # Old-style plist lines: "NSStatusItem Preferred Position menuband" = 317;
  # (defaults→JSON via plutil dies on <data>/<date> values, so parse only ours)
  defaults read "$d" 2>/dev/null | grep -E '^ *"?(NSStatusItem |statBlocksVisible)' | sed -E 's/^ *"?([^"=]+)"? *= *([-0-9.]+);.*$/defaults\t'"$d"'\t\\1\t\\2/'
done
flags=""; for f in ${SLAB_FLAGS.join(" ")}; do [ -e "$HOME/.local/share/slab/state/$f" ] && flags="$flags $f"; done
echo "flags	\${flags# }"
`;

function probe(host) {
  const out = sh(host, PROBE);
  const r = { host: host || "local", apps: {}, defaults: {}, flags: [] };
  for (const line of out.split("\n")) {
    const [k, ...rest] = line.split("\t");
    if (k === "host") r.name = rest[0];
    else if (k === "uid") r.uid = rest[0];
    else if (k === "home") r.home = rest[0];
    else if (k === "repo") r.repo = rest[0];
    else if (k === "app") {
      const [id, ver, build, uuid, signer, pid, state] = rest;
      r.apps[id] = ver === "missing" ? null : { ver, build, uuid, signer, pid, state };
    } else if (k === "defaults") {
      const [domain, rawKey, val] = rest; const key = (rawKey || "").trim();
      if (!key) continue;
      if (KEY_RE.test(key) || key === "statBlocksVisible") (r.defaults[domain] ??= {})[key] = Number(val);
    } else if (k === "flags") r.flags = rest[0] ? rest[0].split(" ") : [];
  }
  return r;
}

// ---------------------------------------------------------------- canon

function loadCanon() {
  if (!existsSync(CANON)) return null;
  return JSON.parse(readFileSync(CANON, "utf8"));
}

function capture(from) {
  const p = probe(from);
  const canon = {
    capturedFrom: p.name,
    capturedAt: new Date().toISOString(),
    statusItems: p.defaults,
    slabFlags: Object.fromEntries(SLAB_FLAGS.map((f) => [f, p.flags.includes(f)])),
  };
  writeFileSync(CANON, JSON.stringify(canon, null, 2) + "\n");
  console.log(`captured ${p.name} → ${CANON}`);
  for (const [d, keys] of Object.entries(canon.statusItems)) console.log(`  ${DOMAINS[d]}: ${Object.keys(keys).length} keys`);
  console.log(`  flags on: ${Object.entries(canon.slabFlags).filter(([, v]) => v).map(([k]) => k).join(" ") || "(none)"}`);
}

// Differences between a probe and the canon: [what, canon value, host value].
function diff(canon, p) {
  const out = [];
  for (const [d, keys] of Object.entries(canon.statusItems)) {
    const have = p.defaults[d] || {};
    for (const [k, v] of Object.entries(keys)) if (String(have[k]) !== String(v)) out.push([`${DOMAINS[d]} · ${k.replace("NSStatusItem ", "")}`, v, have[k] ?? "(unset)"]);
  }
  for (const [f, on] of Object.entries(canon.slabFlags)) {
    const has = p.flags.includes(f);
    if (has !== on) out.push([`slab flag ${f}`, on ? "on" : "off", has ? "on" : "off"]);
  }
  return out;
}

// ---------------------------------------------------------------- apply

function applyScript(canon) {
  const lines = ["set -u", 'UID_="$(id -u)"', "S=\"$HOME/.local/share/slab/state\"; mkdir -p \"$S\""];
  // Stop the item owners first so a dying app cannot write stale positions
  // back over the ones we are about to set.
  lines.push(
    'osascript -e \'tell application id "computer.aesthetic.app" to quit\' >/dev/null 2>&1 || true',
    'sleep 1',
  );
  for (const [d, keys] of Object.entries(canon.statusItems)) {
    for (const [k, v] of Object.entries(keys)) {
      const flag = Number.isInteger(v) ? "-int" : typeof v === "number" ? "-float" : "-string";
      lines.push(`defaults write ${d} "${k}" ${flag} ${JSON.stringify(String(v))}`);
    }
  }
  for (const [f, on] of Object.entries(canon.slabFlags)) lines.push(on ? `touch "$S/${f}"` : `rm -f "$S/${f}"`);
  lines.push(
    "killall cfprefsd 2>/dev/null || true",
    "killall ControlCenter 2>/dev/null || true",
    'for a in computer.slab.menubar computer.aestheticcomputer.menuband computer.aestheticcomputer.menubandlauncher; do',
    '  if launchctl print "gui/$UID_/$a" >/dev/null 2>&1; then launchctl kickstart -k "gui/$UID_/$a";',
    '  elif [ -f "$HOME/Library/LaunchAgents/$a.plist" ]; then launchctl bootstrap "gui/$UID_" "$HOME/Library/LaunchAgents/$a.plist" 2>/dev/null || launchctl load "$HOME/Library/LaunchAgents/$a.plist";',
    '  else echo "warn: $a has no launch agent — run its install.sh once"; fi',
    "done",
    '[ -d /Applications/Aesthetic.Computer.app ] && open -g -a /Applications/Aesthetic.Computer.app || true',
    "sleep 2",
  );
  return lines.join("\n") + "\n";
}

function apply(hosts) {
  const canon = loadCanon();
  if (!canon) throw new Error(`no canon at ${CANON} — run capture first`);
  for (const host of hosts.length ? hosts : ["local"]) {
    const before = probe(host);
    const d0 = diff(canon, before);
    if (!d0.length) { console.log(`${before.name}: already at canon (${canon.capturedFrom} ${canon.capturedAt.slice(0, 10)})`); continue; }
    console.log(`${before.name}: applying ${d0.length} change(s)…`);
    const out = sh(host, applyScript(canon), { check: false });
    if (out.trim()) console.log(out.trim().split("\n").map((l) => "  " + l).join("\n"));
    const left = diff(canon, probe(host));
    if (left.length) {
      console.log(`  still off after apply (an app may have written its own position back):`);
      for (const [w, want, have] of left) console.log(`    ${w}: want ${want}, have ${have}`);
    } else console.log(`  ${before.name} now matches canon`);
  }
}

// ---------------------------------------------------------------- audit

function pad(s, n) { s = String(s); return s.length >= n ? s : s + " ".repeat(n - s.length); }

function audit(hosts) {
  const canon = loadCanon();
  const probes = ["local", ...hosts].map((h) => { try { return probe(h); } catch (e) { return { host: h, name: h, error: e.message.split("\n")[0], apps: {}, defaults: {}, flags: [] }; } });
  const w = Math.max(30, ...probes.map((p) => p.name.length + 2));
  console.log(pad("", 22) + probes.map((p) => pad(p.name, w)).join(""));
  console.log(pad("repo HEAD", 22) + probes.map((p) => pad(p.repo || (p.error ? "unreachable" : "-"), w)).join(""));
  for (const a of APPS) {
    const rows = probes.map((p) => p.apps[a.id]);
    const uuids = new Set(rows.filter(Boolean).map((r) => r.uuid));
    console.log(pad(`${a.label}`, 22) + rows.map((r) => pad(r ? `${r.ver} (${r.build})` : "missing", w)).join(""));
    console.log(pad(`  build uuid`, 22) + rows.map((r) => pad(r ? r.uuid.slice(0, 8) : "-", w)).join("") + (uuids.size > 1 ? "  ← differ" : uuids.size === 1 && rows.every(Boolean) ? "  ✓ same build" : ""));
    console.log(pad(`  signer`, 22) + rows.map((r) => pad(r ? r.signer.replace(/:.*/, "").slice(0, w - 2) : "-", w)).join(""));
    console.log(pad(`  running / agent`, 22) + rows.map((r) => pad(r ? `${r.pid !== "-" ? "pid " + r.pid : "NOT running"} / ${r.state}` : "-", w)).join(""));
  }
  console.log(pad("slab flags", 22) + probes.map((p) => pad(p.flags.length + " on", w)).join(""));
  for (const p of probes) if (p.flags.length) console.log(pad(`  ${p.name}`, 22) + p.flags.join(" "));
  if (!canon) { console.log(`\nno canon yet — run: node slab/bin/menubar-parity.mjs capture --from <reference-host>`); return; }
  console.log(`\nlayout vs canon (${canon.capturedFrom}, ${canon.capturedAt.slice(0, 10)}):`);
  for (const p of probes) {
    if (p.error) { console.log(`  ${p.name}: ${p.error}`); continue; }
    const d = diff(canon, p);
    console.log(`  ${p.name}: ${d.length ? d.length + " difference(s)" : "matches"}`);
    for (const [what, want, have] of d) console.log(`    ${what}: canon ${want}, here ${have}`);
  }
}

// ---------------------------------------------------------------- deploy

// Ship this Mac's installed bundles to other hosts so builds are byte-identical.
// Menu Band and the Electron app carry a Developer ID signature that is valid
// on every Mac, so they are rsynced whole. SlabMenubar is self-signed per
// machine, so it goes through deploy-host.sh (prebuilt binary, re-signed there).
function deploy(hosts) {
  if (!hosts.length) throw new Error("deploy needs at least one host");
  const local = probe("local");
  for (const host of hosts) {
    const remote = probe(host);
    console.log(`→ ${remote.name}`);
    for (const a of APPS) {
      const mine = local.apps[a.id], theirs = remote.apps[a.id];
      if (!mine) { console.log(`  ${a.label}: not installed here, skipping`); continue; }
      if (theirs && theirs.uuid === mine.uuid) { console.log(`  ${a.label}: same build already (${mine.uuid.slice(0, 8)})`); continue; }
      if (a.id === "slab") {
        console.log(`  ${a.label}: deploy-host.sh ${host}`);
        const r = spawnSync("bash", [join(REPO, "slab/menubar-swift/deploy-host.sh"), host], { stdio: "inherit" });
        if (r.status !== 0) console.log(`  ${a.label}: deploy-host.sh failed (build the release binary first: cd slab/menubar-swift && swift build -c release)`);
        continue;
      }
      const src = a.app.replace("$HOME", process.env.HOME);
      const dstApp = a.app.replace("$HOME", remote.home || "~");
      const esc = (p) => p.replace(/ /g, "\\ ");
      console.log(`  ${a.label}: rsync ${mine.ver} (${mine.uuid.slice(0, 8)}) → ${host}`);
      const stage = `${dstApp}.parity-stage`;
      const r = spawnSync("rsync", ["-a", "--delete", src + "/", `${host}:${esc(stage)}/`], { stdio: "inherit" });
      if (r.status !== 0) { console.log(`  ${a.label}: rsync failed`); continue; }
      const swap = `
set -u
UID_="$(id -u)"; app="${dstApp}"; stage="${stage}"
${a.id === "acapp" ? 'osascript -e \'tell application id "computer.aesthetic.app" to quit\' >/dev/null 2>&1 || true; sleep 1' : ""}
if [ -d "$app" ]; then mv "$app" "$app.parity-prev"; fi
mv "$stage" "$app" && rm -rf "$app.parity-prev"
xattr -dr com.apple.quarantine "$app" 2>/dev/null || true
${a.id === "acapp" ? 'open -g -a /Applications/Aesthetic.Computer.app' : `for l in ${a.agent} computer.aestheticcomputer.menubandlauncher; do
  if launchctl print "gui/$UID_/$l" >/dev/null 2>&1; then launchctl kickstart -k "gui/$UID_/$l";
  elif [ -f "$HOME/Library/LaunchAgents/$l.plist" ]; then launchctl bootstrap "gui/$UID_" "$HOME/Library/LaunchAgents/$l.plist" 2>/dev/null || true; fi
done`}
sleep 2
`;
      const out = sh(host, swap, { check: false });
      if (out.trim()) console.log(out.trim().split("\n").map((l) => "    " + l).join("\n"));
      const after = probe(host).apps[a.id];
      console.log(`  ${a.label}: ${after && after.uuid === mine.uuid ? "✓ deployed, same build" : "✗ still differs"}${after ? `, ${after.pid !== "-" ? "running" : "NOT running"}` : ""}`);
    }
  }
}

// ---------------------------------------------------------------- main

const [cmd, ...rest] = process.argv.slice(2);
try {
  switch (cmd) {
    case "audit": audit(rest); break;
    case "capture": { const i = rest.indexOf("--from"); capture(i >= 0 ? rest[i + 1] : "local"); break; }
    case "apply": apply(rest); break;
    case "deploy": deploy(rest); break;
    default:
      console.log(readFileSync(fileURLToPath(import.meta.url), "utf8").split("\n").filter((l) => l.startsWith("//")).slice(0, 17).map((l) => l.slice(3)).join("\n"));
      process.exit(cmd ? 1 : 0);
  }
} catch (e) { console.error(`menubar-parity: ${e.message}`); process.exit(1); }
