#!/usr/bin/env node
// slab-suite — one lock for the Mac suite, and the hand that holds each Mac to it.
//
// slab/fleet/suite.json is the policy: which Macs exist, which profile each
// wears (studio = the whole instrument, work = the client minis' slimmer set),
// who leads and who follows. The lock — the exact build of every part the
// fleet should be running — is state, kept beside the watch in
// ~/.local/share/slab/suite/lock.json. Apps are locked by Mach-O UUID, not
// version string — two Macs that each compiled "1.6.14" are not the same
// build. CLIs are locked by version.
//
// Things get tried on the leader first. When the leader starts running a build
// it was not running at the last watch, and that build is up, the lock moves
// to it; followers are then brought over once they sit idle.
//
//   node slab/bin/slab-suite.mjs status [host…] [--json]   # fleet vs lock; exit 1 on drift
//   node slab/bin/slab-suite.mjs pin <part…|all> --from H  # lock to what H runs now
//   node slab/bin/slab-suite.mjs sync <host…> [--dry] [--only part]  # bring hosts to the lock
//   node slab/bin/slab-suite.mjs watch [--heal]            # state file + one notice per new drift;
//                                                          # --heal quietly syncs the manifest's followers
//
// sync never compiles on the target. It finds a Mac that already holds the
// locked build, ships that bundle whole, keeps the old one as a tarball under
// ~/.local/share/slab/suite/backups, and proves the result by UUID.
// Menu bar layout stays with menubar-parity.mjs; sync calls its `apply`.

import { spawnSync } from "node:child_process";
import { existsSync, mkdirSync, mkdtempSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { hostname, tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = join(HERE, "../..");
const MANIFEST = join(REPO, "slab/fleet/suite.json");
const STATE = join(process.env.HOME, ".local/share/slab/suite");

// ship: whole = Developer ID, valid anywhere · adhoc = re-signed where it lands
//       slab  = self-signed per Mac so TCC grants hold; goes through deploy-host.sh
export const PARTS = {
  claude: { kind: "cli", label: "Claude Code", up: "claude update" },
  codex: { kind: "cli", label: "Codex" },
  slab: { kind: "app", label: "SlabMenubar", app: "$HOME/Applications/SlabMenubar.app", bin: "slab-menubar", agents: ["computer.slab.menubar"], ship: "slab" },
  menuband: { kind: "app", label: "Menu Band", app: "$HOME/Applications/Menu Band.app", bin: "MenuBand", agents: ["computer.aestheticcomputer.menuband", "computer.aestheticcomputer.menubandlauncher"], ship: "whole" },
  wallpaper: { kind: "app", label: "Wallpaper", app: "$HOME/Applications/BlueberryWallpaper.app", bin: "BlueberryWallpaper", agents: ["computer.aesthetic.blueberry-wallpaper"], ship: "adhoc" },
  macpal: { kind: "app", label: "MacPal", app: "/Applications/MacPal.app", bin: "MacPal", agents: ["computer.aesthetic.macpal"], ship: "adhoc" },
  acapp: { kind: "app", label: "Aesthetic.Computer", app: "/Applications/Aesthetic.Computer.app", bin: "Aesthetic.Computer", agents: [], ship: "whole" },
};

const PATHS = 'export PATH="$HOME/.local/bin:/opt/homebrew/bin:/usr/local/bin:$PATH"';

// Codex lands three ways across the fleet (standalone, brew cask, npm prefix
// under ~/.local/share/codex-cli). Its own `update` handles the first two and
// dies on the third, so that layout is installed at the locked version here.
const codexUp = (want) => `
link="$(readlink "$HOME/.local/bin/codex" 2>/dev/null || true)"
case "$link" in
  */.local/share/codex-cli/*) p="$HOME/.local/share/codex-cli/${want}"
    npm install -g --prefix "$p" @openai/codex@${want} >/dev/null && "$p/bin/codex" --version >/dev/null && ln -sfn "$p/bin/codex" "$HOME/.local/bin/codex" ;;
  *) codex update >/dev/null 2>&1 ;;
esac`;

function sh(host, script, { check = true, stdio } = {}) {
  const argv = isLocal(host) ? ["bash", ["-s"]] : ["ssh", ["-o", "BatchMode=yes", "-o", "ConnectTimeout=8", host, "bash", "-s"]];
  const r = spawnSync(argv[0], argv[1], { input: script, encoding: "utf8", maxBuffer: 1 << 26, stdio });
  if (check && r.status !== 0) throw new Error(`[${host}] ${(r.stderr || r.stdout || "").trim().split("\n").pop()}`);
  return r.stdout || "";
}

const ME = hostname().split(".")[0];
const isLocal = (host) => !host || host === "local" || host === ME;

const PROBE = `
${PATHS}
echo "home	$HOME"
ver() { "$@" 2>/dev/null | grep -Eo '[0-9]+\\.[0-9]+\\.[0-9]+' | head -1; }
echo "cli	claude	$(ver claude --version)"
echo "cli	codex	$(ver codex --version)"
app() { # id path bin agent
  [ -d "$2" ] || { echo "app	$1	missing"; return; }
  local p="$2/Contents/Info.plist" pid
  pid="$(pgrep -x "$3" | head -1)"
  echo "app	$1	$(/usr/libexec/PlistBuddy -c 'Print CFBundleShortVersionString' "$p" 2>/dev/null) ($(/usr/libexec/PlistBuddy -c 'Print CFBundleVersion' "$p" 2>/dev/null))	$(dwarfdump --uuid "$2/Contents/MacOS/$3" 2>/dev/null | awk '{print $2; exit}')	\${pid:--}"
}
${Object.entries(PARTS).filter(([, p]) => p.kind === "app").map(([id, p]) => `app ${id} "${p.app}" "${p.bin}"`).join("\n")}
`;

// → { home, parts: { id: { version, id?, pid? } | null } }
export function parseProbe(out) {
  const r = { parts: {} };
  for (const line of out.split("\n")) {
    const [k, id, a, b, c] = line.split("\t");
    if (k === "home") r.home = id;
    else if (k === "cli") r.parts[id] = a ? { version: a } : null;
    else if (k === "app") r.parts[id] = a === "missing" ? null : { version: a, id: b, pid: c === "-" ? null : c };
  }
  return r;
}

function probe(host) {
  try { return parseProbe(sh(host, PROBE)); } catch (e) { return { error: e.message, parts: {} }; }
}

const cmp = (a, b) => a.split(".").map(Number).reduce((d, n, i) => d || n - Number(b.split(".")[i] || 0), 0);

// One part on one Mac against the lock: ok · drift · ahead · missing · stopped · unpinned · off (not in profile)
export function judge(part, have, lock, wanted) {
  if (!wanted) return "off";
  if (!have) return "missing";
  if (!lock) return "unpinned";
  if (part.kind === "cli") { const d = cmp(have.version, lock.version); return d < 0 ? "drift" : d > 0 ? "ahead" : "ok"; }
  if (have.id !== lock.id) return "drift";
  return part.agents.length && !have.pid ? "stopped" : "ok";
}

const LOCK = join(STATE, "lock.json");
const read = (file, or) => (existsSync(file) ? JSON.parse(readFileSync(file, "utf8")) : or);
const load = () => ({ ...JSON.parse(readFileSync(MANIFEST, "utf8")), lock: read(LOCK, {}) });
const save = (m) => { mkdirSync(STATE, { recursive: true }); writeFileSync(LOCK, JSON.stringify(m.lock, null, 2) + "\n"); };
const wants = (m, host, id) => (m.profiles[m.hosts[host]] || []).includes(id);

function survey(m, hosts) {
  return Object.fromEntries(hosts.map((h) => {
    if (!m.hosts[h]) throw new Error(`${h} is not in ${MANIFEST}`);
    const p = probe(h);
    const verdicts = Object.fromEntries(Object.entries(PARTS).map(([id, part]) => [id, p.error ? "unreachable" : judge(part, p.parts[id], m.lock[id], wants(m, h, id))]));
    return [h, { ...p, verdicts }];
  }));
}

const BAD = new Set(["drift", "missing", "stopped"]);
const MARK = { ok: "✓", drift: "✗ drift", ahead: "↑ ahead", missing: "✗ missing", stopped: "✗ not running", unpinned: "? unpinned", off: "·", unreachable: "—" };
const pad = (s, n) => String(s).padEnd(n);
const short = (x) => (x?.id ? `${x.version} ${x.id.slice(0, 8)}` : x?.version || "");

function table(m, seen) {
  const hosts = Object.keys(seen), w = 30;
  console.log(pad("", 20) + pad("lock", w) + hosts.map((h) => pad(`${h} (${m.hosts[h]})`, w)).join(""));
  for (const [id, part] of Object.entries(PARTS)) {
    console.log(pad(part.label, 20) + pad(short(m.lock[id]) || "-", w) + hosts.map((h) => {
      const v = seen[h].verdicts[id], have = seen[h].parts[id];
      return pad(v === "ok" || v === "off" || v === "unreachable" ? MARK[v] : `${MARK[v]}${have ? "  " + short(have) : ""}`, w);
    }).join(""));
  }
  for (const h of hosts) if (seen[h].error) console.log(`\n${h}: ${seen[h].error}`);
}

const drifted = (seen) => Object.entries(seen).flatMap(([h, s]) => Object.entries(s.verdicts).filter(([, v]) => BAD.has(v)).map(([id, v]) => `${h}:${id}:${v}`));

function status(hosts, json) {
  const m = load(), seen = survey(m, hosts.length ? hosts : Object.keys(m.hosts));
  if (json) console.log(JSON.stringify({ lock: m.lock, hosts: seen }, null, 2)); else table(m, seen);
  process.exit(drifted(seen).length ? 1 : 0);
}

function pin(ids, from) {
  if (!from) throw new Error("pin needs --from <host>");
  const m = load(), p = probe(from);
  if (p.error) throw new Error(p.error);
  for (const id of ids[0] === "all" ? Object.keys(PARTS) : ids) {
    const have = p.parts[id];
    if (!PARTS[id] || !have) { console.log(`${id}: not on ${from}, left alone`); continue; }
    m.lock[id] = { version: have.version, ...(have.id ? { id: have.id } : {}), from, at: new Date().toISOString().slice(0, 10) };
    console.log(`${id}: locked to ${short(have)} (${from})`);
  }
  save(m);
}

// Land a locked bundle at <tmp>/<id>.app on this Mac, from whichever Mac holds it.
function fetch(id, part, lock, seen, tmp) {
  const src = Object.keys(seen).sort((a, b) => isLocal(b) - isLocal(a)).find((h) => seen[h].parts[id]?.id === lock.id);
  if (!src) throw new Error(`no reachable Mac holds ${part.label} ${lock.id.slice(0, 8)} — pin again or bring ${lock.from} online`);
  const path = part.app.replace("$HOME", seen[src].home), dst = join(tmp, `${id}.app`);
  mkdirSync(dst, { recursive: true });
  const pull = isLocal(src)
    ? spawnSync("rsync", ["-a", path + "/", dst + "/"])
    : spawnSync("bash", ["-c", `ssh -o BatchMode=yes "$1" 'bash -s' <<<"tar -C '${path}' -cf - ." | tar -x -C "$2"`, "-", src, dst]);
  if (pull.status !== 0) throw new Error(`could not fetch ${part.label} from ${src}`);
  return { src, dst };
}

const swap = (id, part) => `
set -eu
U="$(id -u)"; app="${part.app}"; stage="$HOME/.local/share/slab/suite/stage/${id}.app"
keep="$HOME/.local/share/slab/suite/backups/$(date +%Y%m%d-%H%M%S)"; mkdir -p "$keep"
[ -d "$app" ] && tar -czf "$keep/${id}.tgz" -C "$(dirname "$app")" "$(basename "$app")"
ls -dt "$HOME/.local/share/slab/suite/backups"/* | tail -n +6 | xargs rm -rf
${id === "acapp" ? `osascript -e 'tell application id "computer.aesthetic.app" to quit' >/dev/null 2>&1 || true; sleep 1` : ""}
${part.ship === "adhoc" ? `# launchd pins an ad-hoc job to the signature it first saw; a kickstart onto
# the re-signed bundle dies OS_REASON_CODESIGNING, so these jobs are reloaded.
for a in ${part.agents.join(" ")}; do launchctl bootout "gui/$U/$a" 2>/dev/null || true; done; sleep 1` : ""}
rm -rf "$app"; mv "$stage" "$app"
xattr -dr com.apple.quarantine "$app" 2>/dev/null || true
${part.ship === "adhoc" ? 'codesign --force --deep --options runtime -s - "$app" 2>/dev/null' : ""}
for a in ${part.agents.join(" ")}; do
  if launchctl print "gui/$U/$a" >/dev/null 2>&1; then launchctl kickstart -k "gui/$U/$a"
  elif [ -f "$HOME/Library/LaunchAgents/$a.plist" ]; then launchctl bootstrap "gui/$U" "$HOME/Library/LaunchAgents/$a.plist"
  else echo "warn: no launch agent $a here — run its installer once"; fi
done
${id === "acapp" ? 'open -g -a "$app"' : ""}
sleep 3
`;

function shipSlab(host, bundle) {
  const r = spawnSync("bash", [join(REPO, "slab/menubar-swift/deploy-host.sh"), host], { env: { ...process.env, SLAB_PREBUILT: join(bundle, "Contents/MacOS/slab-menubar") }, encoding: "utf8" });
  if (r.status !== 0) throw new Error(`deploy-host.sh ${host}: ${(r.stderr || r.stdout).trim().split("\n").pop()}`);
}

function sync(hosts, dry, only) {
  if (!hosts.length) throw new Error("sync needs the hosts by name — it never sweeps the whole fleet");
  const m = load(), seen = survey(m, [...new Set([...Object.keys(m.hosts).filter(isLocal), ...Object.keys(m.hosts)])]);
  const tmp = mkdtempSync(join(tmpdir(), "slab-suite-"));
  for (const host of hosts) {
    const todo = Object.entries(seen[host]?.verdicts || {}).filter(([id, v]) => BAD.has(v) && (!only || id === only));
    if (seen[host]?.error) { console.log(`${host}: unreachable — ${seen[host].error}`); continue; }
    if (!todo.length) { console.log(`${host}: at the lock`); continue; }
    console.log(`${host}: ${todo.map(([id, v]) => `${id} (${v})`).join(", ")}`);
    if (dry) continue;
    for (const [id, verdict] of todo) {
      const part = PARTS[id], lock = m.lock[id];
      try {
        if (part.kind === "cli") sh(host, `${PATHS}\n${id === "codex" ? codexUp(lock.version) : part.up + " >/dev/null 2>&1"}`, { check: false });
        else if (verdict === "stopped") sh(host, `for a in ${part.agents.join(" ")}; do launchctl kickstart "gui/$(id -u)/$a" 2>/dev/null || launchctl bootstrap "gui/$(id -u)" "$HOME/Library/LaunchAgents/$a.plist"; done; sleep 3`, { check: false });
        else {
          const { src, dst } = fetch(id, part, lock, seen, tmp);
          if (part.ship === "slab") shipSlab(host, dst);
          else {
            const stage = ".local/share/slab/suite/stage";
            if (isLocal(host)) { mkdirSync(join(process.env.HOME, stage), { recursive: true }); spawnSync("rsync", ["-a", "--delete", dst + "/", join(process.env.HOME, stage, `${id}.app`) + "/"]); }
            else { sh(host, `mkdir -p "$HOME/${stage}"`); spawnSync("rsync", ["-a", "--delete", dst + "/", `${host}:${stage}/${id}.app/`]); }
            const out = sh(host, swap(id, part), { check: false }).trim();
            if (out) console.log("    " + out.split("\n").join("\n    "));
          }
          console.log(`  ${part.label}: shipped from ${src}`);
        }
      } catch (e) { console.log(`  ${part.label}: ${e.message}`); }
    }
    const after = probe(host);
    for (const [id] of todo) {
      const v = judge(PARTS[id], after.parts[id], m.lock[id], true);
      console.log(`  ${PARTS[id].label}: ${BAD.has(v) ? MARK[v] + "  " + short(after.parts[id]) : "✓ " + short(after.parts[id])}`);
    }
    if (m.hosts[host] === "studio") spawnSync("node", [join(HERE, "menubar-parity.mjs"), "apply", isLocal(host) ? "local" : host], { stdio: "inherit" });
  }
  rmSync(tmp, { recursive: true, force: true });
}

// For the launch agent: keep a state file the menubar can read, and speak up
// once when the set of drifted parts changes — not every hour it stays drifted.
// --heal brings a follower back without asking, but only once nobody has
// touched it for five minutes: a sync restarts the instrument.
const idle = (host) => Number(sh(host, "ioreg -c IOHIDSystem | awk '/HIDIdleTime/ {print int($NF/1000000000); exit}'", { check: false })) || 0;

// What the leader runs now against what it ran at the last watch → lock moves.
// A build that is not up, or a CLI older than the lock, is never promoted, and
// stays unrecorded so it is weighed again next time.
export function promote(lead, was, lock) {
  const is = { ...was }, moves = {};
  for (const [id, have] of Object.entries(lead.parts)) {
    if (!have || !PARTS[id]) continue;
    const key = have.id || have.version, part = PARTS[id];
    const fresh = was && was[id] !== key && key !== (lock[id]?.id || lock[id]?.version);
    const sound = part.kind === "cli" ? !lock[id] || cmp(have.version, lock[id].version) > 0 : !part.agents.length || !!have.pid;
    if (fresh && !sound) continue;
    is[id] = key;
    if (fresh) moves[id] = { version: have.version, ...(have.id ? { id: have.id } : {}) };
  }
  return { is, moves };
}

function lead(m, seen) {
  const l = seen[m.lead], file = join(STATE, "lead.json");
  if (!l || l.error) return false;
  const { is, moves } = promote(l, read(file, null), m.lock);
  mkdirSync(STATE, { recursive: true });
  writeFileSync(file, JSON.stringify(is, null, 2) + "\n");
  for (const [id, to] of Object.entries(moves)) {
    m.lock[id] = { ...to, from: m.lead, at: new Date().toISOString().slice(0, 10) };
    console.log(`${id}: lock follows ${m.lead} → ${short(to)}`);
  }
  if (Object.keys(moves).length) save(m);
  return Object.keys(moves).length > 0;
}

function watch(heal) {
  const m = load();
  let seen = survey(m, Object.keys(m.hosts));
  if (lead(m, seen)) seen = survey(m, Object.keys(m.hosts));
  const due = heal ? (m.follow || []).filter((h) => !seen[h].error && drifted({ [h]: seen[h] }).length && idle(h) >= 300) : [];
  if (due.length) { sync(due, false); seen = survey(m, Object.keys(m.hosts)); }
  mkdirSync(STATE, { recursive: true });
  const file = join(STATE, "status.json");
  const before = read(file, {}).drift || [];
  // A sleeping laptop keeps its last known drift, so waking is not news.
  const now = [...drifted(seen), ...before.filter((d) => seen[d.split(":")[0]]?.error)];
  writeFileSync(file, JSON.stringify({ at: new Date().toISOString(), drift: now, unreachable: Object.keys(seen).filter((h) => seen[h].error), lock: m.lock }, null, 2) + "\n");
  const fresh = now.filter((d) => !before.includes(d));
  if (fresh.length) spawnSync("osascript", ["-e", `display notification ${JSON.stringify(fresh.map((d) => d.split(":").slice(0, 2).join(" ")).join(", "))} with title "Slab suite drift" subtitle "slab-suite sync <host> brings it back"`]);
  console.log(`${new Date().toISOString()} drift=${now.length} new=${fresh.length} ${now.join(" ")}`);
}

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  const [cmd, ...rest] = process.argv.slice(2);
  const only = rest.includes("--only") ? rest[rest.indexOf("--only") + 1] : null;
  const flag = (f) => rest.includes(f), names = rest.filter((a, i) => !a.startsWith("--") && rest[i - 1] !== "--from");
  try {
    if (cmd === "status") status(names, flag("--json"));
    else if (cmd === "pin") pin(names, rest[rest.indexOf("--from") + 1]);
    else if (cmd === "sync") sync(names.filter((n) => n !== only), flag("--dry"), only);
    else if (cmd === "watch") watch(flag("--heal"));
    else { console.log(readFileSync(fileURLToPath(import.meta.url), "utf8").split("\n").slice(1, 26).map((l) => l.slice(3)).join("\n")); process.exit(cmd ? 1 : 0); }
  } catch (e) { console.error(`slab-suite: ${e.message}`); process.exit(1); }
}
