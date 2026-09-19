// How one Aesel process names, resumes, and addresses its windows.
// Pure filesystem helpers; the Electron host in main.cjs calls them.
const fs = require('node:fs');
const path = require('node:path');
const { createHash } = require('node:crypto');

const INSTANCE = /^(default|window-[a-f0-9-]{36})$/;
// A restart or update writes a marker per window; markers older than this are
// leftovers from a launch that never came back, not sessions to reopen.
const CONTINUE_WITHIN_MS = 120000;

// argv after the executable (and the app path in development).
function launchOptions(argv) {
  const option = name => { const at = argv.indexOf(name); return at < 0 ? '' : argv[at + 1] || ''; };
  return { argv, option, flag: name => argv.includes(name) };
}

function sessionFile(stateRoot, workspace, instance) {
  return path.join(stateRoot, 'sessions', createHash('sha256').update(`${workspace}\0${instance}`).digest('hex') + '.json');
}

function writeContinuation(file, { workspace, instance, host, at = Date.now() }) {
  fs.writeFileSync(file, JSON.stringify({ cwd: workspace, instance, host, at }), { mode: 0o600 });
}

// Fresh continuation markers left by this host, oldest first. Every marker is
// consumed, stale or not, so a failed launch cannot reopen weeks later.
function resumableSessions(sessionDir, { host, now = Date.now() }) {
  let names; try { names = fs.readdirSync(sessionDir); } catch { return []; }
  const found = [];
  for (const name of names) {
    if (!name.endsWith('.json.continue')) continue;
    const file = path.join(sessionDir, name);
    try {
      const marker = JSON.parse(fs.readFileSync(file, 'utf8'));
      const age = now - Number(marker.at);
      const markerHost = marker.host || 'studio';
      if (typeof marker.cwd === 'string' && marker.cwd && age >= 0 && age < CONTINUE_WITHIN_MS && markerHost === host) {
        found.push({ workspace: marker.cwd, instance: INSTANCE.test(marker.instance || '') ? marker.instance : 'default', at: Number(marker.at) });
      }
    } catch {}
    fs.rmSync(file, { force: true });
  }
  return found.sort((a, b) => a.at - b.at);
}

// The letter a window is known by: A, B, C… across every live Aesel window on
// this machine, whichever process hosts it. Dead owners are swept.
function claimAddress(dir, { instance, workspace, pid = process.pid, alive = n => process.kill(n, 0) }) {
  fs.mkdirSync(dir, { recursive: true, mode: 0o700 });
  for (let index = 0; index < 26; index++) {
    const label = String.fromCharCode(65 + index), file = path.join(dir, `${label}.json`);
    try {
      const owner = JSON.parse(fs.readFileSync(file, 'utf8'));
      try { alive(Number(owner.pid)); continue; } catch { fs.rmSync(file, { force: true }); }
    } catch {}
    try {
      const fd = fs.openSync(file, 'wx', 0o600);
      fs.writeFileSync(fd, JSON.stringify({ pid, instance, workspace, startedAt: Date.now() }));
      fs.closeSync(fd);
      return { label, path: file };
    } catch {}
  }
  return { label: String(pid), path: '' };
}

function releaseAddress(address, { instance, pid = process.pid }) {
  if (!address?.path) return;
  try {
    const owner = JSON.parse(fs.readFileSync(address.path, 'utf8'));
    if (Number(owner.pid) === pid && (owner.instance === undefined || owner.instance === instance)) fs.rmSync(address.path, { force: true });
  } catch {}
}

// A development build applies to a window only once its agent has announced
// itself and drawn a first state, and never while a restart is already in flight.
function readyForDevApply({ action, agentReady, lastVisibleState, pendingRestart }) {
  return !!action && agentReady === true && !!lastVisibleState && !pendingRestart;
}

module.exports = { INSTANCE, readyForDevApply, CONTINUE_WITHIN_MS, launchOptions, sessionFile, writeContinuation, resumableSessions, claimAddress, releaseAddress };
