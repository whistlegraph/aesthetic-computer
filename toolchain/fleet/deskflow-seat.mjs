import { readFile, writeFile, mkdir, rename } from 'node:fs/promises';
import { homedir } from 'node:os';
import { join } from 'node:path';
import { randomUUID, createHash } from 'node:crypto';
import { displayTargets, listDisplays, run, native } from './displays.mjs';
import { addressParts } from './display-model.mjs';
import { compileSeat, screenNames, preflightSeatHosts } from './deskflow-seat-model.mjs';

const root = join(homedir(), '.config/slab/displays');
const seatPath = join(root, 'seat.json');
const receiptPath = join(root, 'last-seat-apply.json');
const script = await readFile(new URL('./deskflow-seat-host.py', import.meta.url), 'utf8');
const quote = text => `'${text.replaceAll("'", "'\\''")}'`;
const localName = (await run('/usr/sbin/scutil', ['--get', 'LocalHostName'])).trim();

async function host(machine, request) {
  const target = displayTargets([machine])[0];
  const options = { input: JSON.stringify(request), timeout: 30000 };
  const raw = target.localNames.includes(localName)
    ? await run('/usr/bin/python3', ['-c', script], options)
    : await run('/usr/bin/ssh', ['-o', 'BatchMode=yes', '-o', 'ConnectTimeout=5', target.target,
      `/usr/bin/python3 -c ${quote(script)}`], options);
  return JSON.parse(raw);
}
async function atomic(path, value) {
  await mkdir(root, { recursive: true });
  const temp = `${path}.${randomUUID()}.tmp`;
  await writeFile(temp, JSON.stringify(value, null, 2), { mode: 0o600 });
  await rename(temp, path);
}
async function readJSON(path) { try { return JSON.parse(await readFile(path, 'utf8')); } catch { return null; } }

export async function loadSeat() {
  const local = JSON.parse(await readFile(join(homedir(), '.config/slab/deskflow-handoff.json'), 'utf8'));
  const names = Object.keys(local.transportPeers ?? {});
  if (!names.length) throw new Error('No Deskflow fleet is configured');
  const [inventory, records] = await Promise.all([
    listDisplays({ machines: names }),
    Promise.all(names.map(async machine => {
      try { return { machine, ok: true, ...await host(machine, { operation: 'read' }) }; }
      catch (error) { return { machine, ok: false, error: error.message }; }
    })),
  ]);
  const active = records.find(r => r.ok && r.role === 'server');
  if (!active) throw new Error('The active Deskflow controller is unavailable');
  const saved = await readJSON(seatPath);
  const seed = saved ?? await readJSON(join(homedir(), 'Documents/Shelf/desktop-layout/desktop-map.json'));
  const configured = screenNames(active.config);
  const screens = [];
  for (const name of configured) {
    const record = records.find(r => r.ok && r.handoff.screenName === name);
    const savedScreen = seed?.screens.find(s => s.screenName === name);
    const machine = record?.machine ?? savedScreen?.machine;
    const found = inventory.machines.find(m => m.machine === machine && m.ok);
    const displays = found?.displays.filter(d => d.active) ?? [];
    const display = displays.find(d => d.main) ?? displays[0];
    const address = display?.address ?? savedScreen?.address ?? (machine ? `${machine}:1` : null);
    const previous = savedScreen ?? seed?.screens.find(s => address && s.address === address);
    const right = screens.reduce((right, s) => Math.max(right, s.x + s.width), 0);
    screens.push({ number: previous?.number ?? screens.length + 1, screenName: name, address,
      machine: machine ?? null, online: !!display, name: display?.name ?? name,
      multipleDisplays: displays.length > 1,
      x: previous?.x ?? right, y: previous?.y ?? 338,
      width: previous?.width ?? 400, height: previous?.height ?? 250,
      pixels: display ? `${display.bounds.width} × ${display.bounds.height}` : 'Offline',
    });
  }
  // Preserve existing seat numbers and assign new ones without collision.
  const seen = new Set();
  for (const s of screens) {
    if (seen.has(s.number)) s.number = Math.max(...screens.map(s => s.number)) + 1;
    seen.add(s.number);
  }
  const receipt = await readJSON(receiptPath);
  const pendingHosts = saved?.pendingHosts ?? [];
  const routesMatch = saved?.applied === true && saved?.configHash === active.hash;
  const warning = routesMatch && pendingHosts.length
    ? `Applied on connected Macs · Pending sync: ${pendingHosts.join(', ')}. Refresh and Apply when they return, before changing controllers.`
    : records.filter(r => !r.ok).map(r => `${r.machine} unavailable`).join('; ');
  return { version: 1, screens, hosts: records.map(r => ({ machine: r.machine, ok: r.ok,
    hash: r.hash, controller: r.handoff?.controller ?? saved?.hosts?.find(h => h.machine === r.machine)?.controller ?? false, error: r.error })),
    applied: routesMatch && !records.some(r => r.ok && pendingHosts.includes(r.machine)), pendingHosts,
    canRestore: !!receipt, controller: active.machine,
    warning };
}

export async function applySeat(seat) {
  if (!Array.isArray(seat.hosts) || !seat.hosts.length) throw new Error('Refresh the seat before applying');
  if (seat.screens.some(s => s.multipleDisplays)) throw new Error('This editor currently supports one active display per machine');
  const { hosts, active, pendingHosts } = await preflightSeatHosts(seat.hosts,
    machine => host(machine, { operation: 'read' }));
  const plan = compileSeat(seat, active.state.config);
  const routes = JSON.stringify({ version: 1, keys: plan.routeKeys, screens: seat.screens });
  const changes = hosts.map(h => ({ machine: h.machine, before: h.state,
    // Keep each host's settings; replace only links and our shortcut block.
    config: h.state.handoff.controller ? compileSeat(seat, h.state.config).config : plan.config }));
  // Update standby controllers and cached client geometry first, active server last.
  changes.sort((a, b) => Number(a.machine === active.machine) - Number(b.machine === active.machine));
  const completed = [], attempted = [];
  const journal = join(root, `seat-transaction-${randomUUID()}.json`);
  const receipt = { version: 1, beforeSeat: await readJSON(seatPath), changes: completed, pendingHosts };
  await atomic(journal, { status: 'prepared', changes, pendingHosts });
  try {
    for (const change of changes) {
      attempted.push({ ...change, hash: createHash('sha256').update(change.config).digest('hex') });
      await atomic(journal, { status: 'applying', attempted, completed });
      const result = await host(change.machine, { operation: 'write', expected: change.before.hash, config: change.config, routes });
      completed.push({ ...change, ...result });
    }
    await atomic(seatPath, { ...seat, applied: true, pendingHosts, configHash: plan.config === active.state.config ? active.state.hash : completed.find(c => c.machine === active.machine).hash });
    await atomic(receiptPath, receipt);
    await atomic(journal, { status: 'complete', ...receipt });
  } catch (error) {
    const failed = [];
    for (const change of [...attempted].reverse()) {
      try {
        // A lost SSH response is an unknown outcome: inspect before rolling back.
        const current = await host(change.machine, { operation: 'read' });
        if (current.hash === change.before.hash && current.routes === change.before.routes) continue;
        if (current.hash !== change.hash) throw new Error('Configuration changed again; saved backup requires review');
        await host(change.machine, { operation: 'write', expected: change.hash, config: change.before.config, routes: change.before.routes });
      }
      catch (rollback) { failed.push(`${change.machine}: ${rollback.message}`); }
    }
    if (!failed.length) await atomic(seatPath, receipt.beforeSeat);
    await atomic(journal, { status: failed.length ? 'recovery-needed' : 'rolled-back', attempted, failed });
    throw new Error(`${error.message}. ${failed.length ? `Restore required: ${failed.join('; ')}. Recovery record: ${journal}` : 'Earlier changes restored.'}`);
  }
  return { applied: true, pendingHosts, backups: completed.map(c => ({ machine: c.machine, path: c.backup })), seat: await loadSeat() };
}

export async function identifySeat() {
  const seat = await loadSeat();
  const results = await Promise.all(seat.screens.filter(s => s.online && s.address).map(async s => {
    const { machine, number } = addressParts(s.address);
    return native(displayTargets([machine])[0], ['identify', '8', String(number), String(s.number)]);
  }));
  return { identified: results.length };
}

export async function restoreSeat() {
  const receipt = await readJSON(receiptPath);
  if (!receipt?.changes?.length) throw new Error('No saved layout to restore');
  // Preflight all hosts before making changes; avoid silently replacing newer edits.
  for (const change of receipt.changes) {
    const current = await host(change.machine, { operation: 'read' });
    if (current.hash !== change.hash && current.hash !== change.before.hash) throw new Error(`${change.machine} has newer changes; automatic restore refused`);
  }
  const failures = [];
  for (const change of [...receipt.changes].reverse()) {
    try {
      const current = await host(change.machine, { operation: 'read' });
      if (current.hash === change.before.hash && current.routes === change.before.routes) continue;
      await host(change.machine, { operation: 'write', expected: change.hash, config: change.before.config, routes: change.before.routes });
    }
    catch (error) { failures.push(`${change.machine}: ${error.message}`); }
  }
  if (failures.length) throw new Error(`Restore incomplete: ${failures.join('; ')}`);
  await atomic(seatPath, receipt.beforeSeat ?? { version: 1, screens: [], applied: false });
  await atomic(receiptPath, null);
  return { restored: true, seat: await loadSeat() };
}
