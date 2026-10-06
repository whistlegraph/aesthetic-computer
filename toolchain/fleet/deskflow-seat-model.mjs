import { validateSeat, seatEdges, addressParts } from './display-model.mjs';

// Only peers already shown offline in the preview may be deferred. A peer
// disappearing, returning, or changing configuration requires a fresh preview.
export async function preflightSeatHosts(expected, readHost) {
  if (!Array.isArray(expected) || !expected.length) throw new Error('Refresh the seat before applying');
  if (new Set(expected.map(h => h.machine)).size !== expected.length) throw new Error('Duplicate seat host');
  const records = await Promise.all(expected.map(async h => {
    let state;
    try { state = await readHost(h.machine); }
    catch (error) {
      if (h.ok === false) return { machine: h.machine, pending: true };
      throw new Error(`${h.machine} unavailable; refresh before applying: ${error.message}`);
    }
    if (h.ok === false || h.hash !== state.hash) throw new Error(`${h.machine} changed; refresh before applying`);
    return { ...h, state };
  }));
  const hosts = records.filter(h => !h.pending);
  const controllers = hosts.filter(h => h.state.role === 'server');
  if (controllers.length !== 1) throw new Error('Exactly one active controller must be available');
  return { hosts, active: controllers[0], pendingHosts: records.filter(h => h.pending).map(h => h.machine) };
}

export function screenNames(config) {
  const section = /(?:^|\n)section: screens\s*\n([\s\S]*?)^end\s*$/m.exec(config)?.[1];
  if (!section) throw new Error('Missing Deskflow screens');
  return [...section.matchAll(/^\s*([\w.-]+):\s*$/gm)].map(m => m[1]);
}

const keys = [105, 107, 113, 106, 64, 79, 80, 90]; // macOS F13–F20
export function compileSeat(seat, config) {
  validateSeat(seat);
  const names = screenNames(config);
  if (names.length > keys.length) throw new Error('The seat editor supports up to eight machines');
  const used = new Set();
  const machines = new Set();
  for (const s of seat.screens) {
    if (!names.includes(s.screenName) || used.has(s.screenName)) throw new Error('Each Deskflow screen needs exactly one tile');
    used.add(s.screenName);
    if (s.address) {
      const { machine } = addressParts(s.address);
      if (machines.has(machine)) throw new Error('Deskflow addresses a whole Mac; multiple displays on one Mac need a local layout first');
      machines.add(machine);
    }
  }
  if (used.size !== names.length) throw new Error('Keep a tile for every configured Deskflow screen');
  const edges = seatEdges(seat.screens);
  const reached = new Set([seat.screens[0].number]);
  for (;;) {
    const before = reached.size;
    for (const edge of edges) if (reached.has(edge.from)) reached.add(edge.to);
    if (before === reached.size) break;
  }
  if (reached.size !== seat.screens.length) throw new Error('Close the gaps: every monitor must share an edge with the seat');
  const percent = n => Math.round(n * 100);
  const range = span => {
    const [a, b] = span.map(percent);
    if (a >= b) throw new Error('An edge is too small for Deskflow; increase its overlap');
    return a === 0 && b === 100 ? '' : `(${a},${b})`;
  };
  const lines = ['section: links'];
  for (const s of seat.screens) {
    lines.push(`\t${s.screenName}:`);
    for (const e of edges.filter(e => e.from === s.number)) {
      const destination = seat.screens.find(s => s.number === e.to);
      lines.push(`\t\t${e.side}${range(e.source)} = ${destination.screenName}${range(e.destination)}`);
    }
  }
  lines.push('end');
  let output = config.replace(/^section: links\s*\n[\s\S]*?^end\s*$/m, lines.join('\n'));
  if (output === config && !config.includes(lines.join('\n'))) throw new Error('Missing Deskflow links section');
  // Private routing shortcuts keep controller handoff independent of tile positions.
  const routeKeys = Object.fromEntries([...names].sort().map((name, i) => [name, keys[i]]));
  const start = '# slab-seat shortcuts begin', end = '# slab-seat shortcuts end';
  output = output.replace(/\n[\t ]*# slab-seat shortcuts begin[\s\S]*?# slab-seat shortcuts end\n?/g, '\n');
  const shortcuts = [...names].sort().map((name, i) => `\tkeystroke(Control+Alt+Shift+F${13 + i}) = switchToScreen(${name})`).join('\n');
  output = output.replace(/^(section: options\s*\n)/m, `$1\t${start}\n${shortcuts}\n\t${end}\n`);
  if (!output.includes(start)) throw new Error('Missing Deskflow options section');
  return { config: output, edges, routeKeys };
}
