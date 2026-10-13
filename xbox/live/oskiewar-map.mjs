// Portable workshop maps. Only data crosses the coach connection.
export const MAP_FORMAT = 'ac.oskiewar.map';
export const ITEM_KINDS = ['HANDGUN', 'SPACE LASER', 'RUBBER SMG',
  'ROCKET LAUNCHER', 'LIGHT SABER', 'GRENADE'];
export function validateMap(value) {
  const fail = message => { throw new Error(message); };
  const number = (n, lo, hi) => Number.isFinite(n) && n >= lo && n <= hi;
  if (!value || value.format !== MAP_FORMAT || value.version !== 1)
    fail('Expected ac.oskiewar.map version 1');
  if (typeof value.name !== 'string' || !value.name.trim() || value.name.length > 60)
    fail('Name must contain 1–60 characters');
  const columns = value.columns ?? 40;
  if (!Number.isInteger(columns) || !number(columns, 8, 40)) fail('Use 8–40 columns');
  if (!Array.isArray(value.features) || !value.features.length || value.features.length > 24)
    fail('Use 1–24 terrain segments');
  let edge = 0;
  const features = value.features.map(f => {
    if (!f || f.from !== edge || !number(f.to, f.from + .25, columns) ||
        !['flat', 'bank', 'transition'].includes(f.kind) ||
        !number(f.lift ?? 0, -450, 720) || !number(f.rise ?? 0, 0, 720) ||
        (f.kind !== 'flat' && ![-1, 1].includes(f.dir)))
      fail(`Terrain must cover columns 0–${columns} in order; lift -450–720, rise 0–720`);
    edge = f.to;
    return { from: f.from, to: f.to, kind: f.kind, lift: f.lift ?? 0,
      rise: f.rise ?? 0, dir: f.dir === -1 ? -1 : 1 };
  });
  if (edge !== columns) fail(`Terrain must end at column ${columns}`);
  if (!Array.isArray(value.spawns) || value.spawns.length !== 2 ||
      !value.spawns.every(col => number(col, 0, columns - 1))) fail('Supply two spawn columns within the map');
  if (!Array.isArray(value.decks) || value.decks.length > 16) fail('Use at most 16 decks');
  const decks = value.decks.map(d => {
    if (!d || !number(d.col, 0, columns - 1) || !number(d.cols, .5, columns - d.col) ||
        !number(d.row, 1, 14)) fail('Invalid deck col/cols/row');
    return { col: d.col, cols: d.cols, row: d.row };
  });
  if (!Array.isArray(value.pickups) || value.pickups.length > 32) fail('Use at most 32 pickups');
  const pickups = value.pickups.map(p => {
    if (!p || !ITEM_KINDS.includes(p.kind) || !number(p.col, 0, columns - 1) ||
        !Number.isInteger(p.amount) || !number(p.amount, 0, 99)) fail('Invalid pickup kind/col/amount');
    return { kind: p.kind, col: p.col, amount: p.amount };
  });
  return { format: MAP_FORMAT, version: 1, name: value.name.trim(), features,
    ...(value.columns === undefined ? {} : { columns }),
    decks, spawns: value.spawns.slice(), pickups, skateboard: value.skateboard === true };
}
