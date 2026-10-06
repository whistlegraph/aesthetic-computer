export function addressParts(address) {
  const match = /^([a-zA-Z0-9][a-zA-Z0-9.-]*):([1-9][0-9]*)$/.exec(address);
  if (!match || !Number.isSafeInteger(Number(match[2]))) throw new Error("Expected a display address such as neo:1");
  return { machine: match[1], number: Number(match[2]) };
}

export function snapshot(inventory) {
  return inventory.displays.filter(d => d.active).map(d => {
    if (!d.mode) throw new Error(`No current mode for display ${d.number}`);
    return { uuid: d.uuid, x: d.bounds.x, y: d.bounds.y, modeID: d.mode.id, rotation: d.rotation };
  }).sort((a, b) => a.uuid.localeCompare(b.uuid));
}

export function validateRectangles(rects) {
  if (!rects.length) throw new Error("Layout is empty");
  for (const r of rects) {
    for (const key of ["x", "y", "width", "height"]) {
      if (!Number.isInteger(r[key]) || Math.abs(r[key]) > 100000) throw new Error(`Invalid ${key}`);
    }
    if (r.width <= 0 || r.height <= 0) throw new Error("Dimensions must be positive");
  }
  for (let i = 0; i < rects.length; i++) for (let j = i + 1; j < rects.length; j++) {
    const a = rects[i], b = rects[j];
    if (Math.min(a.x + a.width, b.x + b.width) > Math.max(a.x, b.x)
      && Math.min(a.y + a.height, b.y + b.height) > Math.max(a.y, b.y)) throw new Error("Displays overlap");
  }
}

export function planLayout(inventory, changes, modes = new Map()) {
  if (inventory.displays.some(d => d.mirrored)) throw new Error("Mirrored layouts are read-only");
  const expected = snapshot(inventory);
  const layout = expected.map(p => ({ ...p }));
  const seen = new Set();
  for (const change of changes) {
    if (seen.has(change.number)) throw new Error("Duplicate display change");
    seen.add(change.number);
    const d = inventory.displays.find(d => d.number === change.number && d.active);
    if (!d) throw new Error(`Display ${change.number} is not active`);
    for (const key of Object.keys(change)) if (!["number", "x", "y", "modeID"].includes(key)) throw new Error(`Unknown layout field: ${key}`);
    const p = layout.find(p => p.uuid === d.uuid);
    for (const key of ["x", "y", "modeID"]) if (change[key] !== undefined) {
      if (!Number.isInteger(change[key])) throw new Error(`${key} must be an integer`);
      p[key] = change[key];
    }
  }
  const rects = layout.map(p => {
    const d = inventory.displays.find(d => d.uuid === p.uuid);
    const mode = p.modeID === d.mode.id ? d.mode : modes.get(d.number)?.find(m => m.id === p.modeID);
    if (!mode) throw new Error(`Unavailable mode ${p.modeID} for display ${d.number}`);
    // Use the observed frame for unchanged modes, including rotated displays.
    const portrait = d.rotation % 180 !== 0;
    const width = p.modeID === d.mode.id ? d.bounds.width : portrait ? mode.height : mode.width;
    const height = p.modeID === d.mode.id ? d.bounds.height : portrait ? mode.width : mode.height;
    return { x: p.x, y: p.y, width, height };
  });
  validateRectangles(rects);
  if (!rects.some(r => r.x === 0 && r.y === 0)) throw new Error("One display must start at (0,0)");
  const edges = seatEdges(rects.map((r, i) => ({ ...r, number: i + 1 })));
  const reached = new Set([1]);
  for (;;) {
    const size = reached.size;
    for (const e of edges) if (reached.has(e.from)) reached.add(e.to);
    if (reached.size === size) break;
  }
  if (reached.size !== rects.length) throw new Error("Displays must share edges; close the gaps");
  return { expected, layout };
}

// Physical seat coordinates are distinct from each Mac's Quartz coordinates.
// Every shared edge records both spans, so a wide panel can meet two laptops.
export function seatEdges(screens) {
  validateRectangles(screens);
  const edges = [];
  for (const a of screens) for (const b of screens) if (a !== b) {
    let side, lo, hi, startA, startB, sizeA, sizeB;
    if (a.x + a.width === b.x || b.x + b.width === a.x) {
      side = a.x + a.width === b.x ? "right" : "left";
      lo = Math.max(a.y, b.y); hi = Math.min(a.y + a.height, b.y + b.height);
      startA = a.y; startB = b.y; sizeA = a.height; sizeB = b.height;
    } else if (a.y + a.height === b.y || b.y + b.height === a.y) {
      side = a.y + a.height === b.y ? "down" : "up";
      lo = Math.max(a.x, b.x); hi = Math.min(a.x + a.width, b.x + b.width);
      startA = a.x; startB = b.x; sizeA = a.width; sizeB = b.width;
    }
    if (side && hi > lo) edges.push({ from: a.number, to: b.number, side,
      source: [(lo - startA) / sizeA, (hi - startA) / sizeA],
      destination: [(lo - startB) / sizeB, (hi - startB) / sizeB] });
  }
  return edges;
}

export function validateSeat(seat) {
  if (seat.version !== 1 || !Array.isArray(seat.screens)) throw new Error("Expected a version 1 seat map");
  const numbers = new Set(), addresses = new Set();
  for (const s of seat.screens) {
    if (!Number.isInteger(s.number) || s.number < 1 || numbers.has(s.number)) throw new Error("Seat numbers must be unique positive integers");
    numbers.add(s.number);
    if (s.address !== null && s.address !== undefined) {
      addressParts(s.address);
      if (addresses.has(s.address)) throw new Error("Duplicate display address");
      addresses.add(s.address);
    }
  }
  validateRectangles(seat.screens);
  return seat;
}
