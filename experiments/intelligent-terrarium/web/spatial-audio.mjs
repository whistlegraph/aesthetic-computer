export function spatialize(source, listener, radius = 12) {
  const dx = Number(source[0]) - Number(listener.x || 0);
  const dy = Number(source[1]) - Number(listener.y || 0);
  const dz = Number(source[2]) - Number(listener.z || 0);
  const yaw = Number(listener.yaw || 0);
  const right = dx * Math.cos(yaw) - dz * Math.sin(yaw);
  const distance = Math.hypot(dx, dy, dz);
  const pan = Math.max(-1, Math.min(1, right / Math.max(1, Math.hypot(dx, dz))));
  const gain = Math.max(0, Math.min(1, 1 - distance / Math.max(0.001, radius)));
  return { pan, gain, distance };
}

export class SonicDeduper {
  constructor(limit = 256) {
    this.limit = limit;
    this.ids = new Set();
  }

  accept(id) {
    if (this.ids.has(id)) return false;
    this.ids.add(id);
    if (this.ids.size > this.limit) this.ids.delete(this.ids.values().next().value);
    return true;
  }
}
