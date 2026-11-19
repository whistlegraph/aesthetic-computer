// Dool, 2026.11.16
// The old address of rax (renamed 2026-11-18): aesthetic.computer/dool~room
// still drops into that room.

export function boot({ params, jump }) {
  jump(`prompt~${["rax", ...params.slice(0, 1)].join(" ")}~!autorun`);
}

export function meta() {
  return { title: "rax", desc: "The world under the prompt." };
}
