// Dool, 2026.11.16
// An address into dool: aesthetic.computer/dool~room drops into that room.
// The prompt runs `dool <room>` (prompt.mjs); bare /dool is lith's own page.

export function boot({ params, jump }) {
  // Everything after `prompt~` is one prompt line: its words take spaces.
  jump(`prompt~${["dool", ...params.slice(0, 1)].join(" ")}~!autorun`);
}

export function meta() {
  return { title: "dool", desc: "The world under the prompt." };
}
