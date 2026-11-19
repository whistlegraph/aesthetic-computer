// Rax, 2026.11.16
// An address into rax: aesthetic.computer/rax~room drops into that room.
// The prompt runs `rax <room>` (prompt.mjs); bare /rax is lith's own page.

export function boot({ params, jump }) {
  // Everything after `prompt~` is one prompt line: its words take spaces.
  jump(`prompt~${["rax", ...params.slice(0, 1)].join(" ")}~!autorun`);
}

export function meta() {
  return { title: "rax", desc: "The world under the prompt." };
}
