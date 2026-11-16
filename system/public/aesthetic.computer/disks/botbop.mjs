// Botbop, 2026.11.16
// The address of dool's side-on arena: aesthetic.computer/botbop opens it and
// botbop~name a room of its own. The prompt runs `botbop` (prompt.mjs), which
// drops the paper into dool there (lib/dool-sheet.mjs, duel/bop.mjs).

export function boot({ params, jump }) {
  // Everything after `prompt~` is one prompt line: its words take spaces.
  jump(`prompt~${["botbop", ...params.slice(0, 1)].join(" ")}~!autorun`);
}

export function meta() {
  return {
    title: "botbop",
    desc: "Land on a head. Bop the drones and each other in dool's side-on arena.",
  };
}
