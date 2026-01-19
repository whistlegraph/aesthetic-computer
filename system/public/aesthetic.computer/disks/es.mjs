// es, 2026.01.19
// Shortcut for `spanish` - translate any language to Spanish.

export function boot({ colon, params, alias }) {
  alias(`spanish`, colon, params);
}

export { meta } from "../disks/spanish.mjs";
