// ru, 2026.07.07
// Shortcut for `russian` - translate any language to Russian.

export function boot({ colon, params, alias }) {
  alias(`russian`, colon, params);
}

export { meta } from "../disks/russian.mjs";
