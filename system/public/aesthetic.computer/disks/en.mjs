// en, 2026.01.19
// Shortcut for `english` - translate any language to English.

export function boot({ colon, params, alias }) {
  alias(`english`, colon, params);
}

export { meta } from "../disks/english.mjs";
