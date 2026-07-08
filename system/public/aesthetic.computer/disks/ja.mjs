// ja, 2026.07.07
// Shortcut for `japanese` - translate any language to Japanese.

export function boot({ colon, params, alias }) {
  alias(`japanese`, colon, params);
}

export { meta } from "../disks/japanese.mjs";
