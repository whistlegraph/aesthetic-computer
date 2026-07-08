// zh, 2026.07.07
// Shortcut for `chinese` - translate any language to Chinese.

export function boot({ colon, params, alias }) {
  alias(`chinese`, colon, params);
}

export { meta } from "../disks/chinese.mjs";
