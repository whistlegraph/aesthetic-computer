// da, 2026.01.19
// Shortcut for `danish` - translate any language to Danish.

export function boot({ colon, params, alias }) {
  alias(`danish`, colon, params);
}

export { meta } from "../disks/danish.mjs";
