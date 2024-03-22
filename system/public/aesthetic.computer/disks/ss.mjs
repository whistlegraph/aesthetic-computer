// ss, 24.03.15.10.40 
// This piece is a meta-router / shortcut for "screenshots".

export function boot({ colon, params, alias }) {
  alias(`screenshots`, colon, params);
}

export { meta } from "../disks/screenshots.mjs";