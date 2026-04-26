// login.mjs — legacy alias for link.mjs.
// `link` is the canonical command; this re-export preserves any cached
// references to the old `login` piece.
export { boot, paint, act, sim } from "./link.mjs";
