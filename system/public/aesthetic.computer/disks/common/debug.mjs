// A debug constant that can be imported by supporting piece library code, which gets set in `disk.mjs`.

export let DEBUG = false;

// Set in `disk.mjs` on first load.
export function setDebug(bool) {
  DEBUG = bool;
}
