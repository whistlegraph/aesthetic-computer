import { openDB } from "../dep/idb.js";

/* #region 🏁 todo
 - [] Is the await dbPromise being everywhere / initialization actually correct?
#endregion */

let dbPromise;

// This is wrapped in a try because sometimes IDB is unavailable like
// in sandboxed iframes. 23.02.02.14.47
// const sandboxed = self === self.top;

try {
  dbPromise = openDB("keyval-store", 1, {
    upgrade(db) {
      db.createObjectStore("keyval");
    },
  });
} catch (err) {
  // console.warn(err);
}

async function get(key) {
  return (await dbPromise)?.get("keyval", key);
}

async function set(key, val) {
  return (await dbPromise)?.put("keyval", val, key);
}

async function del(key) {
  return (await dbPromise)?.delete("keyval", key);
}

async function clear() {
  return (await dbPromise)?.clear("keyval");
}

async function keys() {
  return (await dbPromise)?.getAllKeys("keyval");
}

export { get, set, del, clear, keys };
