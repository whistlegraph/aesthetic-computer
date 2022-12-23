import { openDB } from "../dep/idb.js";

const dbPromise = openDB("keyval-store", 1, {
  upgrade(db) {
    db.createObjectStore("keyval");
  },
});

async function get(key) {
  return (await dbPromise).get("keyval", key);
}

async function set(key, val) {
  return (await dbPromise).put("keyval", val, key);
}

async function del(key) {
  return (await dbPromise).delete("keyval", key);
}

async function clear() {
  return (await dbPromise).clear("keyval");
}

async function keys() {
  return (await dbPromise).getAllKeys("keyval");
}

export { get, set, del, clear, keys };
