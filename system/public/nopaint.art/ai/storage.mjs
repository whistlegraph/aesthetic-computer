export async function openStore() {
  const db = await new Promise((resolve, reject) => {
    const request = indexedDB.open("nopaint-ai", 1);
    request.onupgradeneeded = () => {
      request.result.createObjectStore("paintings", { keyPath: "id" });
      request.result.createObjectStore("settings");
    };
    request.onsuccess = () => resolve(request.result);
    request.onerror = () => reject(request.error);
  });
  function query(table, method, ...args) {
    return new Promise((resolve, reject) => {
      const transaction = db.transaction(
        table,
        ["put", "delete"].includes(method) ? "readwrite" : "readonly",
      );
      const request = transaction.objectStore(table)[method](...args);
      transaction.oncomplete = () => resolve(request.result);
      transaction.onerror = () => reject(transaction.error);
      transaction.onabort = () => reject(transaction.error);
    });
  }
  return {
    get: (id) => query("paintings", "get", id),
    save: (track) => query("paintings", "put", track),
    all: () => query("paintings", "getAll"),
    setting: (key, value) =>
      value === undefined
        ? query("settings", "get", key)
        : query("settings", "put", value, key),
  };
}
