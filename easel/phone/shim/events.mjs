// The slice of `node:events` that `ac-server.mjs` actually uses.
//
// It extends EventEmitter and calls `emit`. Nothing subscribes twice, nothing
// removes a listener mid-emit, and there is no `once`. So this is the whole of
// it — a Map of arrays — rather than a port of Node's class, which carries
// captureRejections, error-event throwing and max-listener warnings that would
// be dead weight in a webview.

export class EventEmitter {
  #listeners = new Map();

  on(name, fn) {
    const list = this.#listeners.get(name);
    if (list) list.push(fn);
    else this.#listeners.set(name, [fn]);
    return this;
  }

  off(name, fn) {
    const list = this.#listeners.get(name);
    if (!list) return this;
    const at = list.indexOf(fn);
    if (at !== -1) list.splice(at, 1);
    return this;
  }

  once(name, fn) {
    const wrapped = (...args) => {
      this.off(name, wrapped);
      fn(...args);
    };
    return this.on(name, wrapped);
  }

  // Copied before iterating: a listener that unsubscribes itself must not
  // shorten the list we are walking.
  emit(name, ...args) {
    const list = this.#listeners.get(name);
    if (!list?.length) return false;
    for (const fn of [...list]) {
      try {
        fn(...args);
      } catch (error) {
        console.error(`[easel] listener for ${name} threw`, error);
      }
    }
    return true;
  }

  removeAllListeners(name) {
    if (name === undefined) this.#listeners.clear();
    else this.#listeners.delete(name);
    return this;
  }
}

export default { EventEmitter };
