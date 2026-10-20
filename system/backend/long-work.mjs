// Long work lith must not cut off on a restart: a paid image move can run for
// minutes, far past the server's connection drain. Handlers register that
// work here; on SIGTERM lith keeps serving until it settles (lith/server.mjs),
// and handlers turn new work away while it does, so the client retries
// against the fresh process instead of starting something that will be cut.
// The state hangs off globalThis so every import path shares one registry.
const state = (globalThis.__acLongWork ||= { work: new Set(), draining: false });

export function trackLongWork(promise) {
  state.work.add(promise);
  const done = () => state.work.delete(promise);
  promise.then(done, done);
  return promise;
}

export const draining = () => state.draining;

// Resolves once every registered piece of work has settled, or after `limit`
// milliseconds, whichever comes first. Returns how many were still running.
export async function drainLongWork(limit) {
  state.draining = true;
  const deadline = Date.now() + limit;
  while (state.work.size && Date.now() < deadline) {
    const timer = new Promise((resolve) => setTimeout(resolve, Math.min(1000, deadline - Date.now())));
    await Promise.race([timer, ...[...state.work].map((work) => work.catch(() => {}))]);
  }
  return state.work.size;
}
