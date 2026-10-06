const aiRequest = input => {
  let url;
  try { url = new URL(typeof input === 'string' || input instanceof URL ? input : input.url,
    globalThis.location?.href || 'https://aesthetic.computer'); } catch { return false; }
  if (url.origin === 'https://aesthetic.computer') return ['/api/easel-inference', '/api/easel-musical-jev'].includes(url.pathname);
  if (url.origin === 'https://help.aesthetic.computer' && url.pathname.startsWith('/api/aesel/sessions')) {
    // Revocation must still be able to cancel an already-started remote turn.
    return !/^\/api\/aesel\/sessions\/[^/]+\/interrupt$/.test(url.pathname);
  }
  return false;
};

export function createAIConsentGate({fetch, allowed = false, onRequired = () => {}}) {
  let permitted = allowed === true;
  const active = new Set();
  function requireConsent() {
    if (permitted) return;
    onRequired();
    throw Error('Allow AI creation in AI & privacy before sending this request.');
  }
  return {
    get allowed() { return permitted; },
    require: requireConsent,
    setAllowed(value) {
      permitted = value === true;
      if (!permitted) { for (const controller of active) controller.abort(); active.clear(); }
    },
    async fetch(input, init = {}) {
      if (!aiRequest(input)) return fetch(input, init);
      requireConsent();
      const controller = new AbortController();
      const upstream = init.signal || (typeof input === 'object' ? input.signal : null);
      const abort = () => controller.abort(upstream?.reason);
      const assertActive = () => {
        if (controller.signal.aborted) throw controller.signal.reason;
      };
      let reader, output;
      const cleanup = () => {
        active.delete(controller);
        upstream?.removeEventListener('abort', abort);
        controller.signal.removeEventListener('abort', abortStream);
      };
      const abortStream = () => {
        output?.error(controller.signal.reason);
        void reader?.cancel(controller.signal.reason).catch(() => {});
        cleanup();
      };
      controller.signal.addEventListener('abort', abortStream, {once: true});
      if (upstream?.aborted) abort(); else upstream?.addEventListener('abort', abort, {once: true});
      try {
        assertActive();
        active.add(controller);
        const response = await fetch(input, {...init, signal: controller.signal});
        if (controller.signal.aborted) await response.body?.cancel(controller.signal.reason).catch(() => {});
        assertActive();
        if (!response.body) { cleanup(); return response; }
        reader = response.body.getReader();
        const body = new ReadableStream({
          start(stream) { output = stream; },
          async pull(stream) {
            try {
              assertActive();
              const result = await reader.read();
              assertActive();
              if (result.done) { cleanup(); stream.close(); } else stream.enqueue(result.value);
            } catch (error) { cleanup(); stream.error(error); }
          },
          async cancel(reason) { cleanup(); controller.abort(); await reader.cancel(reason); },
        });
        return new Response(body, {status: response.status, statusText: response.statusText, headers: response.headers});
      } catch (error) { cleanup(); throw error; }
    },
  };
}
