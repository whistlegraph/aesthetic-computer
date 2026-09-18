// Relay each provider chunk as soon as the consumer can accept it. Cancellation
// travels back to fetch instead of leaving a paid generation running unseen.
export function relayInference(body, { onUsage = () => {}, abort = () => {} } = {}) {
  const reader = body.getReader();
  const decoder = new TextDecoder();
  let tail = "";
  let spent = 0;
  let accumulated = {};
  let finished = false;
  function finish() {
    if (finished) return;
    finished = true;
    Promise.resolve().then(() => onUsage(spent)).catch(() => {});
    reader.releaseLock();
  }
  function meter(bytes) {
    tail += decoder.decode(bytes, { stream: true });
    let cut;
    while ((cut = tail.indexOf("\n")) !== -1) {
      const line = tail.slice(0, cut).trim();
      tail = tail.slice(cut + 1);
      if (!line.startsWith("data:")) continue;
      try {
        const json = JSON.parse(line.slice(5).trimStart());
        const usage = json?.usage || json?.message?.usage;
        if (usage) {
          accumulated = { ...accumulated, ...usage };
          spent = (accumulated.input_tokens || 0) + (accumulated.output_tokens || 0)
            + Math.round((accumulated.cache_read_input_tokens || 0) * 0.1)
            + Math.round((accumulated.cache_creation_input_tokens || 0) * 1.25);
        }
      } catch {}
    }
    // A malformed provider must not accumulate an unbounded unterminated line.
    if (tail.length > 1_048_576) tail = "";
  }
  return new ReadableStream({
    async pull(controller) {
      try {
        const { done, value } = await reader.read();
        if (finished) return;
        if (done) { controller.close(); finish(); return; }
        meter(value);
        controller.enqueue(value);
      } catch (error) {
        if (!finished) { controller.error(error); finish(); }
      }
    },
    async cancel(reason) {
      abort(reason);
      try { await reader.cancel(reason); } finally { finish(); }
    },
  });
}
