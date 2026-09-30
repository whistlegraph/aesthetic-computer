// Relay each provider chunk as soon as the consumer can accept it. Cancellation
// travels back to fetch instead of leaving a paid generation running unseen.
// `onUsage(tokens, usage)` fires once: weighted tokens, and the provider's own
// usage block, whose `cost` (USD) OpenRouter reports in the final message_delta.
export function relayInference(body, { onUsage = () => {}, abort = () => {}, onSettlementError = error => console.error("[aesel] usage settlement failed", error?.message || error), onFinish = () => {} } = {}) {
  const reader = body.getReader();
  const decoder = new TextDecoder();
  let tail = "";
  let spent = 0;
  let accumulated = {};
  let finished = false;
  let completion;
  function finish() {
    if (completion) return completion;
    finished = true;
    completion = Promise.resolve().then(() => onUsage(spent, accumulated)).catch(onSettlementError).finally(() => { reader.releaseLock(); onFinish(); });
    return completion;
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
        if (done) { await finish(); controller.close(); return; }
        meter(value);
        controller.enqueue(value);
      } catch (error) {
        if (!finished) { await finish(); controller.error(error); }
      }
    },
    async cancel(reason) {
      abort(reason);
      try { await reader.cancel(reason); } finally { await finish(); }
    },
  });
}
