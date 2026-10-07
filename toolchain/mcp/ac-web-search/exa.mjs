// Exa is an adapter behind AC's stable search/fetch interface.
const ENDPOINT = "https://mcp.exa.ai/mcp?tools=web_search_exa,web_fetch_exa";
const MAX_RESPONSE = 1_000_000;

export async function readRpc(response, id) {
  if (!response.ok) {
    await response.body?.cancel();
    throw new Error(response.status === 429
      ? "Exa rate limit reached; wait or configure your own EXA_API_KEY."
      : `Exa returned HTTP ${response.status}.`);
  }
  const reader = response.body.getReader();
  const decoder = new TextDecoder();
  const sse = response.headers.get("content-type")?.includes("text/event-stream");
  let buffer = "", bytes = 0;
  try {
    while (true) {
      const { value, done } = await reader.read();
      bytes += value?.byteLength || 0;
      if (bytes > MAX_RESPONSE) throw new Error("Exa response exceeded the size limit.");
      buffer += decoder.decode(value, { stream: !done });
      if (sse) {
        let boundary;
        while ((boundary = /\r?\n\r?\n/.exec(buffer))) {
          const frame = buffer.slice(0, boundary.index);
          buffer = buffer.slice(boundary.index + boundary[0].length);
          const data = frame.split(/\r?\n/).filter(line => line.startsWith("data:"))
            .map(line => line.slice(5).trimStart()).join("\n");
          if (!data) continue;
          const message = JSON.parse(data);
          if (message.id === id) return message;
        }
      }
      if (done) break;
    }
    if (!sse) {
      const message = JSON.parse(buffer);
      if (message.id === id) return message;
    }
    throw new Error("Exa returned no matching response.");
  } finally {
    await reader.cancel().catch(() => {});
  }
}

export function createExa({ fetchImpl = fetch, apiKey = "", timeoutMs = 30000 } = {}) {
  let sequence = 0;
  async function call(name, args) {
    const id = ++sequence;
    let message;
    try {
      const response = await fetchImpl(ENDPOINT, {
        method: "POST",
        headers: { "content-type": "application/json", accept: "application/json, text/event-stream",
          ...(apiKey ? { "x-api-key": apiKey } : {}) },
        body: JSON.stringify({ jsonrpc: "2.0", id, method: "tools/call", params: { name, arguments: args } }),
        signal: AbortSignal.timeout(timeoutMs),
        redirect: "error",
      });
      message = await readRpc(response, id);
    } catch (error) {
      if (error.name === "TimeoutError" || error.name === "AbortError") throw new Error("Exa request timed out.");
      // Do not forward network internals, headers, or credentials.
      if (error.message.startsWith("Exa ")) throw error;
      throw new Error("Exa request failed.");
    }
    if (message.error) throw new Error("Exa rejected the search request.");
    if (!Array.isArray(message.result?.content)) throw new Error("Exa returned an invalid tool result.");
    let remaining = 80000;
    const content = message.result.content.filter(item => item.type === "text").map(item => {
      const value = apiKey ? item.text.split(apiKey).join("[redacted]") : item.text;
      const text = value.slice(0, remaining);
      remaining -= text.length;
      return { type: "text", text: text + (text.length < value.length ? "\n[Result truncated]" : "") };
    }).filter(item => item.text);
    return { content, ...(message.result.isError ? { isError: true } : {}) };
  }
  return {
    search: ({ query, objective, limit }) => call("web_search_exa", { query, objective, numResults: limit }),
    fetch: ({ urls, max_characters }) => call("web_fetch_exa", { urls, maxCharacters: max_characters }),
  };
}
