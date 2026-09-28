// Fetch-only client for Slab's stateless JSON-over-HTTP MCP endpoints.
// Usable from a trusted Aesel host adapter or Node; no vendor CLI or SDK needed.
// This is not a general MCP client (no SSE, OAuth, server sessions, or stdio).
export const COMPUTER_USE_SERVERS = Object.freeze({
  frame: "http://127.0.0.1:7767/mcp",
  puppet: "http://127.0.0.1:7769/mcp",
});

export function createComputerUseClient({
  servers = COMPUTER_USE_SERVERS,
  allowedTools = [],
  fetch: fetchImpl = globalThis.fetch,
  timeoutMs = 45000,
  sessionId = globalThis.crypto.randomUUID(),
} = {}) {
  const allowed = new Set(allowedTools);
  let nextId = 0;
  let routes = new Map();
  let connecting;
  const versions = new Map();
  async function rpc(server, method, params, signal) {
    const endpoint = servers[server];
    if (!endpoint) throw new Error(`Unknown computer-use server: ${server}`);
    const controller = new AbortController();
    const abort = () => controller.abort(signal.reason);
    if (signal?.aborted) abort();
    else signal?.addEventListener("abort", abort, { once: true });
    const timer = setTimeout(() => controller.abort(new Error(`Computer-use ${method} timed out; outcome may be unknown`)), timeoutMs);
    const notification = method.startsWith("notifications/");
    const id = notification ? undefined : ++nextId;
    try {
      const response = await fetchImpl(endpoint, {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
          Accept: "application/json, text/event-stream",
          "X-Slab-Observation-Session": sessionId,
          ...(versions.has(server) ? { "MCP-Protocol-Version": versions.get(server) } : {}),
        },
        body: JSON.stringify({ jsonrpc: "2.0", id, method, params }),
        signal: controller.signal,
        redirect: "error",
      });
      if (!response.ok) throw new Error(`${server}: HTTP ${response.status}`);
      if (notification) return;
      const message = await response.json();
      if (message.jsonrpc !== "2.0" || message.id !== id) throw new Error(`${server}: mismatched RPC response`);
      if (message.error) throw new Error(`${server}: ${message.error.message}`);
      if (!("result" in message)) throw new Error(`${server}: missing RPC result`);
      return message.result;
    } finally {
      clearTimeout(timer);
      signal?.removeEventListener("abort", abort);
    }
  }
  async function discover() {
    if (connecting) return connecting;
    connecting = (async () => {
      const catalogs = await Promise.all(Object.keys(servers).map(async server => {
        const info = await rpc(server, "initialize", {
          protocolVersion: "2024-11-05", capabilities: {},
          clientInfo: { name: "slab-computer-use", version: "1.0.0" },
        });
        if (info.protocolVersion !== "2024-11-05") throw new Error(`${server}: unsupported protocol ${info.protocolVersion}`);
        versions.set(server, info.protocolVersion);
        await rpc(server, "notifications/initialized");
        const { tools } = await rpc(server, "tools/list");
        return { server, info, tools };
      }));
      const found = new Map();
      const tools = [];
      for (const catalog of catalogs) for (const tool of catalog.tools) {
        if (!allowed.has(tool.name)) continue;
        if (found.has(tool.name)) throw new Error(`Ambiguous tool: ${tool.name}`);
        found.set(tool.name, catalog.server);
        tools.push(tool);
      }
      routes = found;
      return { tools, servers: catalogs.map(({server,info}) => ({name:server,...info})) };
    })();
    try { return await connecting; }
    catch (error) { connecting = undefined; throw error; }
  }
  return {
    discover,
    async call(name, args = {}, { signal } = {}) {
      if (!allowed.has(name)) throw new Error(`Tool not enabled: ${name}`);
      if (signal?.aborted) throw signal.reason || new Error("Computer-use call cancelled");
      await discover();
      const server = routes.get(name);
      if (!server) throw new Error(`Tool not available: ${name}`);
      // Do not retry an action after a lost response. Preserve image blocks,
      // structuredContent, and isError; the consumer owns model/UI formatting.
      return rpc(server, "tools/call", { name, arguments: args }, signal);
    },
  };
}
