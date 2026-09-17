// Optional trusted-host adapter. Importing it grants no tools or screen access.
// The Aesel host selects the machine, page, and allowlist; model arguments may
// not redirect an operation to another machine or browser page.
import { createComputerUseClient } from "../../slab/lib/computer-use-client.mjs";

export function createAeselComputerUse({ machine, target, allowedTools = [], ...transport } = {}) {
  if (typeof machine !== "string" || !machine) throw new Error("Aesel computer use needs an explicit machine");
  const browserTools = allowedTools.some(name => name.startsWith("puppet_") && !["puppet_list", "puppet_term", "puppet_type", "puppet_keys"].includes(name));
  if (browserTools && (typeof target !== "string" || !target)) throw new Error("Aesel browser tools need an explicit target");
  const client = createComputerUseClient({ ...transport, allowedTools });
  return {
    async discover() {
      const catalog = await client.discover();
      return {
        ...catalog,
        tools: catalog.tools.map(tool => {
          const { machine: _machine, target: _target, sessionId: _sessionId, ...properties } = tool.inputSchema.properties || {};
          return { ...tool, inputSchema: { ...tool.inputSchema, properties,
            required: (tool.inputSchema.required || []).filter(key => key !== "machine" && key !== "target" && key !== "sessionId") } };
        }),
      };
    },
    call(name, args = {}, options) {
      if (Object.hasOwn(args, "machine") || Object.hasOwn(args, "target") || Object.hasOwn(args, "sessionId")) {
        throw new Error("Machine, target, and observation session are fixed by the Aesel host");
      }
      return client.call(name, { ...args, machine, ...(target ? { target } : {}) }, options);
    },
  };
}
