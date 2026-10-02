import {fileURLToPath} from 'node:url';
import {bothNames} from './env.mjs';

// Bridge startup only needs process arguments, not the tool implementations.
export const SERVER_NAME='ac';
export function codexMcpArgs(cwd,environment={}) {
  return Object.entries(mcpConfig(cwd,environment).mcpServers).flatMap(([name,config])=>[
    '-c',`mcp_servers.${name}.command=${JSON.stringify(config.command)}`,
    '-c',`mcp_servers.${name}.args=${JSON.stringify(config.args)}`,
    ...Object.entries(config.env||{}).flatMap(([key,value])=>['-c',`mcp_servers.${name}.env.${key}=${JSON.stringify(value)}`]),
  ]);
}
export function mcpConfig(cwd,environment={}) {
  const env={...(process.versions.electron?{ELECTRON_RUN_AS_NODE:'1'}:{}),
    ...bothNames({AESEL_HARNESS_SOCKET:environment.AESEL_HARNESS_SOCKET}),
    ...(environment.AESEL_NATIVE_SESSION?{AESEL_NATIVE_SESSION:environment.AESEL_NATIVE_SESSION}:{})};
  const server=file=>({command:process.execPath,...(Object.keys(env).length?{env}:{}),
    args:[fileURLToPath(new URL(file,import.meta.url)),'--cwd',cwd]});
  return {mcpServers:{'easel-media':server('./media-mcp.mjs'),[SERVER_NAME]:server('./tools.mjs')}};
}
