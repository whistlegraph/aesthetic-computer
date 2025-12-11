#!/usr/bin/env node
/**
 * 🧠 Emacs MCP Server - Simple MCP server for Emacs integration
 * Runs via stdio and provides tools to execute elisp via emacsclient
 * 
 * TODO: Add ability to highlight/color a buffer's background when being
 *       observed/watched by an AI agent. Could add tools like:
 *       - emacs_watch_buffer(buffer, color) - set background highlight
 *       - emacs_unwatch_buffer(buffer) - restore normal background
 *       This would give visual feedback when Copilot is monitoring logs.
 */

import { spawn } from 'child_process';
import * as readline from 'readline';

const EMACSCLIENT = process.env.EMACSCLIENT || '/usr/sbin/emacsclient';

// MCP Server state
let messageId = 0;

// Execute elisp via emacsclient
async function execEmacs(code) {
  return new Promise((resolve, reject) => {
    const proc = spawn(EMACSCLIENT, ['--eval', code]);
    let stdout = '';
    let stderr = '';
    
    proc.stdout.on('data', (data) => { stdout += data.toString(); });
    proc.stderr.on('data', (data) => { stderr += data.toString(); });
    
    proc.on('close', (exitCode) => {
      if (exitCode === 0) {
        resolve(stdout.trim());
      } else {
        reject(new Error(stderr.trim() || `emacsclient exited with code ${exitCode}`));
      }
    });
    
    proc.on('error', (err) => {
      reject(new Error(`Failed to start emacsclient: ${err.message}`));
    });
  });
}

// Handle MCP JSON-RPC messages
async function handleMessage(message) {
  const { id, method, params } = message;
  
  try {
    switch (method) {
      case 'initialize':
        return {
          jsonrpc: '2.0',
          id,
          result: {
            protocolVersion: '2024-11-05',
            capabilities: {
              tools: {}
            },
            serverInfo: {
              name: 'emacs-mcp',
              version: '1.0.0'
            }
          }
        };
        
      case 'initialized':
        // No response needed for notification
        return null;
        
      case 'tools/list':
        return {
          jsonrpc: '2.0',
          id,
          result: {
            tools: [
              {
                name: 'execute_emacs_lisp',
                description: 'Execute Emacs Lisp code in the running Emacs instance via emacsclient',
                inputSchema: {
                  type: 'object',
                  properties: {
                    code: {
                      type: 'string',
                      description: 'The Emacs Lisp code to execute'
                    }
                  },
                  required: ['code']
                }
              },
              {
                name: 'emacs_list_buffers',
                description: 'List all open buffers in Emacs',
                inputSchema: {
                  type: 'object',
                  properties: {}
                }
              },
              {
                name: 'emacs_switch_buffer',
                description: 'Switch to a specific buffer in Emacs',
                inputSchema: {
                  type: 'object',
                  properties: {
                    buffer: {
                      type: 'string',
                      description: 'The name of the buffer to switch to'
                    }
                  },
                  required: ['buffer']
                }
              },
              {
                name: 'emacs_send_keys',
                description: 'Send keystrokes to the current Emacs buffer (useful for eat terminals)',
                inputSchema: {
                  type: 'object',
                  properties: {
                    buffer: {
                      type: 'string',
                      description: 'The buffer name to send keys to'
                    },
                    keys: {
                      type: 'string',
                      description: 'The keys/text to send'
                    }
                  },
                  required: ['buffer', 'keys']
                }
              },
              {
                name: 'emacs_get_buffer_content',
                description: 'Get the content of an Emacs buffer',
                inputSchema: {
                  type: 'object',
                  properties: {
                    buffer: {
                      type: 'string',
                      description: 'The buffer name'
                    },
                    maxChars: {
                      type: 'number',
                      description: 'Maximum characters to return (default 2000)'
                    }
                  },
                  required: ['buffer']
                }
              }
            ]
          }
        };
        
      case 'tools/call':
        const { name, arguments: args } = params;
        let result;
        
        switch (name) {
          case 'execute_emacs_lisp':
            result = await execEmacs(args.code);
            break;
            
          case 'emacs_list_buffers':
            result = await execEmacs('(mapcar #\'buffer-name (buffer-list))');
            break;
            
          case 'emacs_switch_buffer':
            result = await execEmacs(`(switch-to-buffer "${args.buffer}")`);
            break;
            
          case 'emacs_send_keys':
            // Send keys to an eat terminal buffer
            const escapedKeys = args.keys.replace(/\\/g, '\\\\').replace(/"/g, '\\"');
            result = await execEmacs(`
              (with-current-buffer "${args.buffer}"
                (if (and (boundp 'eat-terminal) eat-terminal)
                    (progn
                      (eat-term-send-string eat-terminal "${escapedKeys}")
                      "Keys sent to eat terminal")
                  (progn
                    (insert "${escapedKeys}")
                    "Text inserted into buffer")))
            `);
            break;
            
          case 'emacs_get_buffer_content':
            const maxChars = args.maxChars || 2000;
            result = await execEmacs(`
              (with-current-buffer "${args.buffer}"
                (buffer-substring-no-properties 
                  (point-min) 
                  (min (point-max) (+ (point-min) ${maxChars}))))
            `);
            break;
            
          default:
            throw new Error(`Unknown tool: ${name}`);
        }
        
        return {
          jsonrpc: '2.0',
          id,
          result: {
            content: [
              {
                type: 'text',
                text: result
              }
            ]
          }
        };
        
      default:
        // Unknown method
        return {
          jsonrpc: '2.0',
          id,
          error: {
            code: -32601,
            message: `Method not found: ${method}`
          }
        };
    }
  } catch (error) {
    return {
      jsonrpc: '2.0',
      id,
      error: {
        code: -32000,
        message: error.message
      }
    };
  }
}

// Main: read JSON-RPC from stdin, write to stdout
const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
  terminal: false
});

rl.on('line', async (line) => {
  try {
    const message = JSON.parse(line);
    const response = await handleMessage(message);
    if (response) {
      console.log(JSON.stringify(response));
    }
  } catch (e) {
    console.error(JSON.stringify({
      jsonrpc: '2.0',
      id: null,
      error: {
        code: -32700,
        message: `Parse error: ${e.message}`
      }
    }));
  }
});

// Log to stderr for debugging (won't interfere with JSON-RPC on stdout)
console.error('🧠 Emacs MCP Server started');
