# VS Code Chrome DevTools Protocol MCP Integration

**Date:** November 24, 2025  
**Status:** ✅ Successfully Implemented

## Overview

This document details the successful integration of Chrome DevTools Protocol (CDP) with VS Code's Electron instance, enabling MCP (Model Context Protocol) servers running in dev containers to inspect and debug VS Code webviews, including the Aesthetic Computer extension.

## The Ouroboros Effect

This integration creates a self-referential debugging loop where:
- **VS Code** runs the dev container
- **The dev container** runs an LLM (via GitHub Copilot)
- **The LLM** can now inspect and debug VS Code itself through CDP
- **VS Code extensions** (like Aesthetic Computer) become inspectable by the AI

The snake eats its own tail - the editor debugging itself through AI assistance.

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│  Host Machine (macOS/Linux)                                 │
│  ┌──────────────────────────────────────────────────────┐   │
│  │  VS Code (Electron)                                  │   │
│  │  • Launched with --remote-debugging-port=9222        │   │
│  │  • Launched with --remote-allow-origins="*"          │   │
│  │  • Exposes Chrome DevTools Protocol on localhost     │   │
│  │  ┌────────────────────────────────────────────────┐  │   │
│  │  │  Aesthetic Computer Extension (Webview)        │  │   │
│  │  │  https://localhost:8888/piece                  │  │   │
│  │  └────────────────────────────────────────────────┘  │   │
│  └──────────────────────────────────────────────────────┘   │
│           │                                                  │
│           │ Port 9222 (Chrome DevTools Protocol)            │
│           ▼                                                  │
└───────────┼──────────────────────────────────────────────────┘
            │
            │ host.docker.internal:9222
            │
┌───────────▼──────────────────────────────────────────────────┐
│  Dev Container (Fedora Linux)                                │
│  ┌────────────────────────────────────────────────────────┐  │
│  │  MCP Server (chrome-devtools-mcp)                      │  │
│  │  • Connects to host.docker.internal:9222               │  │
│  │  • Sends Host: localhost header                        │  │
│  │  • Can inspect all VS Code targets                     │  │
│  └────────────────────────────────────────────────────────┘  │
│           │                                                   │
│           ▼                                                   │
│  ┌────────────────────────────────────────────────────────┐  │
│  │  GitHub Copilot / LLM                                  │  │
│  │  • Uses MCP to access CDP                              │  │
│  │  • Can inspect webviews, console, DOM                  │  │
│  │  • Can debug extension code                            │  │
│  └────────────────────────────────────────────────────────┘  │
└──────────────────────────────────────────────────────────────┘
```

## Implementation Details

### 1. Host Configuration

#### macOS (`dotfiles/dot_config/fish_macos/config.fish`)
```fish
function start
  # Set up the ssl certificate.
  sudo ac-ssl

  # Kill any existing clipboard listener
  pkill -f "nc -l 12345"
  sleep 0.2

  # Start clipboard listener in background
  fish -c 'while true; nc -l 12345 | pbcopy; end' &

  # Quit VS Code if it's running
  osascript -e 'tell application "Visual Studio Code" to quit'
  sleep 1

  # Launch VS Code with Chrome DevTools Protocol enabled on port 9222
  # Allow remote origins so dev container can connect via host.docker.internal
  open -a "Visual Studio Code" --args --remote-debugging-port=9222 --remote-allow-origins="*"
end
```

#### Fedora Linux (`dotfiles/dot_config/fish/config.fish`)
```fish
function acd
    ac
    set containers (docker ps -q)
    if test -n "$containers"
        docker stop $containers
    end
    set container_id (pwd | tr -d '\n' | xxd -c 256 -p)
    set workspace_name (basename (pwd))
    # Launch VS Code with Chrome DevTools Protocol enabled on port 9222
    # Allow remote origins so dev container can connect via host.docker.internal
    code --remote-debugging-port=9222 --remote-allow-origins="*" --folder-uri="vscode-remote://dev-container+$container_id/workspaces/$workspace_name"
    cd -
end
```

### 2. MCP Configuration

**`.vscode/mcp.json`**
```json
{
  "mcpServers": {
    "chrome-devtools": {
      "command": "npx",
      "args": [
        "-y",
        "chrome-devtools-mcp@latest",
        "--browserUrl=http://host.docker.internal:9222"
      ]
    }
  }
}
```

### 3. Security Considerations

**Host Header Requirement:**
- VS Code's CDP endpoint requires `Host: localhost` header for security
- Connections via `host.docker.internal` are rejected without this header
- The `chrome-devtools-mcp` library handles this automatically
- Manual `curl` commands need `-H "Host: localhost"` flag

**Remote Origins:**
- `--remote-allow-origins="*"` allows dev container to connect
- Without this flag, cross-origin connections are blocked
- This is necessary because `host.docker.internal` is not `localhost`

## Available Debugging Targets

When VS Code is running with the Aesthetic Computer extension active, the following targets are available:

1. **VS Code Workbench** (`type: page`)
   - The main VS Code window
   - Access to VS Code's UI and state

2. **Aesthetic Computer Extension Webview** (`type: iframe`)
   - The webview hosting the Aesthetic Computer piece
   - URL: `https://localhost:8888/piece_name`
   - Full access to JavaScript runtime, DOM, console

3. **Extension Service Workers** (`type: service_worker`)
   - Background tasks for the extension
   - WebView service worker

4. **Extension Host Workers** (`type: worker`)
   - VS Code extension host processes
   - Extension execution context

## Testing the Connection

```bash
# From dev container:
curl -s -H "Host: localhost" http://host.docker.internal:9222/json | jq '.[] | {id, title, type, url}'
```

Expected output includes:
- VS Code workbench page
- Aesthetic Computer webview iframe
- Extension workers

## Use Cases

### 1. Real-time Debugging
- Inspect live state of Aesthetic Computer pieces
- Access browser console from within VS Code
- Monitor network requests
- Profile performance

### 2. AI-Assisted Development
- LLM can inspect actual running code
- Debug based on real runtime state
- Generate fixes based on console errors
- Understand live DOM structure

### 3. Extension Development
- Debug VS Code extensions programmatically
- Inspect webview behavior
- Monitor extension lifecycle
- Test extension interactions

## Limitations

1. **Chrome/Chromium Only**: The MCP is designed for Chrome DevTools Protocol
2. **Host Dependency**: Requires VS Code to be launched with specific flags
3. **Network Access**: Dev container must be able to reach host network
4. **Security**: `--remote-allow-origins="*"` reduces security (acceptable for dev environments)

## Future Enhancements

1. **Selective Origin Allowlist**: Replace `"*"` with specific allowed origins
2. **Automatic Flag Detection**: Detect if VS Code was launched without flags and restart
3. **Multi-Target Management**: Better UX for switching between multiple debugging targets
4. **Extension-Specific Tools**: Custom MCP tools for Aesthetic Computer piece introspection
5. **Live Reloading**: Auto-reconnect when VS Code restarts

## References

- [Chrome DevTools Protocol](https://chromedevtools.github.io/devtools-protocol/)
- [Electron Remote Debugging](https://www.electronjs.org/docs/latest/tutorial/debugging-main-process)
- [VS Code Extension API](https://code.visualstudio.com/api)
- [Model Context Protocol (MCP)](https://modelcontextprotocol.io/)

## Conclusion

This integration successfully bridges the gap between AI assistants and live IDE state, creating a powerful feedback loop for development. The LLM can now "see" what's actually happening in the editor, not just read static code files.

The ouroboros is complete - the editor can now debug itself through AI assistance. 🐍✨
