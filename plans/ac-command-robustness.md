# AC Command Robustness Plan

**Date**: 2025-01-23  
**Author**: GitHub Copilot  
**Status**: Draft

## Problem Statement

### Current Behavior
The `ac` fish shell function sends HTTP POST requests to `https://localhost:8889/jump` to navigate the VSCode extension's webview panel to different pieces. The command works as expected when:
- The aesthetic computer extension panel is visible
- The local dev server (session-server) is running
- The websocket connection is established

However, it can fail silently when:
- The VSCode extension sidebar panel is hidden or closed
- The panel webview hasn't loaded yet
- The session server hasn't started
- The websocket connection isn't established

This creates a poor developer experience where running `ac gameboy~melody` from the terminal may appear to do nothing, with no feedback about why.

### Impact on Workflow
This issue affects the primary development workflow:
1. Developer edits Game Boy ROM code in `kidlisp-gameboy/`
2. Build script (`build.sh`) compiles the ROM successfully
3. Build script auto-runs `fish -c "ac gameboy~$OUTPUT_NAME"` 
4. **FAILURE POINT**: If panel not visible, message lost → developer sees no result
5. Developer must manually click sidebar icon, then re-run command

This breaks the smooth "edit → build → test" cycle that's critical for rapid iteration on ROMs.

---

## Current Implementation

### Message Flow Architecture

```
┌──────────────┐         ┌──────────────────┐         ┌─────────────┐
│ Fish Shell   │  HTTP   │ Session Server   │  WebSoc │ VSCode      │
│ ac function  │  POST   │ localhost:8889   │  ket    │ Webview     │
│              │────────>│ /jump endpoint   │────────>│ Panel       │
└──────────────┘         └──────────────────┘         └─────────────┘
```

### Component Analysis

#### 1. Fish Shell Function
**Location**: `.devcontainer/config.fish:848-857`

```fish
function ac --description 'cd to aesthetic-computer or jump to piece'
    if test (count $argv) -eq 0
        cd ~/aesthetic-computer
    else
        set piece_path $argv[1]
        echo "🎯 Jumping to: $piece_path"
        set response (curl -s -k -X POST https://localhost:8889/jump \
            -H "Content-Type: application/json" \
            -d "{\"piece\": \"$piece_path\"}")
        echo "$response"
    end
end
```

**Issues**:
- ❌ No panel visibility check before sending
- ❌ No retry logic if POST fails
- ❌ No validation of response
- ❌ Silent failure if panel not ready
- ✅ Simple and fast when working

#### 2. Session Server
**Location**: `session-server/session.mjs:225-235`

```javascript
// Jump to a specific piece (navigate)
fastify.post("/jump", async (req) => {
  const { piece } = req.body;
  everyone(pack("jump", { piece }, "pieces"));
  return { msg: "Jump request sent!", piece };
});
```

**Behavior**:
- Receives POST request
- Broadcasts `jump` message to all connected websocket clients
- Returns success regardless of whether clients received it
- No confirmation that panel actually navigated

#### 3. VSCode Extension Panel
**Location**: `vscode-extension/extension.ts:716-740, 742-920`

```typescript
class AestheticViewProvider implements vscode.WebviewViewProvider {
  public static readonly viewType = "aestheticComputer.sidebarView";
  private _view?: vscode.WebviewView;

  public sendMessageToWebview(message: any) {
    if (this._view && this._view.webview) {
      this._view.webview.postMessage(message);
    }
  }

  public refreshWebview(): void {
    if (this._view) {
      const slug = extContext.globalState.get("panel:slug", "");
      this._view.title = slug + (local ? " 🧑‍🤝‍🧑" : "");
      this._view.webview.html = getWebViewContent(this._view.webview, slug);
    }
  }

  public resolveWebviewView(
    webviewView: vscode.WebviewView,
    context: vscode.WebviewViewResolveContext<unknown>,
    _token: vscode.CancellationToken,
  ): void {
    this._view = webviewView;
    // ... initialization ...
    
    webviewView.webview.onDidReceiveMessage(async (data) => {
      // Handles messages from webview
    });

    webviewView.onDidChangeVisibility(() => {
      if (!webviewView.visible) {
        // Panel hidden - no messages received
      } else {
        // Panel visible - send focus event
        webviewView.webview.postMessage({ type: "aesthetic-parent:focused" });
      }
    });
  }
}
```

**Issues**:
- ❌ No method to show panel programmatically
- ❌ No way to check if panel is visible from fish
- ❌ No message queue for when panel becomes visible
- ✅ Well-structured for receiving messages when visible

---

## Proposed Solutions

### Option A: VSCode Command to Show Panel + Fish Wrapper

**Concept**: Create a VSCode command that shows the panel, then have fish call it before sending the jump message.

**Implementation**:

1. **Extension Side** - Add command to show panel:
```typescript
// In extension.ts activate() function
context.subscriptions.push(
  vscode.commands.registerCommand("aestheticComputer.showPanel", async () => {
    await vscode.commands.executeCommand(
      "workbench.view.extension.aestheticComputer"
    );
    // Wait for panel to initialize
    return new Promise((resolve) => setTimeout(resolve, 500));
  })
);
```

2. **Fish Side** - Update ac function:
```fish
function ac --description 'cd to aesthetic-computer or jump to piece'
    if test (count $argv) -eq 0
        cd ~/aesthetic-computer
    else
        set piece_path $argv[1]
        echo "🎯 Jumping to: $piece_path"
        
        # Show the panel first
        code --command aestheticComputer.showPanel
        sleep 0.5  # Give panel time to initialize
        
        # Then send the jump command
        set response (curl -s -k -X POST https://localhost:8889/jump \
            -H "Content-Type: application/json" \
            -d "{\"piece\": \"$piece_path\"}")
        echo "$response"
    end
end
```

**Pros**:
- ✅ Directly addresses root cause (hidden panel)
- ✅ Uses native VSCode APIs
- ✅ Panel always visible after command
- ✅ Works even if websocket not connected yet

**Cons**:
- ❌ Requires `code` CLI to be available
- ❌ May not work in all environments (Codespaces, SSH)
- ❌ Timing issues - hard to know when panel fully loaded
- ❌ Intrusive - forces panel to show even if user doesn't want it

### Option B: Retry Logic with Exponential Backoff

**Concept**: Keep fish simple, but add retry logic to handle transient failures.

**Implementation**:

```fish
function ac --description 'cd to aesthetic-computer or jump to piece'
    if test (count $argv) -eq 0
        cd ~/aesthetic-computer
    else
        set piece_path $argv[1]
        echo "🎯 Jumping to: $piece_path"
        
        # Retry up to 5 times with exponential backoff
        set max_retries 5
        set retry_count 0
        set success false
        
        while test $retry_count -lt $max_retries
            set response (curl -s -k -X POST https://localhost:8889/jump \
                -H "Content-Type: application/json" \
                -d "{\"piece\": \"$piece_path\"}")
            
            # Check if response indicates success
            if string match -q "*Jump request sent*" $response
                echo "$response"
                set success true
                break
            end
            
            set retry_count (math $retry_count + 1)
            if test $retry_count -lt $max_retries
                set delay (math "0.5 * (2 ^ $retry_count)")
                echo "⏳ Retrying in $delay seconds... (attempt $retry_count/$max_retries)"
                sleep $delay
            end
        end
        
        if not $success
            echo "❌ Failed to send jump command after $max_retries attempts"
            echo "💡 Try manually opening the Aesthetic Computer panel in VSCode"
            return 1
        end
    end
end
```

**Pros**:
- ✅ No changes to extension needed
- ✅ Handles server startup delay
- ✅ Handles temporary network issues
- ✅ Provides clear feedback to user

**Cons**:
- ❌ Doesn't solve hidden panel issue
- ❌ Wastes time retrying if panel will never be visible
- ❌ Can be slow (up to ~30 seconds with backoff)
- ❌ Still fails silently if panel hidden but server up

### Option C: Extension-Side Message Queue

**Concept**: Have the extension buffer jump messages when panel is hidden, then replay them when it becomes visible.

**Implementation**:

1. **Extension Side** - Add message queue:
```typescript
class AestheticViewProvider implements vscode.WebviewViewProvider {
  private _pendingMessages: any[] = [];
  
  public sendMessageToWebview(message: any) {
    if (this._view && this._view.webview && this._view.visible) {
      this._view.webview.postMessage(message);
    } else {
      // Queue message for later
      this._pendingMessages.push(message);
      console.log(`📬 Queued message (panel hidden): ${message.type || message}`);
    }
  }
  
  public resolveWebviewView(...) {
    // ... existing code ...
    
    webviewView.onDidChangeVisibility(() => {
      if (webviewView.visible) {
        // Panel now visible - send queued messages
        console.log(`📤 Panel visible, sending ${this._pendingMessages.length} queued messages`);
        this._pendingMessages.forEach(msg => {
          webviewView.webview.postMessage(msg);
        });
        this._pendingMessages = [];
        
        webviewView.webview.postMessage({ type: "aesthetic-parent:focused" });
      }
    });
  }
}
```

2. **Session Server Side** - Forward to extension:
```javascript
// Store reference to extension provider (would need to be set up)
let extensionProvider;

fastify.post("/jump", async (req) => {
  const { piece } = req.body;
  
  // Broadcast via websocket
  everyone(pack("jump", { piece }, "pieces"));
  
  // Also send directly to extension if available
  if (extensionProvider) {
    extensionProvider.sendMessageToWebview({
      type: "jump",
      piece: piece
    });
  }
  
  return { msg: "Jump request sent!", piece };
});
```

**Pros**:
- ✅ No changes to fish function needed
- ✅ Messages never lost - always queued
- ✅ Works automatically when panel opened
- ✅ Transparent to user

**Cons**:
- ❌ Complex to implement - requires IPC between session server and extension
- ❌ Queue could grow unbounded if panel never opened
- ❌ User may not realize panel needs to be opened
- ❌ Messages could arrive out of order if queue processes slowly

### Option D: Hybrid Approach

**Concept**: Combine panel visibility check with smart retry logic.

**Implementation**: Use `code --command` CLI to check panel visibility before sending.

**Pros**:
- ✅ Best effort panel visibility
- ✅ Fast retries (< 2 seconds total)

**Cons**:
- ❌ **`code --command` not available in devcontainer/Codespaces**
- ❌ Creates OS/environment dependencies
- ❌ Still requires retry logic
- ❌ Not a complete solution

**Status**: Ruled out - tested and confirmed `code --command` unavailable in devcontainer.

### Option E: Extension as Session Server Client (RECOMMENDED)

**Concept**: The VSCode extension connects directly to the session server as a WebSocket client, receives jump messages directly, and controls its own panel visibility.

**Architecture**:
```
Terminal:                 Session Server:              VSCode Extension:
┌─────────┐              ┌──────────────┐             ┌────────────────┐
│ ac cmd  │─── HTTP ────▶│  POST /jump  │             │  WebSocket     │
│         │     POST     │              │             │  Client        │
└─────────┘              │   Broadcast  │─── WS ─────▶│                │
                         │   to all     │   "vscode:  │  Show Panel    │
                         │   clients    │    jump"    │  + Navigate    │
                         │              │             │                │
Browser:                 │              │             └────────────────┘
┌─────────┐              │              │
│ Client  │◀─── WS ──────│              │
│         │   "jump"     │              │
└─────────┘              └──────────────┘
```

**Implementation**:

1. **Session Server** (`session-server/session.mjs`):
```javascript
const vscodeClients = new Set();

io.on("connection", (socket) => {
  socket.on("identify", (data) => {
    if (data.type === "vscode") {
      vscodeClients.add(socket);
      socket.on("disconnect", () => vscodeClients.delete(socket));
      console.log("✓ VSCode extension connected");
    }
  });
});

fastify.post("/jump", async (req) => {
  const { piece } = req.body;
  
  // Broadcast to browser clients
  everyone(pack("jump", { piece }, "pieces"));
  
  // Direct message to VSCode extension clients
  vscodeClients.forEach(client => {
    client.emit("vscode:jump", { piece });
  });
  
  return { 
    msg: "Jump request sent!", 
    piece,
    vscodeConnected: vscodeClients.size > 0 
  };
});
```

2. **Extension** (`vscode-extension/extension.ts`):
```typescript
import io from "socket.io-client";

class AestheticViewProvider {
  private socket?: any;
  
  async connectToSessionServer() {
    const url = local ? "https://localhost:8889" : "https://aesthetic.computer";
    this.socket = io(url, { 
      rejectUnauthorized: false,
      transports: ["websocket"] 
    });
    
    this.socket.on("connect", () => {
      console.log("Connected to session server");
      this.socket.emit("identify", { type: "vscode" });
    });
    
    this.socket.on("vscode:jump", async (data: { piece: string }) => {
      console.log("Received jump command:", data.piece);
      
      // Show panel if hidden
      if (!this._view?.visible) {
        await vscode.commands.executeCommand(
          "workbench.view.extension.aestheticComputer"
        );
        // Brief delay for panel to initialize
        await new Promise(r => setTimeout(r, 300));
      }
      
      // Send jump message to webview
      this.sendMessageToWebview({ 
        type: "jump", 
        piece: data.piece 
      });
    });
    
    this.socket.on("disconnect", () => {
      console.log("Disconnected from session server");
    });
  }
}

// In activate():
export function activate(context: vscode.ExtensionContext) {
  const provider = new AestheticViewProvider(context.extensionUri);
  
  // Connect to session server
  provider.connectToSessionServer();
  
  // ... rest of activation
}
```

3. **Fish Function** (minimal changes):
```fish
function ac --description 'cd to aesthetic-computer or jump to piece'
    if test (count $argv) -eq 0
        cd ~/aesthetic-computer
    else
        set piece_path $argv[1]
        echo "🎯 Jumping to: $piece_path"
        
        set response (curl -s -k -X POST https://localhost:8889/jump \
            -H "Content-Type: application/json" \
            -d "{\"piece\": \"$piece_path\"}")
        
        if string match -q "*vscodeConnected*true*" $response
            echo "✅ Sent to VSCode extension"
        else if string match -q "*Jump request sent*" $response
            echo "✅ Sent to browser clients"
        else
            echo "❌ $response"
        end
    end
end
```

**Pros**:
- ✅ **Works in ALL environments** (devcontainer, Codespaces, desktop)
- ✅ **Direct communication** - no polling or retry needed
- ✅ **Extension controls its own UI** - can show panel programmatically
- ✅ **Uses existing infrastructure** - session server already has WebSocket
- ✅ **Clean architecture** - extension is proper session client
- ✅ **Instant feedback** - knows immediately if extension connected
- ✅ **No terminal dependencies** - no `code` CLI required
- ✅ **Supports both targets** - browser clients AND extension

**Cons**:
- ❌ Requires extension version update (1.191.0)
- ❌ Users need to update extension
- ❌ More moving parts (but all well-tested infrastructure)

**Why this is best**:
This solves the root problem architecturally instead of working around it. The extension becomes a first-class citizen of the session server ecosystem, receiving messages directly instead of relying on WebView visibility.

---

## Recommendation

**Implement Option E: Extension as Session Server Client**

### Rationale

After thorough analysis and testing, Option E is the clear winner:

1. **Universal compatibility**: Works in devcontainer, Codespaces, and desktop VSCode
2. **Architectural cleanliness**: Extension is a proper session server client, not a workaround
3. **Leverages existing infrastructure**: Session server already manages WebSocket connections
4. **No retry logic needed**: Direct communication with instant feedback
5. **Extension controls its own UI**: Can show panel programmatically before navigating
6. **Supports hybrid usage**: Works for both browser clients AND VSCode extension simultaneously

**Why other options don't work**:
- **Option A/D**: `code --command` unavailable in devcontainer/Codespaces (tested and confirmed)
- **Option B**: Retry logic can't solve root problem (panel visibility)
- **Option C**: Queue complexity without solving user experience issue

**The key insight**: Instead of making the terminal try to control VSCode, make VSCode a first-class participant in the session server ecosystem.

### Implementation Plan

**Phase 1: Session Server Updates** (15 minutes)

1. Add VSCode client tracking to `session-server/session.mjs`:
   - Create `vscodeClients` Set to track extension connections
   - Add "identify" event handler to recognize VSCode clients
   - Update `/jump` endpoint to emit "vscode:jump" to extension clients
   - Return `vscodeConnected` flag in response

2. Test with curl:
   ```fish
   curl -k -X POST https://localhost:8889/jump \
     -H "Content-Type: application/json" \
     -d '{"piece": "gameboy~melody"}'
   ```

**Phase 2: Extension Implementation** (30 minutes)

1. Add dependencies to `vscode-extension/package.json`:
   ```json
   "dependencies": {
     "socket.io-client": "^4.7.2"
   }
   ```

2. Update `vscode-extension/extension.ts`:
   - Import socket.io-client
   - Add `connectToSessionServer()` method to `AestheticViewProvider`
   - Emit "identify" with type "vscode" on connection
   - Listen for "vscode:jump" messages
   - Show panel if hidden before navigating
   - Handle connection/disconnection gracefully

3. Update extension version to 1.191.0 in `package.json`

4. Build and test in devcontainer:
   ```fish
   cd vscode-extension
   npm install
   npm run compile
   # Test with F5 (Extension Development Host)
   ```

**Phase 3: Fish Function Update** (5 minutes)

1. Update `.devcontainer/config.fish` ac function:
   - Parse response for `vscodeConnected` flag
   - Show appropriate success message
   - Minimal changes - no retry logic needed

2. Test scenarios:
   - `ac gameboy~melody` with panel hidden → should show panel and navigate
   - `ac gameboy~melody` with panel visible → should navigate immediately
   - `ac gameboy~melody` with extension not running → should still send to browsers

**Phase 4: Testing & Publishing** (20 minutes)

1. Comprehensive testing:
   - Panel hidden → jump command → verify panel shows
   - Panel visible → jump command → verify immediate navigation  
   - Extension not running → verify graceful fallback
   - Build script auto-launch → verify works in automation
   - Browser client → verify still receives messages

2. Extension packaging and publishing:
   ```fish
   cd vscode-extension
   vsce package
   vsce publish
   ```

3. Update documentation in README

**Total estimated time**: ~70 minutes

---

## Alternative Considerations

### Why this beats the CLI approach (Option D)?

The `code --command` approach seemed ideal but:
- ❌ Not available in VSCode devcontainers
- ❌ Not available in Codespaces  
- ❌ Creates OS/environment dependencies
- ❌ Still requires polling/retry logic

The WebSocket approach:
- ✅ Works in all VSCode environments
- ✅ Direct, instant communication
- ✅ Extension can control its own UI
- ✅ No polling or retry needed
- ✅ Uses existing session server infrastructure

### Why not just rely on browser clients?

The session server already broadcasts to browser clients, but:
- Browser clients can't show the VSCode sidebar panel
- User has to manually open panel before it works
- No way to provide feedback that panel needs opening

### Why not use VSCode Extension IPC?

VSCode extensions can't directly communicate with terminal processes. The session server acts as the perfect intermediary:
- Terminal → HTTP POST → Session server → WebSocket → Extension
- Clean separation of concerns
- Session server already manages websocket connections

---

## Success Criteria

After implementation, the `ac` command should:
1. ✅ Work immediately when panel already visible
2. ✅ **Show panel automatically if hidden** (extension controls this)
3. ✅ Work in all VSCode environments (devcontainer, Codespaces, desktop)
4. ✅ Provide instant feedback about VSCode extension connection status
5. ✅ Complete navigation within 500ms in success case
6. ✅ Work in build script auto-launch scenario
7. ✅ Work in manual terminal usage scenario
8. ✅ Continue to work with browser clients simultaneously

---

## Future Improvements

### Extension enhancements
- Add status bar indicator showing session server connection
- Add notification when jump command received
- Add `ac status` command to check connection health
- Add reconnection logic with exponential backoff

### Session server enhancements  
- Add `/status` endpoint returning connected client counts
- Add message delivery confirmations
- Add client presence tracking (last seen timestamp)

### User-facing improvements
- Add `ac doctor` command to diagnose setup issues
- Show VSCode notification when jump received while coding elsewhere
- Add keyboard shortcut to toggle panel visibility

---

## Appendix: Code Locations

| Component | File | Lines |
|-----------|------|-------|
| Fish ac function | `.devcontainer/config.fish` | 848-857 |
| Session server /jump | `session-server/session.mjs` | 225-235 |
| Extension provider | `vscode-extension/extension.ts` | 716-920 |
| Build script auto-launch | `kidlisp-gameboy/build.sh` | 68-74 |

---

**Next Steps**: 
1. Review this plan with user
2. Begin Phase 1: Session server implementation
3. Phase 2: Extension WebSocket client
4. Phase 3: Fish function update
5. Phase 4: Testing and publishing extension v1.191.0
