// Split, 2024.3.03.15.46.29.905
// Run two instances of aesthetic computer inside itself, side by side.
// Updated 2025.12.25 - Added inter-frame messaging, orientation support, CDP hooks

/* #region 📚 README 
  Split creates two AC instances in a single browser window.
  
  Usage:
    /split~piece           - Same piece in both frames
    /split~piece1~piece2   - Different pieces top/bottom
    /split~1v1             - Two 1v1 instances for local multiplayer testing
  
  Query params passed to frames:
    ?player=1 or ?player=2  - Identifies which split pane
    ?nogap                  - Removes the gap around the piece
  
  Inter-frame communication:
    Frames can send messages to each other via parent.postMessage
    Parent relays messages between frames with { from: 1|2, ... }
#endregion */

/* #region 🏁 TODO 
  - [-] Fix white style flash.
  - [] Add horizontal split option (~hsplit flag)
  - [] Add resizable divider
  + Done
  - [x] Add inter-frame message relay
  - [x] Make it so that `split` has an escape of some kind?
  - [x] Take care of `gap` spacing.
#endregion */

// export const nohud = true;

let topFrame = null;
let bottomFrame = null;

function boot({ params, dom: { html }, net: { waitForPreload } }) {
  // Parse pieces from params (split~piece1~piece2 or split~piece)
  const piece1 = params[0] || 'prompt';
  const piece2 = params[1] || params[0] || 'prompt';
  
  // Check for flags
  const horizontal = params.includes('h') || params.includes('horizontal');
  
  html`
    <style>
      #content {
        display: flex;
        flex-direction: ${horizontal ? 'row' : 'column'};
        height: 100vh;
        width: 100vw;
        box-sizing: border-box;
        margin: 0;
        padding: 0;
        background: rgb(32, 32, 48);
      }
      iframe {
        box-sizing: border-box;
        flex: 1;
        border: none;
        background: rgb(32, 32, 48);
      }
      #split-top {
        border: none;
        ${horizontal 
          ? 'border-right: 4px solid rgb(64, 64, 96);' 
          : 'border-bottom: 4px solid rgb(64, 64, 96);'}
      }
      .split-label {
        position: absolute;
        font-family: monospace;
        font-size: 10px;
        color: rgba(255, 255, 255, 0.5);
        background: rgba(64, 64, 96, 0.8);
        padding: 2px 6px;
        z-index: 100;
        pointer-events: none;
      }
      #label-top {
        top: 4px;
        left: 4px;
      }
      #label-bottom {
        ${horizontal ? 'top: 4px; right: 4px;' : 'bottom: 4px; left: 4px;'}
      }
    </style>
    <div id="label-top" class="split-label">P1: ${piece1}</div>
    <iframe id="split-top" src="/${piece1}?nogap&player=1"></iframe>
    <iframe id="split-bottom" src="/${piece2}?nogap&player=2"></iframe>
    <div id="label-bottom" class="split-label">P2: ${piece2}</div>
  `;
  
  // Set up inter-frame message relay
  setTimeout(() => {
    topFrame = document.getElementById('split-top');
    bottomFrame = document.getElementById('split-bottom');
    
    // Listen for messages from child frames
    window.addEventListener('message', (event) => {
      // Only handle messages from our iframes
      if (event.source === topFrame?.contentWindow) {
        // Message from top frame - relay to bottom
        if (bottomFrame?.contentWindow) {
          bottomFrame.contentWindow.postMessage({ 
            ...event.data, 
            from: 1,
            _splitRelay: true 
          }, '*');
        }
      } else if (event.source === bottomFrame?.contentWindow) {
        // Message from bottom frame - relay to top
        if (topFrame?.contentWindow) {
          topFrame.contentWindow.postMessage({ 
            ...event.data, 
            from: 2,
            _splitRelay: true 
          }, '*');
        }
      }
    });
    
    // Expose frame references for CDP control
    window.__splitFrames = {
      top: topFrame,
      bottom: bottomFrame,
      sendToTop: (msg) => topFrame?.contentWindow?.postMessage(msg, '*'),
      sendToBottom: (msg) => bottomFrame?.contentWindow?.postMessage(msg, '*'),
      sendToBoth: (msg) => {
        topFrame?.contentWindow?.postMessage({ ...msg, to: 1 }, '*');
        bottomFrame?.contentWindow?.postMessage({ ...msg, to: 2 }, '*');
      }
    };
    
    console.log('🎮 Split frames initialized:', { piece1, piece2 });
  }, 100);
}

// 🎨 Paint
function paint({ wipe }) {
  wipe(32, 32, 48);
  return false;
}

// 🎪 Act
function act({ event: e, needsPaint }) {
  if (e.is("dark-mode") || e.is("light-mode")) needsPaint();
  
  // Escape key - go back to prompt
  if (e.is("keyboard:down:Escape")) {
    window.location.href = '/';
  }
}

// 📰 Meta
function meta() {
  return {
    title: "Split View",
    desc: "Run two AC instances side by side for local multiplayer testing",
  };
}

export { boot, paint, act, meta };
