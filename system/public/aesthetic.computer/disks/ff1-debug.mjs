// FF1 Debug Console, 2026.2.1.22.15
// A diagnostic piece that displays system info and test patterns on FF1
// Cast this to FF1 to verify the display is working

let logs = [];
const MAX_LOGS = 50;
let bootTime;
let frameCount = 0;

export function boot({ params, screen, net }) {
  bootTime = Date.now();
  
  // Add initial diagnostics as log entries
  addLog('info', '🔍 FF1 Debug Console started');
  addLog('info', `📐 Screen: ${screen.width} x ${screen.height}`);
  addLog('info', `🎨 Density param: ${params.density || 'default'}`);
  addLog('info', `📱 Device mode: ${params.device || 'false'}`);
  addLog('info', `🕐 Boot time: ${new Date().toISOString()}`);
  
  return { logs };
}

function addLog(method, message) {
  const timestamp = new Date().toISOString().substr(11, 12);
  logs.unshift({ timestamp, method, message });
  if (logs.length > MAX_LOGS) logs.pop();
}

export function paint({ ink, wipe, screen, params }) {
  wipe("black");
  
  const lineHeight = 16;
  const padding = 10;
  const maxLines = Math.floor((screen.height - padding * 2) / lineHeight);
  
  // Title
  ink("cyan").write("FF1 DEBUG CONSOLE", { x: padding, y: padding });
  ink("gray").write(`${logs.length} logs | ${new Date().toLocaleTimeString()}`, { x: padding, y: padding + lineHeight });
  
  // Draw logs
  const startY = padding + lineHeight * 3;
  const visibleLogs = logs.slice(0, maxLines - 3);
  
  visibleLogs.forEach((log, i) => {
    const y = startY + i * lineHeight;
    
    // Color based on log type
    let color = "white";
    if (log.method === 'error' || log.method === 'ERROR') color = "red";
    else if (log.method === 'warn') color = "yellow";
    else if (log.method === 'info') color = "cyan";
    else if (log.method === 'debug') color = "gray";
    else if (log.method === 'REJECT') color = "orange";
    
    // Timestamp
    ink("gray").write(log.timestamp, { x: padding, y });
    
    // Method
    ink(color).write(`[${log.method.toUpperCase().substr(0, 5)}]`, { x: padding + 100, y });
    
    // Message (truncated)
    const maxMsgLen = 80;
    const msg = log.message.length > maxMsgLen 
      ? log.message.substr(0, maxMsgLen) + '...' 
      : log.message;
    ink(color).write(msg, { x: padding + 170, y });
  });
  
  // Frame count and uptime at bottom
  const uptime = Math.floor((Date.now() - bootTime) / 1000);
  ink("gray").write(`Frame: ${frameCount} | Uptime: ${uptime}s`, { 
    x: padding, 
    y: screen.height - padding - lineHeight 
  });
}

export function sim() {
  frameCount++;
  
  // Add periodic status updates
  if (frameCount % 300 === 0) { // Every ~5 seconds at 60fps
    addLog('info', `⏱️ Frame ${frameCount} | ${Math.floor((Date.now() - bootTime) / 1000)}s uptime`);
  }
}

export function act({ event: e, jump, screen }) {
  if (e.is("keyboard:down:r")) {
    addLog('info', '🔄 Manual refresh triggered');
    addLog('info', `📐 Screen: ${screen.width} x ${screen.height}`);
    addLog('info', `🕐 Time: ${new Date().toISOString()}`);
  }
  
  if (e.is("keyboard:down:escape")) {
    jump("prompt");
  }
  
  // Log touch/click events
  if (e.is("touch")) {
    addLog('debug', `👆 Touch at ${e.x}, ${e.y}`);
  }
}

export const system = "nopaint";