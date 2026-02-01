// FF1 Debug Console, 2026.2.1.21.45.06.836
// A diagnostic piece that captures and displays console logs on the FF1 display
// Cast this to FF1 to see what's happening in the browser

let logs = [];
const MAX_LOGS = 50;
const originalConsole = {};

export function boot({ params }) {
  // Hijack console methods to capture logs
  ['log', 'warn', 'error', 'info', 'debug'].forEach(method => {
    originalConsole[method] = console[method];
    console[method] = (...args) => {
      const timestamp = new Date().toISOString().substr(11, 12);
      const message = args.map(a => {
        try {
          return typeof a === 'object' ? JSON.stringify(a) : String(a);
        } catch {
          return String(a);
        }
      }).join(' ');
      
      logs.unshift({ timestamp, method, message });
      if (logs.length > MAX_LOGS) logs.pop();
      
      // Still call original
      originalConsole[method](...args);
    };
  });

  // Capture errors
  window.addEventListener('error', (e) => {
    const timestamp = new Date().toISOString().substr(11, 12);
    logs.unshift({ timestamp, method: 'ERROR', message: `${e.message} at ${e.filename}:${e.lineno}` });
    if (logs.length > MAX_LOGS) logs.pop();
  });

  // Capture unhandled promise rejections  
  window.addEventListener('unhandledrejection', (e) => {
    const timestamp = new Date().toISOString().substr(11, 12);
    logs.unshift({ timestamp, method: 'REJECT', message: String(e.reason) });
    if (logs.length > MAX_LOGS) logs.pop();
  });

  // Log some initial diagnostics
  console.log('🔍 FF1 Debug Console started');
  console.log('📍 Location:', window.location.href);
  console.log('📐 Screen:', window.screen.width, 'x', window.screen.height);
  console.log('🖥️ Window:', window.innerWidth, 'x', window.innerHeight);
  console.log('📊 DPR:', window.devicePixelRatio);
  console.log('🕐 Time:', new Date().toISOString());
  
  // Try to get system info
  if (navigator.userAgent) console.log('🌐 UA:', navigator.userAgent);
  
  return { logs };
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
  
  // Instructions at bottom
  ink("gray").write("Press R to refresh diagnostics | ESC to exit", { 
    x: padding, 
    y: screen.height - padding - lineHeight 
  });
}

export function act({ event: e, jump }) {
  if (e.is("keyboard:down:r")) {
    console.log('🔄 Refreshing diagnostics...');
    console.log('📐 Screen:', window.screen.width, 'x', window.screen.height);
    console.log('🖥️ Window:', window.innerWidth, 'x', window.innerHeight);
    console.log('📊 DPR:', window.devicePixelRatio);
    console.log('🕐 Time:', new Date().toISOString());
    console.log('📍 Location:', window.location.href);
  }
  
  if (e.is("keyboard:down:escape")) {
    jump("prompt");
  }
}

export const system = "nopaint";
