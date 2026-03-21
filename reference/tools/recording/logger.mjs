/**
 * Logger utility that routes output through TUI dashboard when enabled,
 * or uses normal console when TUI is disabled.
 */

let dashboard = null;
let useTUI = false;

/**
 * Initialize the logger with TUI dashboard
 */
export function initLogger(dashboardInstance, tuiEnabled) {
  dashboard = dashboardInstance;
  useTUI = tuiEnabled;
}

/**
 * Log an info message
 */
export function logInfo(message) {
  if (useTUI && dashboard) {
    dashboard.addLog('info', message);
  } else {
    console.log(message);
  }
}

/**
 * Log an error message
 */
export function logError(message) {
  if (useTUI && dashboard) {
    dashboard.addLog('error', message);
  } else {
    console.error(message);
  }
}

/**
 * Log a warning message
 */
export function logWarning(message) {
  if (useTUI && dashboard) {
    dashboard.addLog('warning', message);
  } else {
    console.warn(message);
  }
}

/**
 * Override console methods when TUI is active
 */
export function overrideConsole() {
  if (useTUI) {
    // Store originals
    const originalLog = console.log;
    const originalError = console.error;
    const originalWarn = console.warn;
    const originalStdoutWrite = process.stdout.write;
    const originalStderrWrite = process.stderr.write;
    
    // Override console methods
    console.log = (...args) => {
      if (dashboard) {
        dashboard.addLog('info', args.join(' '));
      }
    };
    
    console.error = (...args) => {
      if (dashboard) {
        dashboard.addLog('error', args.join(' '));
      }
    };
    
    console.warn = (...args) => {
      if (dashboard) {
        dashboard.addLog('warning', args.join(' '));
      }
    };
    
    // Override stdout/stderr to catch direct writes (but allow blessed through)
    process.stdout.write = function(chunk, encoding, callback) {
      // Allow blessed terminal control sequences through
      if (typeof chunk === 'string' && (chunk.includes('\x1b[') || chunk.includes('\u001b['))) {
        return originalStdoutWrite.call(this, chunk, encoding, callback);
      }
      
      // Suppress other direct writes in TUI mode
      if (typeof encoding === 'function') {
        encoding(); // callback
      } else if (typeof callback === 'function') {
        callback();
      }
      return true;
    };
    
    process.stderr.write = function(chunk, encoding, callback) {
      // Allow blessed terminal control sequences through
      if (typeof chunk === 'string' && (chunk.includes('\x1b[') || chunk.includes('\u001b['))) {
        return originalStderrWrite.call(this, chunk, encoding, callback);
      }
      
      // Suppress other direct writes in TUI mode
      if (typeof encoding === 'function') {
        encoding(); // callback
      } else if (typeof callback === 'function') {
        callback();
      }
      return true;
    };
    
    // Return restore function
    return () => {
      console.log = originalLog;
      console.error = originalError;
      console.warn = originalWarn;
      process.stdout.write = originalStdoutWrite;
      process.stderr.write = originalStderrWrite;
    };
  }
  return () => {}; // No-op restore function
}

/**
 * Log timing information (performance logs)
 */
export function logTiming(message) {
  if (useTUI && dashboard) {
    dashboard.addLog('timing', message);
  } else {
    console.log(message);
  }
}

/**
 * Log export information
 */
export function logExport(message) {
  if (useTUI && dashboard) {
    dashboard.addLog('export', message);
  } else {
    console.log(message);
  }
}

/**
 * Update progress (no-op since we removed progress bar from dashboard)
 */
export function updateProgress(current, total, message) {
  // No-op - progress is now shown in status panel as "Frame X/Y"
}

/**
 * Update memory stats (TUI only)
 */
export function updateMemoryStats(rss, heap, delta) {
  if (useTUI && dashboard) {
    dashboard.updateMemoryStats({ rssMemory: rss, heapMemory: heap, memoryDelta: delta });
  }
}

/**
 * Update status (TUI only)
 */
export function updateStatus(stats) {
  if (useTUI && dashboard) {
    dashboard.updateStatus(stats);
  }
}