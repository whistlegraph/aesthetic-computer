import blessed from 'blessed';
import chalk from 'chalk';

/**
 * TUI Dashboard for tape.mjs recording
 * Provides a simplified 2-panel layout:
 * - Status panel (top): frame count, memory, performance stats
 * - Function log panel (bottom): scrollable function call logs
 */
export class Dashboard {
  constructor() {
    this.screen = null;
    this.statusBox = null;
    this.logBox = null;
    this.functionLogs = [];
    this.maxLogs = 1000;
    this.logQueue = []; // Queue for ordered log processing
    this.logProcessing = false; // Prevent race conditions
    
    this.stats = {
      currentFrame: 0,
      totalFrames: 0,
      rssMemory: 0,
      heapMemory: 0,
      memoryDelta: 0,
      avgFps: 0,
      status: 'Initializing'
    };
    
    this.init();
  }
  
  init() {
    // Create main screen with full control
    this.screen = blessed.screen({
      smartCSR: true,
      title: '🎬 Aesthetic Computer Tape Recorder',
      debug: false,
      dockBorders: true,
      fullUnicode: true,
      autoPadding: false
    });
    
    // Status panel (top, full width)
    this.statusBox = blessed.box({
      label: ' Status ',
      top: 0,
      left: 0,
      width: '100%',
      height: 8,
      border: { type: 'line' },
      style: {
        border: { fg: 'cyan' },
        label: { fg: 'cyan', bold: true }
      },
      content: 'Initializing...'
    });

    // Function log panel (middle, scrollable) - now takes up remaining space
    this.logBox = blessed.box({
      label: ' Function Calls ',
      top: 8,
      left: 0,
      width: '100%',
      height: '100%-8', // Fill remaining space below status
      border: { type: 'line' },
      scrollable: true,
      alwaysScroll: true,
      scrollbar: {
        ch: ' ',
        style: { bg: 'blue' },
        track: { bg: 'grey' }
      },
      style: {
        border: { fg: 'green' },
        label: { fg: 'green', bold: true }
      }
    });
    
    // Append boxes to screen (no progress panel)
    this.screen.append(this.statusBox);
    this.screen.append(this.logBox);
    
    // Handle key events
    this.screen.key(['q', 'C-c'], () => {
      this.destroy();
    });
    
    // Handle window resize
    this.screen.on('resize', () => {
      this.render();
    });
    
    // Initial render and take control
    this.render();
    
    // Set up periodic rendering to keep dashboard visible (but not too frequent)
    this.renderInterval = setInterval(() => {
      this.render();
    }, 500); // Render every 500ms - less aggressive
  }
  
  /**
   * Update status information
   */
  updateStatus(stats) {
    Object.assign(this.stats, stats);
    
    const content = [
      `Status: ${this.stats.status}`,
      `Frame: ${this.stats.currentFrame}/${this.stats.totalFrames}`,
      `Memory: RSS ${this.formatMemory(this.stats.rssMemory)} (${this.formatMemoryDelta(this.stats.memoryDelta)})`,
      `Heap: ${this.formatMemory(this.stats.heapMemory)}`,
      `Avg FPS: ${this.stats.avgFps.toFixed(1)}`,
      `Time: ${new Date().toLocaleTimeString()}`
    ].join('\n'); // Use actual newlines
    
    this.statusBox.setContent(content);
    this.render();
  }
  
  /**
   * Add function call log entry with proper ordering
   */
  addLog(type, message, timing = null) {
    const timestamp = new Date().toLocaleTimeString();
    const icon = this.getLogIcon(type);
    const timingStr = timing ? ` (${timing}ms)` : '';
    const logEntry = `${timestamp} ${icon} ${message}${timingStr}`;
    
    // Directly add and update - simpler approach
    this.functionLogs.push(logEntry);
    
    // Keep only recent logs to prevent memory growth
    if (this.functionLogs.length > this.maxLogs) {
      this.functionLogs.shift();
    }
    
    // Update log box content immediately
    this.logBox.setContent(this.functionLogs.join('\n'));
    this.logBox.setScrollPerc(100); // Auto-scroll to bottom
    this.render();
  }
  
  /**
   * Get appropriate icon for log type
   */
  getLogIcon(type) {
    const icons = {
      'line': '⚡',
      'findColor': '⚡',
      'color': '⚡',
      'write': '📝',
      'blur': '🌀',
      'scroll': '📜',
      'slow': '🐌',
      'frame': '🎬',
      'memory': '💾',
      'export': '🚀',
      'error': '💥',
      'info': 'ℹ️',
      'warning': '⚠️'
    };
    
    return icons[type] || 'ℹ️';
  }
  
  /**
   * Format memory in human readable form
   */
  formatMemory(bytes) {
    if (bytes === 0) return '0MB';
    const mb = bytes / (1024 * 1024);
    return `${mb.toFixed(0)}MB`;
  }
  
  /**
   * Format memory delta with + or - sign
   */
  formatMemoryDelta(bytes) {
    if (bytes === 0) return '+0MB';
    const mb = bytes / (1024 * 1024);
    const sign = mb >= 0 ? '+' : '';
    return `${sign}${mb.toFixed(0)}MB`;
  }
  
  /**
   * Render the screen
   */
  render() {
    this.screen.render();
  }
  
  /**
   * Clean up and exit
   */
  destroy() {
    if (this.renderInterval) {
      clearInterval(this.renderInterval);
      this.renderInterval = null;
    }
    if (this.screen) {
      this.screen.destroy();
      process.exit(0);
    }
  }
  
  /**
   * Show startup message
   */
  showStartup() {
    this.updateStatus({
      status: 'Starting up...',
      currentFrame: 0,
      totalFrames: 0,
      rssMemory: 0,
      heapMemory: 0,
      memoryDelta: 0,
      avgFps: 0
    });
    
    this.addLog('info', 'Dashboard initialized');
    this.addLog('info', 'Press q or Ctrl+C to exit');
  }
  
  /**
   * Show completion message
   */
  showComplete(outputFile = null) {
    this.updateStatus({
      ...this.stats,
      status: 'Complete!'
    });
    
    this.addLog('info', '🎉 Animation recording completed!');
    if (outputFile) {
      this.addLog('export', `MP4 saved: ${outputFile}`);
    }
  }
  
  /**
   * Show error message
   */
  showError(error) {
    this.updateStatus({
      ...this.stats,
      status: 'Error!'
    });
    
    this.addLog('error', `Error: ${error.message}`);
  }
}

// Export singleton instance
export const dashboard = new Dashboard();