#!/usr/bin/env node

/**
 * Main Dashboard Controller - Orchestrates the entire recording process
 * This starts the TUI dashboard first, then manages the recording process
 * and routes all output through the dashboard panels.
 */

import { spawn } from 'child_process';
import { Dashboard } from './dashboard.mjs';
import { initLogger, logInfo, logError, logWarning, updateStatus, updateProgress } from './logger.mjs';
import { existsSync, readdirSync, writeFileSync, appendFileSync } from 'fs';
import { join } from 'path';

class DashboardMain {
  constructor() {
    this.dashboard = new Dashboard();
    this.isRecording = false;
    this.recordingProcess = null;
    this.frameCacheDir = join(process.cwd(), '.tape-frames-cache');
    this.frameCountMonitor = null;
    this.expectedFrames = 24; // Default
    
    // Session tracking for summary
    this.sessionStart = Date.now();
    this.maxMemoryUsed = 0;
    this.totalFramesProcessed = 0;
    this.peakRSS = 0;
    this.peakHeap = 0;
    
    // Set up tape log file in output directory
    const timestamp = new Date().toISOString().replace(/[:.]/g, '-');
    const outputDir = join(process.cwd(), '..', 'output');
    this.logFilePath = join(outputDir, `tape-session-${timestamp}.tape-log`);
    this.initLogFile();

    logInfo('🎬 Dashboard Recording Controller initialized');
    logInfo('Recording will start automatically. Press q or Ctrl+C to exit.');

    // Start initial status
    this.updateDashboardStatus('Initializing...');

    // Set up exit handlers
    this.setupExitHandlers();
  }

  initLogFile() {
    const header = `=== TAPE RECORDING SESSION LOG ===
Started: ${new Date().toISOString()}
Arguments: ${process.argv.slice(2).join(' ')}
Working Directory: ${process.cwd()}
Log File: ${this.logFilePath}
Output Directory: ${join(process.cwd(), '..', 'output')}

=== TUI OUTPUT LOG ===
[Legend: LOG = Function Panel Logs, STATUS = Status Panel Updates, SYSTEM = System Messages]

=== MEMORY TRACKING ===
`;
    writeFileSync(this.logFilePath, header);
    console.log(`📋 Tape log file created: ${this.logFilePath}`);
  }

  logToFile(message) {
    const timestamp = new Date().toISOString();
    appendFileSync(this.logFilePath, `${timestamp} | ${message}\n`);
  }

  writeSessionSummary(exitCode) {
    const sessionEnd = Date.now();
    const duration = (sessionEnd - this.sessionStart) / 1000;
    const summary = `
=== SESSION SUMMARY ===
End Time: ${new Date().toISOString()}
Duration: ${duration.toFixed(1)} seconds
Exit Code: ${exitCode}
Total Frames Processed: ${this.totalFramesProcessed}
Peak RSS Memory: ${(this.peakRSS / 1024 / 1024).toFixed(1)}MB
Peak Heap Memory: ${(this.peakHeap / 1024 / 1024).toFixed(1)}MB
Status: ${exitCode === 0 ? 'SUCCESS' : 'FAILED'}

=== END OF SESSION ===
`;
    appendFileSync(this.logFilePath, summary);
  }

  async init() {
    // Initialize dashboard first
    this.dashboard.init();
    initLogger(this.dashboard, true);
    
    this.dashboard.showStartup();
    this.updateDashboardStatus('Initializing...');
    
    // Set up global dashboard logging for comprehensive capture
    globalThis.dashboardLog = (type, message) => {
      this.dashboard.addLog(type, message);
      this.logToFile(`[LOG:${type.toUpperCase()}] ${message}`);
      
      // Track peak memory usage from messages
      if (message.includes('RSS:') && message.includes('MB')) {
        const rssMatch = message.match(/RSS:\s*(\d+\.?\d*)MB/);
        const heapMatch = message.match(/Heap:\s*(\d+\.?\d*)MB/);
        if (rssMatch) {
          const rss = parseFloat(rssMatch[1]) * 1024 * 1024;
          this.peakRSS = Math.max(this.peakRSS, rss);
        }
        if (heapMatch) {
          const heap = parseFloat(heapMatch[1]) * 1024 * 1024;
          this.peakHeap = Math.max(this.peakHeap, heap);
        }
      }
    };
    
    // Override dashboard addLog to capture all log entries
    const originalAddLog = this.dashboard.addLog.bind(this.dashboard);
    this.dashboard.addLog = (type, message, timing = null) => {
      originalAddLog(type, message, timing);
      const timingStr = timing ? ` (${timing}ms)` : '';
      this.logToFile(`[FUNC:${type.toUpperCase()}] ${message}${timingStr}`);
    };
    
    // Override dashboard updateStatus to capture status changes  
    const originalUpdateStatus = this.dashboard.updateStatus.bind(this.dashboard);
    this.dashboard.updateStatus = (stats) => {
      originalUpdateStatus(stats);
      this.logToFile(`[STATUS] Frame: ${stats.currentFrame}/${stats.totalFrames} | RSS: ${(stats.rssMemory / 1024 / 1024).toFixed(1)}MB | Heap: ${(stats.heapMemory / 1024 / 1024).toFixed(1)}MB | Status: ${stats.status}`);
    };
    
    // Test logging directly
    this.dashboard.addLog('info', 'Dashboard Controller initialized');
    this.dashboard.addLog('info', 'About to start recording in 500ms...');
    
    // Start the recording after a brief delay to let dashboard settle
    setTimeout(() => {
      this.dashboard.addLog('info', 'Starting recording now...');
      this.startRecording(process.argv.slice(2));
    }, 500);
  }

  startFrameCountMonitoring() {
    // Monitor frame cache directory for new frame files
    this.frameCountMonitor = setInterval(() => {
      if (!this.isRecording) {
        clearInterval(this.frameCountMonitor);
        return;
      }
      
      try {
        if (existsSync(this.frameCacheDir)) {
          const files = readdirSync(this.frameCacheDir);
          const frameFiles = files.filter(file => file.startsWith('frame-') && (file.endsWith('.rgba') || file.endsWith('.png')));
          const currentFrameCount = frameFiles.length;
          
          if (currentFrameCount > 0) {
            this.totalFramesProcessed = Math.max(this.totalFramesProcessed, currentFrameCount);
            updateProgress(currentFrameCount, this.expectedFrames, `Dumped ${currentFrameCount}/${this.expectedFrames} frames`);
            this.updateDashboardStatus('Recording in progress', {
              currentFrame: currentFrameCount,
              totalFrames: this.expectedFrames
            });
          }
          
          // Track memory usage for summary
          const memUsage = process.memoryUsage();
          this.peakRSS = Math.max(this.peakRSS, memUsage.rss);
          this.peakHeap = Math.max(this.peakHeap, memUsage.heapUsed);
        }
      } catch (error) {
        // Ignore errors - directory might not exist yet
      }
    }, 500); // Check every 500ms
  }

  updateDashboardStatus(status, extra = {}) {
    updateStatus({
      status,
      currentFrame: 0,
      totalFrames: 0,
      rssMemory: process.memoryUsage().rss,
      heapMemory: process.memoryUsage().heapUsed,
      memoryDelta: 0,
      avgFps: 0,
      ...extra
    });
  }

  async startRecording(args) {
    if (this.isRecording) {
      logError('Recording already in progress');
      return;
    }

    this.isRecording = true;
    this.updateDashboardStatus('Starting recording...');
    
    // Extract expected frame count from args
    const framesIndex = args.indexOf('--frames');
    if (framesIndex !== -1 && framesIndex + 1 < args.length) {
      this.expectedFrames = parseInt(args[framesIndex + 1]) || 24;
    }
    
    logInfo(`Starting recording with args: ${args.join(' ')}`);
    logInfo(`Expected frames: ${this.expectedFrames}`);
    
    // Start frame count monitoring
    this.startFrameCountMonitoring();
    
    // Spawn the tape.mjs process with --no-tui flag to disable its TUI
    const recordingArgs = [...args, '--no-tui'];
    
    // Add Node.js debug flags for detailed performance and memory tracking
    const nodeArgs = [
      '--trace-warnings',           // Show full stack traces for warnings
      '--trace-uncaught',          // Show stack traces for uncaught exceptions
      '--max-old-space-size=8192', // Increase memory limit to 8GB
      '--expose-gc',               // Allow manual garbage collection
      'tape.mjs',
      ...recordingArgs
    ];
    
    logInfo(`Spawning: node ${nodeArgs.join(' ')}`);
    
    this.recordingProcess = spawn('node', nodeArgs, {
      stdio: ['pipe', 'pipe', 'pipe'],
      cwd: process.cwd(),
      env: { 
        ...process.env, 
        NODE_OPTIONS: '--trace-warnings --trace-uncaught',
        DEBUG: '*'  // Enable debug output
      }
    });

    logInfo(`Recording process spawned with PID: ${this.recordingProcess.pid}`);

    // Route stdout through dashboard
    this.recordingProcess.stdout.on('data', (data) => {
      const output = data.toString();
      
      // Filter out sixel data before logging to prevent garbled output
      if (this.containsSixelData(output)) {
        // Don't log raw sixel data, just note that we received it
        this.dashboard.addLog('info', '🎬 Frame data received (sixel filtered)');
      } else {
        logInfo(`Recording stdout: ${output.trim()}`);
      }
      
      this.processRecordingOutput(output);
    });

    // Route stderr through dashboard
    this.recordingProcess.stderr.on('data', (data) => {
      const error = data.toString();
      logError(`Recording stderr: ${error.trim()}`);
    });

        // Handle process completion
    this.recordingProcess.on('close', (code) => {
      this.isRecording = false;
      logInfo(`Recording process exited with code: ${code}`);
      
      // Write session summary to log file
      this.writeSessionSummary(code);
      
      if (code === 0) {
        this.updateDashboardStatus('Recording completed successfully');
        logInfo('🎉 Recording completed successfully!');
        logInfo(`✅ Recording session finished. Log saved to: ${this.logFilePath}`);
        logInfo('Press q to exit and see summary...');
      } else {
        this.updateDashboardStatus(`Recording failed (exit code: ${code})`);
        logError(`Recording process exited with code ${code}`);
        logError(`❌ Recording failed. Log saved to: ${this.logFilePath}`);
        logError('Press q to exit and see summary...');
      }
    });

    this.recordingProcess.on('error', (err) => {
      this.isRecording = false;
      this.updateDashboardStatus('Recording failed to start');
      logError(`Failed to start recording process: ${err.message}`);
      logError('Press q to exit...');
    });
  }

  processRecordingOutput(output) {
    // Clean up the output and split into lines properly
    const cleanOutput = output.replace(/\r\n/g, '\n').replace(/\r/g, '\n');
    const lines = cleanOutput.split('\n').filter(line => line.trim());
    
    for (const line of lines) {
      let cleanLine = line.trim();
      if (!cleanLine) continue;
      
      // Additional text cleaning for spacing and formatting issues
      cleanLine = cleanLine.replace(/\s+/g, ' '); // Collapse multiple spaces
      cleanLine = cleanLine.replace(/[^\x20-\x7E]/g, ''); // Remove non-printable ASCII
      cleanLine = cleanLine.trim();
      
      if (!cleanLine || cleanLine.length < 3) continue; // Skip empty or very short lines
      
      // Filter out binary data, escape sequences, and sixel data
      if (this.isBinaryOrControlData(cleanLine)) {
        continue; // Skip corrupted/binary lines
      }
      
      // Parse different types of output and route to appropriate dashboard panels
      if (cleanLine.includes('Frame') && cleanLine.includes('RSS')) {
        // Memory monitoring output
        this.dashboard.addLog('info', cleanLine);
      } else if (cleanLine.includes('🎬') || cleanLine.includes('📊')) {
        // Recording progress
        this.dashboard.addLog('info', cleanLine);
        this.extractProgressInfo(cleanLine);
      } else if (cleanLine.includes('⚡') || cleanLine.includes('🐌')) {
        // Performance info
        this.dashboard.addLog('performance', cleanLine);
      } else if (cleanLine.includes('❌') || cleanLine.includes('⚠️') || cleanLine.includes('Error:')) {
        // Errors and warnings
        this.dashboard.addLog('error', cleanLine);
      } else if (cleanLine.includes('✅') || cleanLine.includes('🎉')) {
        // Success messages
        this.dashboard.addLog('export', cleanLine);
      } else if (cleanLine.length > 0 && cleanLine.length < 200) {
        // General info - only if reasonable length
        this.dashboard.addLog('info', cleanLine);
      }
    }
  }

  containsSixelData(text) {
    // Simplified sixel data detection for filtering recording output
    return text.includes('\x1bPq') ||       // Sixel start sequence
           text.includes('\x1b\\') ||        // Sixel end sequence  
           text.includes('$#') ||            // Sixel color/position codes
           text.includes('\x1b[') ||         // ANSI escape sequences
           /\$#\d+/.test(text);              // Sixel color codes like $#42
  }

  isBinaryOrControlData(line) {
    // More aggressive filtering for garbled output
    if (line.includes('\x1b') || line.includes('\u001b')) return true; // ANSI escape sequences
    if (line.includes('$#')) return true; // Any sixel color codes
    if (line.length > 500) return true; // Long lines are likely binary
    
    // Check for control characters or malformed text
    const controlChars = (line.match(/[\x00-\x1F\x7F-\x9F]/g) || []).length;
    if (controlChars > 0 && line.length < 100) return true; // Any control chars in short lines
    
    // Check for excessive non-printable characters (more than 5% of line)
    const nonPrintable = (line.match(/[^\x20-\x7E\u00A0-\uFFFF\n\r\t]/g) || []).length;
    if (nonPrintable > line.length * 0.05 && line.length > 20) return true;
    
    // Check for patterns that look like garbled text
    if (/[?]{5,}/.test(line) || /[A-Z]{10,}/.test(line)) return true;
    
    return false;
  }

  extractProgressInfo(line) {
    // Look for various frame progress patterns
    let frameMatch;
    
    // Pattern: "Frame 3 - Before RGBA" or "Frame 3 - After RGBA"
    frameMatch = line.match(/Frame (\d+) - (?:Before|After)/);
    if (frameMatch) {
      const current = parseInt(frameMatch[1]) + 1; // Convert 0-based to 1-based
      const total = 24; // Use known total from args
      
      updateProgress(current, total, `Processing frame ${current}/${total}`);
      this.updateDashboardStatus('Recording in progress', {
        currentFrame: current,
        totalFrames: total
      });
      return;
    }
    
    // Pattern: "🎬 Frame 4/24:"
    frameMatch = line.match(/🎬 Frame (\d+)\/(\d+)/);
    if (frameMatch) {
      const current = parseInt(frameMatch[1]);
      const total = parseInt(frameMatch[2]);
      
      updateProgress(current, total, `Processing frame ${current}/${total}`);
      this.updateDashboardStatus('Recording in progress', {
        currentFrame: current,
        totalFrames: total
      });
      return;
    }
    
    // Pattern: Progress bar "🎬 ██████░░░░ 67% (2/3 frames)"
    frameMatch = line.match(/(\d+)% \((\d+)\/(\d+) frames\)/);
    if (frameMatch) {
      const current = parseInt(frameMatch[2]);
      const total = parseInt(frameMatch[3]);
      
      updateProgress(current, total, `Processing frame ${current}/${total}`);
      this.updateDashboardStatus('Recording in progress', {
        currentFrame: current,
        totalFrames: total
      });
      return;
    }
    
    // Look for recording completion indicators
    if (line.includes('Recording Complete') || line.includes('MP4 exported successfully')) {
      this.updateDashboardStatus('Recording completed successfully');
      updateProgress(this.expectedFrames, this.expectedFrames, 'Recording complete!');
    }
  }

  setupExitHandlers() {
    // Create gracefulExit method we can call
    this.gracefulExit = (showSummary = true) => {
      if (this.frameCountMonitor) {
        clearInterval(this.frameCountMonitor);
      }
      
      // Print summary to console before TUI cleanup to ensure it shows
      if (showSummary) {
        this.printSessionSummary();
      }
      
      if (this.recordingProcess && !this.recordingProcess.killed) {
        this.recordingProcess.kill('SIGTERM');
      }
      
      this.dashboard.destroy();
      
      process.exit(0);
    };

    process.on('SIGINT', this.gracefulExit);
    process.on('SIGTERM', this.gracefulExit);
    
    // Handle 'q' key press to quit (only if stdin is a TTY)
    if (process.stdin.isTTY) {
      process.stdin.setRawMode(true);
      process.stdin.on('data', (key) => {
        if (key.toString() === 'q' || key.toString() === '\u0003') { // 'q' or Ctrl+C
          this.gracefulExit();
        }
      });
    }
  }

  tryPositionedSixelOverlay(sixelData) {
    // EXPERIMENTAL: Position sixel graphics in the preview panel area
    // The preview panel is in the top-right corner of the blessed interface
    
    if (sixelData && sixelData.includes('\x1bPq')) {
      // Calculate approximate position for preview panel (top-right)
      const terminalWidth = process.stdout.columns || 80;
      const previewRow = 4;  // Row where preview panel content starts
      const previewCol = Math.floor(terminalWidth * 0.7); // Right side of screen
      
      // Create positioned sixel with save/restore cursor
      const positionedSixel = `\x1b[s\x1b[${previewRow};${previewCol}H${sixelData}\x1b[u`;
      
      // Write directly to stdout to overlay on blessed.js
      setTimeout(() => {
        process.stdout.write(positionedSixel);
      }, 100); // Small delay to let blessed.js render first
    }
  }

  printSessionSummary() {
    const sessionDuration = Date.now() - this.sessionStart;
    const durationSeconds = (sessionDuration / 1000).toFixed(1);
    const peakRSSMB = Math.round(this.peakRSS / 1024 / 1024);
    const peakHeapMB = Math.round(this.peakHeap / 1024 / 1024);
    
    console.log('\n' + '='.repeat(60));
    console.log('🎬 RECORDING SESSION SUMMARY');
    console.log('='.repeat(60));
    console.log(`⏱️  Duration: ${durationSeconds}s`);
    console.log(`🎞️  Frames Processed: ${this.totalFramesProcessed}/${this.expectedFrames}`);
    console.log(`🧠  Peak Memory - RSS: ${peakRSSMB}MB, Heap: ${peakHeapMB}MB`);
    console.log(`📁  Frame Cache: ${this.frameCacheDir}`);
    
    if (this.totalFramesProcessed === this.expectedFrames) {
      console.log(`✅  Status: Complete`);
    } else {
      console.log(`⚠️  Status: Incomplete (${this.totalFramesProcessed}/${this.expectedFrames} frames)`);
    }
    
    const fps = this.totalFramesProcessed / (sessionDuration / 1000);
    if (fps > 0) {
      console.log(`⚡  Processing Rate: ${fps.toFixed(1)} FPS`);
    }
    
    console.log('='.repeat(60));
  }
}

// Main execution
async function main() {
  const args = process.argv.slice(2);
  
  if (args.length === 0) {
    console.error('Usage: node dashboard-main.mjs <piece.mjs> [options]');
    process.exit(1);
  }

  const dashboardMain = new DashboardMain();
  await dashboardMain.init();
  
  // Small delay to let dashboard render
  setTimeout(() => {
    dashboardMain.startRecording(args);
  }, 500);
}

main().catch((error) => {
  console.error('Fatal error:', error);
  process.exit(1);
});