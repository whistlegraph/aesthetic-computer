#!/usr/bin/env node

// Simple dashboard test - just show the TUI interface
import { dashboard } from './dashboard.mjs';

console.log('Starting dashboard test...');

// Initialize dashboard
dashboard.init();

// Show startup
dashboard.showStartup();

// Update with some test data
dashboard.updateStatus({
  status: 'Testing...',
  currentFrame: 5,
  totalFrames: 10,
  rssMemory: 100 * 1024 * 1024,
  heapMemory: 50 * 1024 * 1024,
  memoryDelta: 0,
  avgFps: 30.5
});

// Add some test logs
dashboard.addLog('info', 'Dashboard test started');
dashboard.addLog('timing', 'Test timing message', 42);
dashboard.addLog('warning', 'Test warning message');
dashboard.addLog('export', 'Test export message');

// Update progress
dashboard.updateProgress(5, 10, 'Testing progress bar...');

console.log('Dashboard initialized. Press q or Ctrl+C to exit.');

// Keep it running
setInterval(() => {
  const now = new Date();
  dashboard.addLog('info', `Heartbeat: ${now.toLocaleTimeString()}`);
}, 2000);