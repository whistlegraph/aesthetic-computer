# Emacs Monitor - Race Condition Debugging

## Overview

The `emacs-monitor.sh` script helps debug intermittent crashes and race conditions that occur during Emacs initialization with the `aesthetic` command.

## Problem Description

Occasionally, the Emacs daemon gets stuck in an infinite loop or crashes during startup, consuming 100% CPU. This appears to be a race condition related to:

- Network calls during straight.el bootstrap
- Package installation timing
- Complex window/tab creation during aesthetic-backend initialization
- Container resource availability during startup

## Usage

### Option 1: Manual Monitoring

```bash
# Terminal 1: Start the monitor
./utilities/emacs-monitor.sh

# Terminal 2: Run the command that might crash
aesthetic-now

# Terminal 1: Press Ctrl+C when done
```

### Option 2: Automated Race Condition Test

```bash
./utilities/emacs-monitor.sh
# Press 't' + Enter when prompted
```

This will automatically:
1. Kill existing Emacs processes
2. Try `aesthetic-now` 3 times with timeouts
3. Log all attempts and failures

## Log Files

The monitor creates several log files in `/tmp/emacs-monitoring/`:

- `system.log` - System resource monitoring (CPU, memory, processes)
- `aesthetic-output.log` - Output from aesthetic-now attempts
- `/tmp/emacs-debug.log` - Emacs internal debugging (if enabled in config)

## What to Look For

### Signs of Race Condition:
- Emacs process consuming 100% CPU for extended periods
- Process stuck in 'R' (running) state for > 30 seconds
- Network timeouts during straight.el bootstrap
- Zombie NPM processes accumulating
- System load average > 4.0

### Common Failure Patterns:
1. **Network Bootstrap Hang**: Straight.el download times out
2. **Package Installation Loop**: Git clone operations fail/retry
3. **Window Management Crash**: Tab creation fails due to resource contention
4. **Memory Pressure**: System starts swapping during heavy package compilation

## Debugging Tips

If the race condition occurs:

1. **Check the logs**: Look for patterns in timing and resource usage
2. **Network issues**: Check if GitHub/package repos are accessible
3. **Resource contention**: Look for high CPU/memory usage from other processes
4. **Timing**: Note if failures happen consistently at certain times

## Quick Fixes

If Emacs gets stuck:

```bash
# Kill stuck processes
pkill -f emacs
pkill -f aesthetic

# Clean up zombie NPM processes  
pkill -f npm

# Restart with monitoring
./utilities/emacs-monitor.sh
```

## Configuration Tweaks

To reduce race conditions, consider:

1. **Reduce straight.el timeout** (currently 30s in emacs.el line ~154)
2. **Add delays** between tab creation steps
3. **Simplify initial package load** (defer non-essential packages)
4. **Add better error handling** in aesthetic-backend function

## Future Improvements

- Add automatic recovery mechanisms
- Implement exponential backoff for network operations
- Create lighter "safe mode" Emacs configuration
- Add container readiness checks before Emacs startup