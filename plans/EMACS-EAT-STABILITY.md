# Emacs + Eat Terminal Stability Investigation

## Problem Summary
The Aesthetic Computer platform uses Emacs daemon with the `eat` terminal emulator to create multiple terminal tabs (8 tabs total with ~15 terminal panes). After all tabs are created successfully, emacs becomes unresponsive (zombie state) with high CPU usage (~60%).

## System Details
- **Emacs**: GNU Emacs 30.2
- **Eat**: Version from straight.el (NonGNU ELPA)
- **OS**: Fedora Linux 43 (Container Image)
- **Memory**: 15GB total, ~6GB available (not a memory issue)
- **Load**: Spikes to 8-11 during tab creation

## Symptoms
1. All tabs create successfully (confirmed via debug logs)
2. All fish processes spawn correctly (confirmed via `ps aux`)
3. ~30-60 seconds after completion, emacs becomes unresponsive
4. CPU usage spikes to ~60% on the emacs daemon process
5. `emacsclient -e t` times out (zombie state)

## Tabs Being Created
| Tab Name | Panes | Commands |
|----------|-------|----------|
| artery | 1 | `ac-artery-dev` (node TUI) |
| fishy | 1 | plain fish shell |
| status | 2 | `ac-url`, `ac-tunnel` (ngrok) |
| stripe | 2 | `ac-stripe-print`, `ac-stripe-ticket` |
| chat | 3 | `ac-chat-system`, `ac-chat-sotce`, `ac-chat-clock` |
| web 1/2 | 2 | `ac-site`, `ac-session` |
| web 2/2 | 4 | `ac-redis`, `ac-servers`, `ac-oven`, `ac-media` |
| tests | 1 | `ac-kidlisp` |

**Total: 8 tabs, ~16 eat terminal panes**

## Investigation Areas

### 1. Eat Performance Settings
From `eat.el` defcustoms:
```elisp
;; Scrollback buffer size - maybe reduce?
eat-term-scrollback-size 131072  ; 128 KB default

;; Shell integration features that might add overhead
eat-enable-directory-tracking t
eat-enable-shell-command-history t
eat-enable-shell-prompt-annotation t
eat-shell-prompt-annotation-correction-delay 0.1

;; Hooks that fire on terminal updates
eat-update-hook nil  ; Could be causing issues if something's hooked
```

### 2. Native Compilation
Emacs 30.2 has native compilation available. Current config **disables** it:
```elisp
(setq native-comp-async-report-warnings-errors nil
      native-comp-deferred-compilation nil
      native-comp-jit-compilation nil           ; Disabled!
      native-comp-async-jobs-number 1
      native-comp-async-query-on-exit nil)
```

**Potential fix**: Enable native compilation for eat.el specifically, or re-enable JIT compilation.

### 3. Multiple eat Process Handling
Each `eat` call creates:
- A process buffer
- A PTY/terminal
- Filter functions for input/output
- Timers for various features

With 16 terminals, that's 16x the overhead.

### 4. Event Loop Starvation
Hypothesis: When all terminals are pumping output simultaneously (npm installs, node startup, etc.), the Emacs event loop gets overwhelmed processing terminal output.

## Potential Solutions

### A. Reduce Eat Features (Low Risk)
```elisp
;; Disable features we don't need
(setq eat-enable-directory-tracking nil
      eat-enable-shell-command-history nil
      eat-enable-shell-prompt-annotation nil
      eat-show-title-on-mode-line nil
      eat-term-scrollback-size 65536)  ; Reduce to 64KB
```

### B. Enable Native Compilation (Medium Risk)
```elisp
;; Enable native compilation for better performance
(setq native-comp-jit-compilation t
      native-comp-async-jobs-number 2)
```

### C. Use Vterm Instead of Eat (Higher Risk)
Vterm is 1.5x faster than eat for high-throughput scenarios. Trade-off: no char mode, needs libvterm.

### D. Stagger Terminal Startup (Current Approach)
Add small delays between tab creation to let event loop breathe:
```elisp
(let ((delay 0.2))
  (dolist (tab-spec tabs)
    (run-with-timer delay nil #'ac--create-split-tab ...)
    (setq delay (+ delay 0.2))))
```

### E. Lazy Tab Creation
Only create artery + fishy initially. Create other tabs on-demand when user switches to them.

### F. Process Output Throttling
Limit how much output eat processes per event loop cycle.

## Test Plan

### Unit Tests for Each Tab
Create test script that:
1. Starts emacs daemon
2. Creates ONE tab at a time
3. Waits 30 seconds
4. Checks if daemon is still responsive
5. Logs which tab (if any) causes the freeze

```fish
# test-tab-stability.fish
function test-single-tab
    set tab_name $argv[1]
    set commands $argv[2..-1]
    
    echo "Testing tab: $tab_name with commands: $commands"
    
    # Start fresh daemon
    ac-emacs-restart
    sleep 2
    
    # Create single tab via emacsclient
    emacsclient -e "(ac--create-split-tab \"$tab_name\" '($commands))"
    
    # Wait and check
    sleep 30
    if timeout 3 emacsclient -e t >/dev/null 2>&1
        echo "✅ Tab $tab_name: STABLE"
    else
        echo "❌ Tab $tab_name: CAUSED FREEZE"
    end
end
```

### Baseline Test
1. Start daemon with NO tabs (just `aesthetic-backend-minimal`)
2. Verify stability over 5 minutes
3. Then add tabs one by one

## Related Files
- `/workspaces/aesthetic-computer/dotfiles/dot_config/emacs.el` - Emacs config
- `/workspaces/aesthetic-computer/.devcontainer/config.fish` - Fish config with `ac-*` commands
- `/workspaces/aesthetic-computer/.emacs-logs/` - Debug and crash logs

## Commands Reference
```fish
ac-emacs-status          # Check daemon status
ac-emacs-restart         # Restart daemon only
ac-emacs-full-restart    # Restart daemon + reconnect UI
aesthetic-now            # Connect to daemon (skip waiter)
```

## Log Locations
- Debug log: `/workspaces/aesthetic-computer/.emacs-logs/emacs-debug.log`
- Perf log: `/workspaces/aesthetic-computer/.emacs-logs/emacs-perf.log`
- Crash log: `/home/me/.emacs-logs/crashes.log`
- Daemon log: `/home/me/.emacs-logs/daemon_*.log`

## Next Steps
1. [ ] Try disabling eat shell integration features
2. [ ] Test with native compilation enabled
3. [ ] Run single-tab stability tests to isolate problematic tabs
4. [ ] Profile emacs with `M-x profiler-start` during tab creation
5. [ ] Consider vterm as alternative for high-output terminals
