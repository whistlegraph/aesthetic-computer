#!/usr/bin/env fish
# Emacs Optimization - Quick Switch Script
# Usage: ./switch-emacs-config.fish [test|apply|revert]

set -l operation $argv[1]
set -l config_dir ~/aesthetic-computer/dotfiles/dot_config
set -l original_config $config_dir/emacs.el
set -l optimized_config $config_dir/emacs-optimized.el
set -l backup_config $config_dir/emacs-backup.el

function show_help
    echo "Emacs Configuration Switch Utility"
    echo ""
    echo "Usage: ./switch-emacs-config.fish [COMMAND]"
    echo ""
    echo "Commands:"
    echo "  test     - Kill current daemon and test optimized config once"
    echo "  apply    - Backup current config and switch to optimized permanently"
    echo "  revert   - Restore from backup (emacs-backup.el)"
    echo "  status   - Show current config and daemon status"
    echo "  kill     - Kill the blocking native-comp process (if stuck)"
    echo ""
    echo "Examples:"
    echo "  ./switch-emacs-config.fish test     # Test new config"
    echo "  ./switch-emacs-config.fish apply    # Make it permanent"
    echo "  ./switch-emacs-config.fish revert   # Go back to old config"
end

function kill_emacs_daemon
    echo "🔪 Killing Emacs daemon processes..."
    pkill -f "emacs.*daemon" 2>/dev/null
    pkill -f "emacs.*comp-spawn" 2>/dev/null
    sleep 1
    echo "✅ Daemons terminated"
end

function test_optimized
    echo "🧪 Testing optimized configuration..."
    
    if not test -f $optimized_config
        echo "❌ Error: $optimized_config not found"
        return 1
    end
    
    kill_emacs_daemon
    
    echo "🚀 Starting Emacs daemon with optimized config..."
    echo "   Log: /tmp/emacs-debug.log"
    emacs -q --daemon -l $optimized_config
    
    echo ""
    echo "✅ Daemon started. Monitor with:"
    echo "   tail -f /tmp/emacs-debug.log"
    echo ""
    echo "Test it:"
    echo "   emacsclient -nw"
    echo ""
    echo "If it works, run: ./switch-emacs-config.fish apply"
end

function apply_optimized
    echo "🔧 Applying optimized configuration permanently..."
    
    if not test -f $optimized_config
        echo "❌ Error: $optimized_config not found"
        return 1
    end
    
    # Backup current config
    if test -f $original_config
        echo "💾 Backing up current config to $backup_config"
        cp $original_config $backup_config
        echo "✅ Backup created"
    end
    
    # Apply optimized config
    echo "📝 Replacing config with optimized version"
    cp $optimized_config $original_config
    
    kill_emacs_daemon
    
    echo "🚀 Starting daemon with new config..."
    emacs --daemon -l $original_config
    
    echo ""
    echo "✅ Optimized config applied!"
    echo "   Original backed up to: $backup_config"
    echo "   Log: /tmp/emacs-debug.log"
    echo ""
    echo "To revert: ./switch-emacs-config.fish revert"
end

function revert_config
    echo "⏮️  Reverting to backup configuration..."
    
    if not test -f $backup_config
        echo "❌ Error: No backup found at $backup_config"
        echo "   Cannot revert without backup"
        return 1
    end
    
    echo "📝 Restoring from backup"
    cp $backup_config $original_config
    
    kill_emacs_daemon
    
    echo "🚀 Starting daemon with restored config..."
    emacs --daemon -l $original_config
    
    echo ""
    echo "✅ Reverted to backup configuration"
    echo "   Log: /tmp/emacs-debug.log"
end

function show_status
    echo "📊 Emacs Configuration Status"
    echo ""
    
    # Config files
    echo "Config files:"
    if test -f $original_config
        echo "  ✅ emacs.el        (active)"
    else
        echo "  ❌ emacs.el        (missing)"
    end
    
    if test -f $optimized_config
        echo "  ✅ emacs-optimized.el"
    else
        echo "  ❌ emacs-optimized.el (missing)"
    end
    
    if test -f $backup_config
        echo "  ✅ emacs-backup.el"
    else
        echo "  ⚠️  emacs-backup.el (no backup)"
    end
    
    echo ""
    
    # Running processes
    echo "Running processes:"
    set -l daemon_pid (pgrep -f "emacs.*daemon" | head -1)
    if test -n "$daemon_pid"
        echo "  ✅ Emacs daemon running (PID: $daemon_pid)"
        ps -p $daemon_pid -o pid,ppid,pcpu,pmem,etime,cmd | tail -n +2
    else
        echo "  ⚠️  No Emacs daemon running"
    end
    
    echo ""
    
    set -l comp_pid (pgrep -f "emacs.*comp-spawn" | head -1)
    if test -n "$comp_pid"
        echo "  ⚙️  Native compiler running (PID: $comp_pid)"
        ps -p $comp_pid -o pid,ppid,pcpu,pmem,etime,cmd | tail -n +2
    else
        echo "  ✅ No active compilation"
    end
    
    echo ""
    
    # Recent log
    if test -f /tmp/emacs-debug.log
        echo "Recent log entries:"
        tail -5 /tmp/emacs-debug.log | sed 's/^/  /'
    end
end

function kill_compiler
    echo "🔪 Killing blocking native compilation process..."
    
    set -l comp_pids (pgrep -f "emacs.*comp-spawn")
    if test -n "$comp_pids"
        for pid in $comp_pids
            echo "   Killing PID $pid"
            kill $pid
        end
        echo "✅ Compiler processes terminated"
        echo "   Emacs daemon should be responsive now"
    else
        echo "⚠️  No compilation processes found"
    end
end

# Main logic
switch $operation
    case test
        test_optimized
    case apply
        apply_optimized
    case revert
        revert_config
    case status
        show_status
    case kill
        kill_compiler
    case ""
        show_help
    case "*"
        echo "❌ Unknown command: $operation"
        echo ""
        show_help
        exit 1
end
