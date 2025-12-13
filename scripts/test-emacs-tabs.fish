#!/usr/bin/env fish
# Emacs Tab Unit Testing Script
# Tests individual tabs to find which one causes freezes

set -g LOG_DIR /workspaces/aesthetic-computer/.emacs-logs
set -g TEST_LOG $LOG_DIR/tab-test.log

function log
    set msg "[$(date '+%Y-%m-%d %H:%M:%S')] $argv"
    echo $msg
    echo $msg >> $TEST_LOG
end

function check_daemon_responsive
    timeout 3 emacsclient -e '(+ 1 1)' >/dev/null 2>&1
    return $status
end

function get_daemon_cpu
    ps aux | grep "emacs.*daemon" | grep -v grep | awk '{print $3}' | head -1
end

function start_fresh_daemon
    log "🔄 Starting fresh emacs daemon..."
    pkill -9 emacs 2>/dev/null
    pkill -9 emacsclient 2>/dev/null
    sleep 1
    
    emacs -q --daemon -l /workspaces/aesthetic-computer/dotfiles/dot_config/emacs.el >/dev/null 2>&1 &
    disown
    
    # Wait for daemon to be ready
    for i in (seq 1 10)
        if check_daemon_responsive
            log "✅ Daemon ready after $i seconds"
            return 0
        end
        sleep 1
    end
    
    log "❌ Daemon failed to start"
    return 1
end

function create_minimal_backend
    log "Creating minimal backend (artery + fishy only)..."
    emacsclient -e '(aesthetic-backend-minimal)' >/dev/null 2>&1
    sleep 3
end

function add_single_tab
    set tab_name $argv[1]
    set -e argv[1]
    set commands $argv
    
    log "➕ Adding tab: $tab_name with commands: $commands"
    
    # Start profiler before creating tab
    emacsclient -e '(ac-profile-start)' >/dev/null 2>&1
    
    # Create the tab
    set cmd_list (string join " " (for c in $commands; echo "\"$c\""; end))
    emacsclient -e "(ac--create-split-tab \"$tab_name\" '($cmd_list))" >/dev/null 2>&1
    
    return $status
end

function monitor_stability
    set duration $argv[1]
    set check_interval 5
    set checks (math "$duration / $check_interval")
    
    log "📊 Monitoring stability for $duration seconds..."
    
    for i in (seq 1 $checks)
        sleep $check_interval
        
        set cpu (get_daemon_cpu)
        set responsive "?"
        
        if check_daemon_responsive
            set responsive "✓"
        else
            set responsive "✗"
            log "❌ FROZEN at check $i (CPU: $cpu%)"
            
            # Stop profiler and get report
            emacsclient -e '(ac-profile-stop)' 2>/dev/null
            return 1
        end
        
        log "  Check $i/$checks: CPU=$cpu% responsive=$responsive"
    end
    
    # Stop profiler
    emacsclient -e '(ac-profile-stop)' >/dev/null 2>&1
    
    log "✅ Stable for $duration seconds"
    return 0
end

function run_diagnostics
    log "🔍 Running diagnostics..."
    emacsclient -e '(ac-diagnose-all)' >/dev/null 2>&1
end

function test_single_tab
    set tab_name $argv[1]
    set -e argv[1]
    set commands $argv
    
    echo ""
    log "=========================================="
    log "TESTING TAB: $tab_name"
    log "Commands: $commands"
    log "=========================================="
    
    # Start fresh
    if not start_fresh_daemon
        log "❌ TEST FAILED: Could not start daemon"
        return 1
    end
    
    # Create minimal backend first
    create_minimal_backend
    
    # Check baseline stability
    log "Checking baseline (artery + fishy only)..."
    if not monitor_stability 15
        log "❌ TEST FAILED: Baseline unstable"
        return 1
    end
    
    # Add the test tab
    if not add_single_tab $tab_name $commands
        log "❌ TEST FAILED: Could not create tab"
        return 1
    end
    
    # Monitor after adding tab
    log "Monitoring after adding $tab_name..."
    if not monitor_stability 30
        log "❌ TEST FAILED: Tab $tab_name caused instability"
        run_diagnostics
        return 1
    end
    
    run_diagnostics
    log "✅ TEST PASSED: Tab $tab_name is stable"
    return 0
end

function test_all_tabs_sequential
    echo ""
    log "=========================================="
    log "SEQUENTIAL TAB TEST - Adding tabs one by one"
    log "=========================================="
    
    # Start fresh
    if not start_fresh_daemon
        log "❌ Could not start daemon"
        return 1
    end
    
    # Create minimal backend
    create_minimal_backend
    
    # Check baseline
    log "Checking baseline..."
    if not monitor_stability 15
        log "❌ Baseline unstable"
        return 1
    end
    
    # Define tabs to test
    set tabs \
        "status:url:tunnel" \
        "stripe:stripe-print:stripe-ticket" \
        "chat:chat-system:chat-sotce:chat-clock" \
        "web 1/2:site:session" \
        "web 2/2:redis:bookmarks:oven:media" \
        "tests:kidlisp"
    
    for tab_spec in $tabs
        set parts (string split ":" $tab_spec)
        set tab_name $parts[1]
        set -e parts[1]
        set commands $parts
        
        log ""
        log "--- Adding: $tab_name ---"
        
        if not add_single_tab $tab_name $commands
            log "❌ Failed to create tab $tab_name"
            return 1
        end
        
        # Wait for tab to settle
        sleep 5
        
        # Check stability
        if not monitor_stability 20
            log "❌ FOUND PROBLEM: Tab $tab_name caused instability"
            run_diagnostics
            return 1
        end
        
        log "✓ Tab $tab_name stable"
    end
    
    log ""
    log "=========================================="
    log "✅ ALL TABS PASSED SEQUENTIAL TEST"
    log "=========================================="
    return 0
end

function test_rapid_creation
    echo ""
    log "=========================================="
    log "RAPID CREATION TEST - All tabs at once"
    log "=========================================="
    
    # Start fresh
    if not start_fresh_daemon
        log "❌ Could not start daemon"
        return 1
    end
    
    # Start profiler
    emacsclient -e '(ac-profile-start)' >/dev/null 2>&1
    
    # Run full aesthetic-backend
    log "Starting full aesthetic-backend..."
    emacsclient -e '(aesthetic-backend "artery")' >/dev/null 2>&1
    
    # Monitor for 60 seconds
    if not monitor_stability 60
        log "❌ RAPID CREATION CAUSED INSTABILITY"
        run_diagnostics
        return 1
    end
    
    run_diagnostics
    log "✅ RAPID CREATION TEST PASSED"
    return 0
end

function show_help
    echo "Emacs Tab Unit Testing"
    echo ""
    echo "Usage: test-emacs-tabs.fish <command>"
    echo ""
    echo "Commands:"
    echo "  single <tab> <cmds...>  Test a single tab"
    echo "  sequential              Test all tabs one by one"
    echo "  rapid                   Test rapid creation (all at once)"
    echo "  diagnose                Run diagnostics on current daemon"
    echo "  profile-start           Start CPU profiler"
    echo "  profile-stop            Stop profiler and save report"
    echo ""
    echo "Examples:"
    echo "  test-emacs-tabs.fish single status url tunnel"
    echo "  test-emacs-tabs.fish sequential"
    echo "  test-emacs-tabs.fish rapid"
end

# Main
switch $argv[1]
    case single
        set -e argv[1]
        test_single_tab $argv
    case sequential
        test_all_tabs_sequential
    case rapid
        test_rapid_creation
    case diagnose
        emacsclient -e '(ac-diagnose-all)'
        echo "Check $LOG_DIR/emacs-debug.log"
    case profile-start
        emacsclient -e '(ac-profile-start)'
        echo "Profiler started"
    case profile-stop
        emacsclient -e '(ac-profile-stop)'
        echo "Profiler stopped - check $LOG_DIR/emacs-profile.log"
    case help --help -h ''
        show_help
    case '*'
        echo "Unknown command: $argv[1]"
        show_help
        exit 1
end
