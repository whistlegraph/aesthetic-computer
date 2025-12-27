#!/usr/bin/env fish
# Aesthetic Computer Electron - Mac Development Script
# Usage: ./dev.fish [options]
#
# Options:
#   --prod          Start in production mode (aesthetic.computer)
#   --dev           Start in dev mode (localhost:8888)
#   --shell         Start with shell/terminal view
#   --inspect       Enable DevTools on port 9229 (default)
#   --no-inspect    Disable DevTools/CDP
#   --watch         Watch for changes and auto-restart
#   --build         Build DMG before running
#   --piece=NAME    Start with specific piece (default: prompt)
#
# Examples:
#   ./dev.fish                    # Run dev mode with inspector
#   ./dev.fish --prod             # Run production mode
#   ./dev.fish --watch            # Watch mode with auto-restart
#   ./dev.fish --build --prod     # Build DMG then run production

set -l script_dir (dirname (status -f))
cd $script_dir

# Colors
set -l cyan (set_color cyan)
set -l green (set_color green)
set -l yellow (set_color yellow)
set -l red (set_color red)
set -l magenta (set_color magenta)
set -l normal (set_color normal)

echo "$magenta🎨 Aesthetic Computer Electron - Dev Environment$normal"
echo ""

# Parse arguments
set -l mode "dev"
set -l inspect true
set -l watch false
set -l build false
set -l piece "prompt"
set -l extra_args

for arg in $argv
    switch $arg
        case "--prod" "--production"
            set mode "prod"
        case "--dev" "--development"
            set mode "dev"
        case "--shell"
            set extra_args $extra_args "--shell"
        case "--inspect"
            set inspect true
        case "--no-inspect"
            set inspect false
        case "--watch"
            set watch true
        case "--build"
            set build true
        case "--piece=*"
            set piece (string replace "--piece=" "" $arg)
        case "*"
            set extra_args $extra_args $arg
    end
end

# Build electron args
set -l electron_args "."
if test "$mode" = "dev"
    set electron_args $electron_args "--dev"
end
if test -n "$piece"
    set electron_args $electron_args "--piece=$piece"
end
set electron_args $electron_args $extra_args

# Inspector args
set -l inspect_args
if test "$inspect" = true
    set inspect_args "--inspect=9229" "--remote-debugging-port=9222"
    echo "$cyan📡 DevTools Inspector:$normal http://localhost:9229"
    echo "$cyan🔗 CDP (Chrome DevTools Protocol):$normal http://localhost:9222"
    echo ""
end

# Get electron path
set -l electron_bin "./node_modules/electron/dist/Electron.app/Contents/MacOS/Electron"

if not test -f $electron_bin
    echo "$red✗ Electron not found. Running npm install...$normal"
    npm install
end

# Build if requested
if test "$build" = true
    echo "$yellow🔨 Building macOS DMG...$normal"
    npm run build:mac
    echo ""
end

# Show mode
if test "$mode" = "dev"
    echo "$yellow🔧 Mode: DEVELOPMENT (localhost:8888)$normal"
else
    echo "$green🚀 Mode: PRODUCTION (aesthetic.computer)$normal"
end
echo ""

# Function to run electron
function run_electron
    echo "$cyan▶ Starting Electron...$normal"
    echo "  $electron_bin $inspect_args $electron_args"
    echo ""
    
    # Run with CDP ports
    $electron_bin $inspect_args $electron_args
    return $status
end

# Watch mode using fswatch
if test "$watch" = true
    echo "$magenta👁 Watch mode enabled - will restart on file changes$normal"
    echo "  Watching: main.js, preload.js, renderer/"
    echo "  Press Ctrl+C to stop"
    echo ""
    
    # Start electron in background
    set -l electron_pid
    
    function restart_electron
        if test -n "$electron_pid"
            echo ""
            echo "$yellow♻️ Restarting Electron...$normal"
            kill $electron_pid 2>/dev/null
            sleep 0.5
        end
        
        $electron_bin $inspect_args $electron_args &
        set -g electron_pid $last_pid
        echo "$green✓ Electron started (PID: $electron_pid)$normal"
    end
    
    function cleanup
        echo ""
        echo "$cyan🛑 Stopping...$normal"
        if test -n "$electron_pid"
            kill $electron_pid 2>/dev/null
        end
        exit 0
    end
    
    trap cleanup SIGINT SIGTERM
    
    # Initial start
    restart_electron
    
    # Watch for changes
    if command -v fswatch >/dev/null
        fswatch -o main.js preload.js offscreen-manager.js "renderer/" | while read -l event
            restart_electron
        end
    else
        echo "$yellow⚠ fswatch not installed. Install with: brew install fswatch$normal"
        echo "  Running without watch - press Ctrl+C and re-run to pick up changes"
        wait $electron_pid
    end
else
    # Normal run with restart loop
    set -l restart_count 0
    
    while true
        if test $restart_count -gt 0
            echo ""
            echo "$yellow♻️ Restarting Electron (restart #$restart_count)...$normal"
            sleep 1
        end
        
        run_electron
        set -l exit_code $status
        
        if test $exit_code -eq 42
            # Exit code 42 = intentional reboot request
            set restart_count (math $restart_count + 1)
            echo "$green✓ Reboot request received$normal"
        else
            echo ""
            echo "$cyan✓ Electron exited with code $exit_code$normal"
            break
        end
    end
end

echo "$magenta👋 Goodbye!$normal"
