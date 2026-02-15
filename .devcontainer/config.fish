# Aesthetic Computer Development Environment Fish Config (Docker)

# Set environment variables to prevent ETXTBSY errors
set -gx NETLIFY_CLI_TELEMETRY_DISABLED 1
set -gx NODE_DISABLE_COMPILE_CACHE 1

if test -f /home/me/envs/load_envs.fish
    source /home/me/envs/load_envs.fish
    if functions -q load_envs
        load_envs # Load devcontainer envs conditionally.
    end
end

# Add Deno to PATH
set -gx PATH /home/me/.deno/bin $PATH

# Enable `fnm` support.

if test -f $HOME/.fnm/fnm
    set -gx PATH $HOME/.fnm $PATH
    fnm env --use-on-cd --log-level=quiet | source
    fnm use lts-jod --silent-if-unchanged 2>/dev/null
end

# Symlink a VSCode workspace as needed.

if test -d /workspaces/aesthetic-computer
    if test ! -L ~/aesthetic-computer
        echo "Symlinking /workspaces/aesthetic-computer to ~"
        ln -s /workspaces/aesthetic-computer ~
    end
end

# Persist Copilot CLI sessions across container restarts
# Now handled by Docker volume mount (aesthetic-copilot-data)
# The volume is mounted at /home/me/.copilot in devcontainer.json
# No symlink needed - just ensure proper permissions
if test -d ~/.copilot
    # Fix permissions if needed (volume may have wrong owner after restart)
    if not test -O ~/.copilot
        sudo chown -R me:me ~/.copilot 2>/dev/null
    end
end

function fish_prompt
    # Show shell and directory for LLM context
    set_color cyan
    echo -n 'fish'
    set_color normal
    echo -n ' '
    set_color yellow
    echo -n (prompt_pwd)
    set_color normal
    echo -n ' > '
end

function dns
    set -l domain $argv[1]
    echo "https://iwantmyname.com/dashboard/dns/$domain"
end

function fish_greeting
    if test "$nogreet" = true
        return
    end

    printf "🧩 Hi @$AESTHETIC!\n\n"
    
    # Show the new KidLisp source tree shortcut


    # printf "Ask with 'umm' and forget with 'nvm'\n"
    # printf "or use 'code' and 'done' with 'copy'\n"
    # printf "to generate and get code.\n\n"
    # printf "🆕 Try 'aider' to make edits: https://github.com/paul-gauthier/aider?tab=readme-ov-file#usage\n\n"
    # printf "📋 Clipboard also requires `xhost +local:docker` to be set on the host."
end

function ac-tv
    # Detect Codespaces and construct the appropriate base URL
    if test -n "$CODESPACES"; and test -n "$CODESPACE_NAME"; and test -n "$GITHUB_CODESPACES_PORT_FORWARDING_DOMAIN"
        set -l default_base "https://$CODESPACE_NAME-8888.$GITHUB_CODESPACES_PORT_FORWARDING_DOMAIN/api/tv"
    else
        set -l default_base "https://localhost:8888/api/tv"
    end
    
    set -l base $default_base
    set -l preview true
    set -l query_params
    set -l script_args

    for arg in $argv
        switch $arg
            case '--preview' 'preview'
                set preview true
            case '--no-preview' 'no-preview' '--json' 'json'
                set preview false
            case '--raw'
                set preview true
                set script_args $script_args '--raw'
            case '--max-width=*' '--maxwidth=*' '--max-height=*' '--maxheight=*' '--pause=*' '--pauseMs=*' '--pausems=*'
                set preview true
                set script_args $script_args $arg
            case '--base=*'
                set base (string replace -r '^--base=' '' -- $arg)
            case 'base=*'
                set base (string replace 'base=' '' -- $arg)
            case '--limit=*' '--types=*'
                set query_params $query_params (string replace -r '^--' '' -- $arg)
                set script_args $script_args $arg
            case 'limit=*' 'types=*'
                set query_params $query_params $arg
                set script_args $script_args $arg
            case '--*'
                # Forward unknown long flags to preview script if used
                set script_args $script_args $arg
            case '*=*'
                set query_params $query_params $arg
                set script_args $script_args $arg
            case '*'
                set query_params $query_params $arg
                set script_args $script_args $arg
        end
    end

    set -l url $base
    if test (count $query_params) -gt 0
        set -l query (string join '&' $query_params)
        set url "$base?"$query
    end

    if test $preview = true
        set -l script "/workspaces/aesthetic-computer/reference/tools/recording/tv-preview.mjs"
        if not test -f $script
            echo "❌ Preview script missing at $script" >&2
            return 1
        end

        if test $base != $default_base
            set script_args $script_args "--base=$base"
        end

        printf "📺 Preview %s\n" $url
        node --no-warnings $script $script_args
        return $status
    end

    printf "📺 GET %s\n" $url

    set -l response (curl --silent --show-error --insecure $url)
    set -l curl_status $status
    if test $curl_status -ne 0
        echo "❌ Failed to reach $url" >&2
        return 1
    end

    set -l body (string join "\n" $response)

    if type -q jq
        printf '%s\n' $body | jq
    else if type -q python3
        printf '%s\n' $body | python3 -m json.tool
    else
        printf '%s\n' $body
    end
end

# KidLisp Source Tree Tool - allows running source-tree from anywhere
# Usage: st cow, st $cow, st cow --source, st --test-colors, or st --test-css-colors
function st
    # Check for special commands first
    if test "$argv[1]" = "--test-colors"
        node --no-warnings /workspaces/aesthetic-computer/kidlisp/tools/source-tree.mjs --test-colors
        return
    end
    
    if test "$argv[1]" = "--test-css-colors"
        node --no-warnings /workspaces/aesthetic-computer/kidlisp/tools/source-tree.mjs --test-css-colors
        return
    end
    
    if test "$argv[1]" = "--debug-colors"
        node --no-warnings /workspaces/aesthetic-computer/kidlisp/tools/source-tree.mjs --debug-colors
        return
    end
    
    set -l piece_name $argv[1]
    set -l extra_args $argv[2..-1]
    
    # Remove $ prefix if present
    if string match -q '$*' $piece_name
        set piece_name (string sub -s 2 $piece_name)
    end
    
    # Check if --source flag is explicitly provided
    set -l has_source false
    for arg in $extra_args
        if test "$arg" = "--source"
            set has_source true
            break
        end
    end
    
    # Run the source-tree tool with clean output (no Node.js warnings)
    if test $has_source = true
        node --no-warnings /workspaces/aesthetic-computer/kidlisp/tools/source-tree.mjs $piece_name $extra_args
    else
        node --no-warnings /workspaces/aesthetic-computer/kidlisp/tools/source-tree.mjs $piece_name $extra_args
    end
end

# Alternative function for those who prefer the $ syntax
function dollarpiece
    st $argv
end

# AC Pack - Package pieces for Teia with cover GIF generation
# Usage: ac-pack '$ceo' or ac-pack 'ceo' [--density N] (from any directory)
function ac-pack
    if test (count $argv) -lt 1
        echo "Usage: ac-pack PIECE_NAME [--density N]"
        echo "Example: ac-pack '\$ceo' or ac-pack 'ceo'"
        echo "         ac-pack '\$bop' --density 8"
        return 1
    end
    
    set -l piece_name $argv[1]
    set -l current_dir (pwd)
    
    # Store original directory
    echo "📂 Packaging $piece_name from $current_dir"
    
    # Run the ac-pack.mjs script from the teia directory
    cd /workspaces/aesthetic-computer/teia
    
    # Run the packaging with all arguments plus target directory
    command node ac-pack.mjs $argv --target-dir "$current_dir"
    
    # Check if packaging was successful
    if test $status -eq 0
        echo "✅ AC Pack complete! Files created in $current_dir"
    else
        echo "❌ AC Pack failed with exit code $status"
    end
    
    # Return to original directory
    cd $current_dir
end

# AC Login - Authenticate with Aesthetic Computer (OAuth Device Code Flow)
# Usage: ac-login [status|logout|token]
# Stores access token in $AC_TOKEN environment variable for use in scripts
function ac-login
    set -l cmd $argv[1]
    
    if test "$cmd" = "status"
        node /workspaces/aesthetic-computer/tezos/ac-login.mjs status
        return $status
    else if test "$cmd" = "logout"
        node /workspaces/aesthetic-computer/tezos/ac-login.mjs logout
        set -e AC_TOKEN
        set -e AC_USER_EMAIL
        return 0
    else if test "$cmd" = "token"
        node /workspaces/aesthetic-computer/tezos/ac-login.mjs token
        return $status
    else
        # Login and capture token
        node /workspaces/aesthetic-computer/tezos/ac-login.mjs
        
        if test $status -eq 0
            # Read token from file and export to environment
            if test -f ~/.ac-token
                set -l token_json (cat ~/.ac-token)
                set -gx AC_TOKEN (echo $token_json | jq -r '.access_token')
                set -gx AC_USER_EMAIL (echo $token_json | jq -r '.user.email // .user.name // .user.sub')
                echo ""
                echo "🔑 Token exported to \$AC_TOKEN"
                echo "👤 User exported to \$AC_USER_EMAIL"
                echo ""
                echo "Example usage:"
                echo "  curl -H \"Authorization: Bearer \$AC_TOKEN\" https://aesthetic.computer/api/keep-mint"
            end
        end
        
        return $status
    end
end

# AC Token - Get current access token (shortcut)
# Usage: ac-token [--refresh]
function ac-token
    if test "$argv[1]" = "--refresh"
        # Re-read from file
        if test -f ~/.ac-token
            set -l token_json (cat ~/.ac-token)
            set -gx AC_TOKEN (echo $token_json | jq -r '.access_token')
            set -gx AC_USER_EMAIL (echo $token_json | jq -r '.user.email // .user.name // .user.sub')
            echo "✅ Token refreshed from ~/.ac-token"
        else
            echo "❌ No token file found. Run: ac-login"
            return 1
        end
    else if test -n "$AC_TOKEN"
        echo $AC_TOKEN
    else
        # Try to load from file
        if test -f ~/.ac-token
            set -l token_json (cat ~/.ac-token)
            set -gx AC_TOKEN (echo $token_json | jq -r '.access_token')
            echo $AC_TOKEN
        else
            echo "❌ Not logged in. Run: ac-login" >&2
            return 1
        end
    end
end

# AC Keeps - Interactive CLI for keeping KidLisp pieces on Tezos
# Usage: ac-keeps [list|keep|wallet|status] [args...]
# Commands:
#   ac-keeps              - Interactive mode
#   ac-keeps list         - List your pieces
#   ac-keeps list --top   - List by popularity
#   ac-keeps keep $code   - Keep a piece (you pay 5ꜩ)
#   ac-keeps wallet       - Connect Tezos wallet
#   ac-keeps status $code - Check if piece is kept
function ac-keeps
    node /workspaces/aesthetic-computer/tezos/ac-keeps.mjs $argv
end

# AC Keep - Create self-contained HTML bundles for Tezos KEEPS
# Usage: ac-keep '$bop' or ac-keep 'bop' (from any directory)
# Creates a single HTML file that runs offline with all dependencies embedded
# Filenames: @author-$piece-timestamp.html (matching bios.mjs download format)
function ac-keep
    if test (count $argv) -lt 1
        echo "Usage: ac-keep PIECE_NAME"
        echo "Example: ac-keep '\$bop' or ac-keep 'bop'"
        echo ""
        echo "Creates self-contained HTML bundles in tezos/keep-bundles/"
        echo "Output files (named like downloads from bios.mjs):"
        echo "  • \$piece-@author-timestamp.html (uncompressed)"
        echo "  • \$piece-@author-timestamp.brotli.html (Brotli, for Tezos)"
        echo "  • \$piece-@author-timestamp.lisp.html (gzip, for browser/drag-drop)"
        return 1
    end
    
    set -l piece_name $argv[1]
    # Remove $ prefix if present
    set piece_name (string replace -r '^\$' '' -- $piece_name)
    
    echo "📦 Creating KEEP bundle for \$$piece_name..."
    
    # Run the bundle-keep-html.mjs script and capture output
    set -l output (node /workspaces/aesthetic-computer/tezos/bundle-keep-html.mjs $piece_name 2>&1)
    set -l exit_status $status
    
    # Print the output
    echo $output
    
    if test $exit_status -eq 0
        echo ""
        echo "✅ KEEP bundle created!"
        echo "📁 Files in: /workspaces/aesthetic-computer/tezos/keep-bundles/"
    else
        echo "❌ KEEP bundle failed with exit code $exit_status"
    end
    
    return $exit_status
end

# AC Keep Test - Build and serve a KEEP bundle for testing
# Usage: ac-keep-test '$bop' or ac-keep-test 'bop'
function ac-keep-test
    if test (count $argv) -lt 1
        echo "Usage: ac-keep-test PIECE_NAME"
        echo "Example: ac-keep-test '\$bop'"
        return 1
    end
    
    set -l piece_name $argv[1]
    # Remove $ prefix if present
    set piece_name (string replace -r '^\$' '' -- $piece_name)
    
    # Build the bundle and capture output
    echo "📦 Creating KEEP bundle for \$$piece_name..."
    node /workspaces/aesthetic-computer/tezos/bundle-keep-html.mjs $piece_name
    
    if test $status -ne 0
        return 1
    end
    
    echo ""
    echo "🌐 Starting test server on http://localhost:8082..."
    echo "   Browse to http://localhost:8082/ and select the .gzip.html file"
    echo "   Press Ctrl+C to stop"
    echo ""
    
    # Start server in the keep-bundles directory
    cd /workspaces/aesthetic-computer/tezos/keep-bundles
    python3 -m http.server 8082
end

# Usage: ac-unpack [zip-file] [port] - unpacks and tests TEIA packages locally
# If no zip file specified, finds the most recent one in current directory
function ac-unpack
    set -l current_dir (pwd)
    set -l zip_file $argv[1]
    set -l port (test (count $argv) -ge 2; and echo $argv[2]; or echo "8080")
    
    echo "📦 Unpacking TEIA package from $current_dir"
    
    # Run the ac-unpack.mjs script from the teia directory
    cd /workspaces/aesthetic-computer/teia
    
    if test -n "$zip_file"
        # Zip file specified - use full path if it's in the original directory
        if not test -f "$zip_file"
            set zip_file "$current_dir/$zip_file"
        end
        echo "📂 Using specified file: $zip_file"
        node ac-unpack.mjs "$zip_file" $port "$current_dir"
    else
        # No zip file specified - let ac-unpack find the latest one
        echo "🔍 Looking for latest zip file in $current_dir..."
        node ac-unpack.mjs "" $port "$current_dir"
    end
    
    # Return to original directory
    cd $current_dir
    
    if test $status -eq 0
        echo "✅ AC Unpack complete! Server running on port $port"
        echo "🌐 Open http://localhost:$port in your browser to test"
    else
        echo "❌ AC Unpack failed with exit code $status"
    end
end

# Usage: ac-ship [zip-file] [--platforms mac,windows,linux] - packages TEIA zips as Electron desktop apps
# If no zip file specified, finds the most recent one in current directory
function ac-ship
    set -l current_dir (pwd)
    set -l args $argv
    
    echo "🚢 Shipping Electron apps from $current_dir"
    
    # Run the ac-ship.mjs script from the teia directory
    cd /workspaces/aesthetic-computer/teia
    
    # Process arguments to handle relative paths
    set -l processed_args
    for arg in $args
        if not string match -q -- '--*' $arg; and test -f "$current_dir/$arg"
            # This looks like a zip file in the original directory
            set processed_args $processed_args "$current_dir/$arg"
        else
            set processed_args $processed_args $arg
        end
    end
    
    node ac-ship.mjs $processed_args
    
    # Return to original directory
    cd $current_dir
    
    if test $status -eq 0
        echo "✅ AC Ship complete! Electron apps built successfully"
        echo "📱 Check the output for app locations"
    else
        echo "❌ AC Ship failed with exit code $status"
    end
end

# AC Record - Record pieces as MP4 or GIF using the orchestrator
# Usage: ac-record PIECE_NAME [--duration=<value>] [--width N] [--height N] [--gif] [--sixel]
# Duration accepts suffixes: `s` for seconds (e.g. 3s) or `f` for frames (e.g. 120f). Default: 5s (300f).
# Example: ac-record '$bair' --duration=3s or ac-record 'orbital' --duration=120f --width 512 --height 512 --gif
function ac-record
    if test (count $argv) -lt 1
        echo "Usage: ac-record PIECE_NAME [OPTIONS]"
        echo "Example: ac-record '$bair'"
        echo "         ac-record 'orbital' --duration=120f --width 512 --height 512"
        echo "         ac-record '$ceo' --gif --duration=3s"
        echo ""
        echo "Options:"
        echo "  --duration VAL  Recording length. Append 's' for seconds or 'f' for frames"
        echo "                 (default: 5s = 300f). Plain numbers are treated as frames."
        echo "  --width N       Video width (default: 1024)"
        echo "  --height N      Video height (default: 1024)"
        echo "  --gif           Output as GIF instead of MP4"
        echo "  --gif-fps N     Override GIF framerate (default: 50, allowed: 100,50,25,20,10,5,4,2,1)"
        echo "  --gif-25        Convenience flag for 25fps GIF output"
        echo "  --gif-compress  Ultra-compress GIF for smallest file size"
        echo "  --sixel         Display result in terminal as sixel image"
        return 1
    end

    set -l piece_name $argv[1]
    set -l current_dir (pwd)

    set -l frames 300  # ≈5 seconds at 60fps
    set -l duration_seconds "5.00"
    set -l width 1024
    set -l height 1024
    set -l gif_flag ""
    set -l sixel_flag ""
    set -l duration_spec ""
    set -l density ""
    set -l gif_fps ""
    set -l gif_compress ""
    
    # Process optional arguments
    set -l i 2
    while test $i -le (count $argv)
        set -l arg $argv[$i]
        switch $arg
            case '--duration=*'
                set duration_spec (string split -m 1 '=' -- $arg)[2]
            case '--duration'
                set i (math $i + 1)
                if test $i -le (count $argv)
                    set duration_spec $argv[$i]
                else
                    echo "❌ Missing value for --duration" >&2
                    cd $current_dir
                    return 1
                end
            case '--frames'
                set i (math $i + 1)
                if test $i -le (count $argv)
                    set -l frames_arg $argv[$i]
                    set duration_spec (string join '' $frames_arg 'f')
                    printf "⚠️  --frames is deprecated; use --duration=%sf instead\n" $frames_arg >&2
                else
                    echo "❌ Missing value for --frames" >&2
                    cd $current_dir
                    return 1
                end
            case '--width'
                set i (math $i + 1)
                if test $i -le (count $argv)
                    set width $argv[$i]
                end
            case '--height'
                set i (math $i + 1)
                if test $i -le (count $argv)
                    set height $argv[$i]
                end
            case '--density'
                set i (math $i + 1)
                if test $i -le (count $argv)
                    set density $argv[$i]
                end
            case '--gif'
                set gif_flag "--gif"
            case '--sixel'
                set sixel_flag "sixel"
            case '--gif-fps'
                set i (math $i + 1)
                if test $i -le (count $argv)
                    set gif_fps $argv[$i]
                end
            case '--gif-25'
                set gif_fps 25
            case '--gif25'
                set gif_fps 25
            case '--gif-compress'
                set gif_compress "--gif-compress"
            case '*'
                echo "⚠️  Ignoring unknown option: $arg" >&2
        end
        set i (math $i + 1)
    end

    if test -n "$duration_spec"
        set duration_spec (string trim (string lower $duration_spec))
        set -l parse_result (python3 -c "import sys
spec = sys.argv[1].strip().lower()
if not spec:
    raise SystemExit(1)
if spec.endswith('s'):
    seconds = float(spec[:-1] or '0')
    frames = int(round(seconds * 60))
elif spec.endswith('f'):
    frames = int(round(float(spec[:-1] or '0')))
    seconds = frames / 60.0
else:
    frames = int(round(float(spec)))
    seconds = frames / 60.0
if frames <= 0:
    raise SystemExit(1)
print(frames)
print(f'{seconds:.2f}')" $duration_spec)

        if test $status -ne 0
            echo "❌ Invalid --duration value: $duration_spec" >&2
            cd $current_dir
            return 1
        end

        set frames $parse_result[1]
        set duration_seconds $parse_result[2]
    end

    if test $frames -le 0
        echo "❌ Duration must be greater than 0" >&2
        cd $current_dir
        return 1
    end

    echo "🎬 Recording $piece_name from $current_dir"
    echo "📊 Settings: {$frames} frames (~{$duration_seconds}s) @ {$width}x{$height}"
    if test -n "$gif_flag"
        if test -n "$gif_fps"
            printf "🎞️  Output format: GIF @ %sfps\n" $gif_fps
        else
            echo "🎞️  Output format: GIF @ 50fps (default)"
        end
    else
        echo "🎞️  Output format: MP4"
    end
    if test -n "$density"
        echo "🔍 Density: $density"
        set -l final_width (math "$width * $density")
        set -l final_height (math "$height * $density")
        set final_width (string replace -r '\.0+$' '' $final_width)
        set final_height (string replace -r '\.0+$' '' $final_height)
        printf "📐 Output pixels: %sx%s\n" $final_width $final_height
    end
    
    # Run the orchestrator from the recording directory
    cd /workspaces/aesthetic-computer/reference/tools/recording

    # Pass the original duration spec to orchestrator, or default if none specified
    set -l duration_arg
    if test -n "$duration_spec"
        set duration_arg $duration_spec
    else
        set duration_arg "5s"  # Default to 5 seconds
    end

    set -l cmd "node" "orchestrator.mjs" $piece_name $duration_arg $width $height
    if test -n "$gif_flag"
        set cmd $cmd $gif_flag
    end
    if test -n "$sixel_flag"
        set cmd $cmd $sixel_flag
    end
    if test -n "$density"
        set cmd $cmd "--density" $density
    end
    if test -n "$gif_fps"
        set cmd $cmd "--gif-fps" $gif_fps
    end
    if test -n "$gif_compress"
        set cmd $cmd $gif_compress
    end
    
    # Execute the recording
    command $cmd

    if test $status -eq 0
        echo "✅ AC Record complete! Video created in output directory"

        set -l output_base_dir "/workspaces/aesthetic-computer/reference/tools/output"
        set -l piece_clean (string replace '$' '' $piece_name)

        if test -n "$gif_flag"
            set -l latest_gif (find $output_base_dir -name "*$piece_clean*.gif" -type f -printf "%T@ %p\n" 2>/dev/null | sort -nr | head -n1 | cut -d' ' -f2-)
            if test -n "$latest_gif"
                cp "$latest_gif" "$current_dir/"
                set -l filename (basename "$latest_gif")
                echo "📁 Copied $filename to $current_dir"
            end
        else
            set -l latest_mp4 (find $output_base_dir -name "*$piece_clean*.mp4" -type f -printf "%T@ %p\n" 2>/dev/null | sort -nr | head -n1 | cut -d' ' -f2-)
            if test -n "$latest_mp4"
                cp "$latest_mp4" "$current_dir/"
                set -l filename (basename "$latest_mp4")
                echo "📁 Copied $filename to $current_dir"
            end
        end
    else
        echo "❌ AC Record failed with exit code $status"
    end

    cd $current_dir
end

# 🩸 Artery - Direct connection to Aesthetic Computer workbench
# Interactive REPL for controlling AC, seeing console logs, and executing JavaScript
# Usage: artery [command] [args...]
# Examples:
#   artery                  # Show help
#   artery jump prompt      # Navigate to prompt piece
#   artery current          # Show current piece
#   artery repl             # Interactive REPL mode
function artery
    if test (count $argv) -eq 0
        # No args - show help
        node /workspaces/aesthetic-computer/artery/artery.mjs
    else
        # Pass through to artery
        node /workspaces/aesthetic-computer/artery/artery.mjs $argv
    end
end

# 🩸 AC REPL - Quick shortcut to artery REPL mode
# Live JavaScript console with AC console logs
# Usage: ac-repl
# Commands in REPL:
#   .jump <piece>   - Navigate to piece
#   .current        - Show current piece
#   .panel [action] - Control AC panel
#   .exit           - Close artery
function ac-repl
    echo "🩸 Starting Artery REPL - Live connection to Aesthetic Computer"
    echo "   💉 Console logs will appear in real-time"
    echo "   🎯 Type .jump <piece> to navigate"
    echo "   🔍 Type JavaScript to execute in AC context"
    echo ""
    artery repl
end

# 🩸 Artery TUI - Interactive curses-style interface for AC
# Usage: ac-artery or artery-tui
# Features:
#   - Menu-driven interface with keyboard navigation
#   - Open AC panel, jump to pieces, run tests
#   - Live REPL with console log output
#   - WebGPU performance monitoring
function ac-artery
    node /workspaces/aesthetic-computer/artery/artery-tui.mjs
end

# Artery with hot-reload for development
# Watches artery-tui.mjs and artery.mjs for changes and auto-restarts
function ac-artery-dev
    node /workspaces/aesthetic-computer/artery/artery-dev.mjs
end

function artery-tui
    ac-artery
end

# Automated testing for AC pieces
# Usage: test-notepat [duration_ms]
# Example: test-notepat 60000  (runs for 60 seconds)
function test-notepat
    node /workspaces/aesthetic-computer/.vscode/tests/test-notepat.mjs $argv
end

# Test line drawing tool
# Usage: test-line [duration_ms]
# Example: test-line 30000  (runs for 30 seconds)
function test-line
    node /workspaces/aesthetic-computer/.vscode/tests/test-line.mjs $argv
end

# Test toss piece
# Usage: test-toss [duration_ms]
# Example: test-toss 5000  (runs for 5 seconds)
function test-toss
  node /workspaces/aesthetic-computer/.vscode/tests/test-toss.mjs $argv
end

function test-melody
  node /workspaces/aesthetic-computer/.vscode/tests/test-melody.mjs $argv
end

function test-playlist
  node /workspaces/aesthetic-computer/.vscode/tests/test-playlist.mjs $argv
end

function test-chords
  node /workspaces/aesthetic-computer/.vscode/tests/test-chords.mjs $argv
end

function test-generative-waltz
  node /workspaces/aesthetic-computer/.vscode/tests/test-generative-waltz.mjs $argv
end

# 🌐 KidLisp.com Probe - Control kidlisp.com in Simple Browser via CDP
# Usage:
#   kidlisp                    # Show status
#   kidlisp eval <js>          # Evaluate JS in kidlisp.com
#   kidlisp theme              # Toggle theme
#   kidlisp clear              # Clear editor
#   kidlisp set <code>         # Set editor content
#   kidlisp play               # Press play button
function kidlisp
    node /workspaces/aesthetic-computer/kidlisp-tools/kidlisp-probe.mjs $argv
end

# always start in aesthetic-computer directory if there was a greeting
if not test "$nogreet" = true
    cd ~/aesthetic-computer
end

# rebuild the container after exiting with a special code ;)

# alias reload 'exit 70'

# reload fish config
alias reload 'source /workspaces/aesthetic-computer/.devcontainer/config.fish'
alias refish 'source ~/.config/fish/config.fish'

# Workaround for fish script execution on SELinux/btrfs bind mounts
# The "No such file or directory" error happens when fish tries to execute
# scripts via shebang on /workspaces (which is a bind mount from host).
# Use `run script.fish` instead of `./script.fish` or `fish script.fish`
function run --description "Run a fish script using source (workaround for bind mount issue)"
    if test (count $argv) -eq 0
        echo "Usage: run <script.fish> [args...]"
        return 1
    end
    set -l script $argv[1]
    set -l args $argv[2..-1]
    if not test -f $script
        echo "File not found: $script"
        return 1
    end
    # Use source to run the script in current shell
    source $script $args
end

# Port visibility helper - sets port 8888 to public in Codespaces
alias port-public 'source /workspaces/aesthetic-computer/.devcontainer/scripts/set-port-public.fish'

# SSH to Windows host
alias win 'ssh me@host.docker.internal'

# set default editor to emacs
set -gx TERM xterm-256color
set -gx EDITOR emacs
set -gx PATH $PATH /home/me/.local/bin

# add dotnet tools to path
set -Ux PATH $PATH $HOME/.dotnet/tools
# add stuff to path
# set -gx PATH $PATH $HOME/isomorphic_copy/bin
# use temporary clipboard file
# set -gx ISOCP_USE_FILE 1
# set -gx DISPLAY FAKE

set -gx DENO_INSTALL /home/me/.deno
set -gx PATH $PATH $DENO_INSTALL/bin

# enable vi support
set fish_cursor_default block
set fish_cursor_insert line
set fish_cursor_replace_one underscore
set fish_vi_force_cursor true
fish_vi_key_bindings

# add homebrew to path (only if we are on linux)
#switch (uname)
#    case Linux
#        eval (/home/linuxbrew/.linuxbrew/bin/brew shellenv)
#end

# include user binaries in the shell path
fish_add_path ~/.local/bin

# add rust binaries to the shell path
fish_add_path ~/.cargo/bin

# add nanos ops binaries to the shell path
fish_add_path ~/.ops/bin

# ============================================================================
# AC Emacs Management Commands
# ============================================================================

# Emacs log directory
set -gx AC_EMACS_LOG_DIR /home/me/.emacs-logs

# Initialize emacs log directory
function ac-emacs-init-logs
    mkdir -p $AC_EMACS_LOG_DIR
end

# Kill zombie emacs daemon and restart fresh
function ac-emacs-restart
    echo "🔄 Restarting emacs daemon..."
    ac-emacs-init-logs
    
    # Kill any existing emacs processes
    pkill -9 -f "emacs.*daemon" 2>/dev/null
    pkill -9 emacs 2>/dev/null
    pkill -9 emacsclient 2>/dev/null
    sleep 1
    
    # Start fresh daemon with logging
    echo "🚀 Starting fresh emacs daemon..."
    set -l timestamp (date +%Y%m%d_%H%M%S)
    set -l log_file $AC_EMACS_LOG_DIR/daemon_$timestamp.log
    set -l config_path /home/me/aesthetic-computer/dotfiles/dot_config/emacs.el
    
    # Log startup info
    echo "=== Emacs Daemon Start ===" > $log_file
    echo "Timestamp: "(date -Iseconds) >> $log_file
    echo "Config: $config_path" >> $log_file
    echo "===========================" >> $log_file
    
    # Start daemon in background with timeout (don't use tee - it hangs)
    emacs -q --daemon -l $config_path >> $log_file 2>&1 &
    set -l daemon_pid $last_pid
    
    # Create/update symlink to latest log
    ln -sf $log_file $AC_EMACS_LOG_DIR/latest.log
    
    # Wait for daemon to be responsive (max 30 seconds)
    echo "⏳ Waiting for daemon..."
    set -l attempts 0
    while test $attempts -lt 30
        if timeout 2 emacsclient -e t >/dev/null 2>&1
            echo "✅ Emacs daemon ready!"
            echo "📝 Logs: $log_file"
            
            # Start the crash monitor in background
            fish -c "ac-emacs-crash-monitor" &>/dev/null &
            disown
            
            echo ""
            echo "💡 To connect UI: aesthetic-now (or ac-aesthetic)"
            return 0
        end
        sleep 1
        set attempts (math $attempts + 1)
    end
    
    echo "❌ Emacs daemon failed to start (timeout)"
    echo "📝 Check logs: $log_file"
    return 1
end

# Full restart including UI reconnection
function ac-emacs-full-restart
    ac-emacs-restart
    and aesthetic-now
end

# Health check - verify emacs has correct config loaded
function ac-emacs-health-check
    if not pgrep -f "emacs.*daemon" >/dev/null
        echo "❌ Emacs daemon not running"
        return 1
    end
    
    if not timeout 3 emacsclient -e t >/dev/null 2>&1
        echo "❌ Emacs daemon unresponsive"
        return 1
    end
    
    # Check if our config functions are defined
    set -l has_backend (emacsclient -e '(fboundp (quote aesthetic-backend))' 2>/dev/null)
    if test "$has_backend" != "t"
        echo "❌ Emacs loaded wrong config (aesthetic-backend not defined)"
        echo "   This usually means crash monitor restarted emacs incorrectly"
        echo "   Run: ac-emacs-restart"
        return 1
    end
    
    # Check if backend has run
    set -l backend_started (emacsclient -e 'ac--backend-started' 2>/dev/null)
    echo "✅ Emacs daemon healthy"
    echo "   Config: aesthetic-backend defined"
    echo "   Backend started: $backend_started"
    return 0
end

# Crash diary viewer - live tail of crash log with pretty formatting
function ac-crash-diary
    ac-emacs-init-logs
    set -l crash_log $AC_EMACS_LOG_DIR/crashes.log
    
    clear
    echo ""
    echo "  ╔══════════════════════════════════════════════════════════════╗"
    echo "  ║  💥 EMACS CRASH DIARY                                        ║"
    echo "  ║  Log: $crash_log"
    echo "  ╚══════════════════════════════════════════════════════════════╝"
    echo ""
    
    if not test -f $crash_log
        echo "  📭 No crashes recorded yet. That's good!"
        echo ""
        echo "  Press Ctrl+C to exit, or wait for crashes to be logged..."
        echo ""
        # Create empty file and watch it
        touch $crash_log
    else
        # Show summary
        set -l crash_count (grep -c "EMACS CRASH DETECTED" $crash_log 2>/dev/null || echo 0)
        set -l restart_count (grep -c "Auto-restarting emacs" $crash_log 2>/dev/null || echo 0)
        set -l last_crash (grep "EMACS CRASH DETECTED" $crash_log 2>/dev/null | tail -1)
        
        echo "  📊 Summary:"
        echo "     Total crashes: $crash_count"
        echo "     Auto-restarts: $restart_count"
        echo ""
        echo "  ────────────────────────────────────────────────────────────"
        echo ""
    end
    
    # Live tail with syntax highlighting
    tail -f $crash_log 2>/dev/null | while read -l line
        # Color code different types of lines
        if string match -q "*CRASH DETECTED*" "$line"
            set_color red --bold
            echo "  $line"
            set_color normal
        else if string match -q "*Auto-restarting*" "$line"
            set_color yellow
            echo "  $line"
            set_color normal
        else if string match -q "*✅*" "$line"
            set_color green
            echo "  $line"
            set_color normal
        else if string match -q "*❌*" "$line"
            set_color red
            echo "  $line"
            set_color normal
        else if string match -q "*===*" "$line"
            set_color cyan
            echo "  $line"
            set_color normal
        else if string match -q "*---*" "$line"
            set_color brblack
            echo "  $line"
            set_color normal
        else
            echo "  $line"
        end
    end
end

# Background crash monitor - detects when emacs dies unexpectedly
function ac-emacs-crash-monitor
    ac-emacs-init-logs
    set -l crash_log $AC_EMACS_LOG_DIR/crashes.log
    set -l check_interval 5  # Check every 5 seconds
    set -l max_restarts 5    # Max restarts before giving up
    set -l restart_count 0
    set -l restart_window 300  # Reset restart count after 5 mins of stability
    set -l last_restart_time 0
    
    # Record that we started monitoring
    echo "["(date -Iseconds)"] 🔍 Crash monitor started (auto-restart enabled)" >> $crash_log
    
    while true
        # Get current PID
        set -l daemon_pid (pgrep -f "emacs.*daemon" 2>/dev/null | head -1)
        
        if test -z "$daemon_pid"
            # Daemon is gone - log the crash
            echo "" >> $crash_log
            echo "============================================" >> $crash_log
            echo "🔴 EMACS CRASH DETECTED" >> $crash_log
            echo "============================================" >> $crash_log
            echo "Time: "(date -Iseconds) >> $crash_log
            echo "" >> $crash_log
            
            # Try to capture crash details
            echo "--- System State ---" >> $crash_log
            echo "Memory:" >> $crash_log
            free -h 2>/dev/null >> $crash_log
            echo "" >> $crash_log
            echo "Load:" >> $crash_log
            uptime >> $crash_log
            echo "" >> $crash_log
            
            # Check dmesg for OOM or signals
            echo "--- Kernel messages (last 30s) ---" >> $crash_log
            dmesg --time-format=iso 2>/dev/null | tail -30 | grep -iE "oom|kill|emacs|signal|memory|segfault" >> $crash_log
            echo "" >> $crash_log
            
            # Check journalctl for emacs-related messages
            echo "--- Journal (emacs) ---" >> $crash_log
            journalctl --since "1 minute ago" 2>/dev/null | grep -i emacs >> $crash_log
            echo "" >> $crash_log
            
            # Check if there's a core dump
            echo "--- Core dumps ---" >> $crash_log
            coredumpctl list 2>/dev/null | grep -i emacs | tail -3 >> $crash_log
            echo "" >> $crash_log
            
            # Check emacs daemon log if it exists
            set -l latest_log (ls -t $AC_EMACS_LOG_DIR/daemon_*.log 2>/dev/null | head -1)
            if test -n "$latest_log"
                echo "--- Last daemon log (last 50 lines) ---" >> $crash_log
                tail -50 "$latest_log" >> $crash_log
            end
            echo "============================================" >> $crash_log
            
            # Reset restart count if enough time has passed
            set -l now (date +%s)
            if test (math "$now - $last_restart_time") -gt $restart_window
                set restart_count 0
            end
            
            # Check if we should auto-restart
            if test $restart_count -lt $max_restarts
                set restart_count (math "$restart_count + 1")
                set last_restart_time $now
                
                echo "["(date -Iseconds)"] 🔄 Auto-restarting emacs (attempt $restart_count/$max_restarts)..." >> $crash_log
                
                # Create warning file
                echo "Emacs crashed at "(date -Iseconds)", auto-restart #$restart_count" > /tmp/emacs-crash-warning
                
                # Remove any stale startup lock from previous session
                rm -f /tmp/emacs-backend-startup.lock
                
                # Clean up any zombie processes
                pkill -9 emacs 2>/dev/null
                pkill -9 emacsclient 2>/dev/null
                sleep 1
                
                # Restart daemon with correct config (must use -q -l to load our config, not ~/.emacs)
                set -l config_path /home/me/aesthetic-computer/dotfiles/dot_config/emacs.el
                emacs -q --daemon -l $config_path 2>&1 | tee -a $crash_log
                
                # Wait for it to come up
                sleep 2
                
                set -l new_pid (pgrep -f "emacs.*daemon" 2>/dev/null | head -1)
                if test -n "$new_pid"
                    echo "["(date -Iseconds)"] ✅ Emacs daemon restarted (PID: $new_pid)" >> $crash_log
                    
                    # Auto-start aesthetic-backend after crash recovery
                    # This runs in background so it doesn't block the monitor
                    echo "["(date -Iseconds)"] 🚀 Starting aesthetic-backend..." >> $crash_log
                    fish -c "sleep 3 && emacsclient -e '(aesthetic-backend)'" &>/dev/null &
                    disown
                else
                    echo "["(date -Iseconds)"] ❌ Failed to restart emacs daemon" >> $crash_log
                end
            else
                echo "["(date -Iseconds)"] ⛔ Max restarts ($max_restarts) reached, stopping monitor" >> $crash_log
                echo "Emacs crashed repeatedly - manual intervention needed" > /tmp/emacs-crash-warning
                break
            end
        else
            # Daemon is running - also check if it's responsive
            # BUT skip responsiveness check if aesthetic-backend is still starting up
            set -l startup_lock /tmp/emacs-backend-startup.lock
            if test -f $startup_lock
                # Check how old the lock file is (don't wait forever if it's stale)
                set -l lock_age (math (date +%s) - (stat -c %Y $startup_lock 2>/dev/null; or echo 0))
                if test $lock_age -lt 120  # Lock valid for 2 minutes max
                    # Startup in progress - skip responsiveness check, just log
                    echo "["(date -Iseconds)"] ⏳ Startup lock active ($lock_age""s) - skipping responsiveness check" >> $crash_log
                else
                    # Stale lock - remove it and check responsiveness
                    echo "["(date -Iseconds)"] ⚠️ Stale startup lock ($lock_age""s) - removing" >> $crash_log
                    rm -f $startup_lock
                    if not timeout 10 emacsclient -e t >/dev/null 2>&1
                        echo "["(date -Iseconds)"] ⚠️ Daemon unresponsive (PID: $daemon_pid), forcing restart..." >> $crash_log
                        pkill -9 -f "emacs.*daemon" 2>/dev/null
                    end
                end
            else
                # No startup lock - normal responsiveness check (increased timeout)
                if not timeout 10 emacsclient -e t >/dev/null 2>&1
                    echo "["(date -Iseconds)"] ⚠️ Daemon unresponsive (PID: $daemon_pid), forcing restart..." >> $crash_log
                    pkill -9 -f "emacs.*daemon" 2>/dev/null
                    # Loop will detect it's gone and restart
                end
            end
        end
        
        sleep $check_interval
    end
end

# Kill emacs entirely (daemon + any clients)
function ac-emacs-kill
    echo "💀 Killing all emacs processes..."
    # Also kill the crash monitor
    pkill -f "ac-emacs-crash-monitor" 2>/dev/null
    pkill -9 -f "emacs.*daemon" 2>/dev/null
    pkill -9 emacs 2>/dev/null
    pkill -9 emacsclient 2>/dev/null
    echo "✅ All emacs processes killed"
end

# Check emacs daemon status
function ac-emacs-status
    set -l daemon_pid (pgrep -f "emacs.*daemon" 2>/dev/null)
    if test -n "$daemon_pid"
        echo "🟢 Emacs daemon running (PID: $daemon_pid)"
        if timeout 3 emacsclient -e t >/dev/null 2>&1
            echo "✅ Daemon is responsive"
        else
            echo "⚠️  Daemon process exists but NOT responsive (zombie)"
        end
    else
        echo "🔴 Emacs daemon not running"
        # Check for crash warning
        if test -f /tmp/emacs-crash-warning
            echo "⚠️  Last crash: "(cat /tmp/emacs-crash-warning)
        end
    end
    
    # Check crash monitor status
    set -l monitor_pid (pgrep -f "ac-emacs-crash-monitor" 2>/dev/null)
    if test -n "$monitor_pid"
        echo "🔍 Crash monitor active (PID: $monitor_pid)"
    else
        echo "⚠️  Crash monitor not running"
    end
end

# Start the crash monitor manually
function ac-emacs-start-monitor
    if pgrep -f "ac-emacs-crash-monitor" >/dev/null 2>&1
        echo "Crash monitor already running"
        return 0
    end
    
    if not pgrep -f "emacs.*daemon" >/dev/null 2>&1
        echo "❌ No emacs daemon to monitor"
        return 1
    end
    
    echo "🔍 Starting crash monitor..."
    fish -c "ac-emacs-crash-monitor" &>/dev/null &
    disown
    echo "✅ Crash monitor started"
end

# View emacs crash logs
function ac-emacs-logs
    ac-emacs-init-logs
    if test "$argv[1]" = "crashes"
        if test -f $AC_EMACS_LOG_DIR/crashes.log
            bat $AC_EMACS_LOG_DIR/crashes.log 2>/dev/null || cat $AC_EMACS_LOG_DIR/crashes.log
        else
            echo "No crash logs found"
        end
    else if test "$argv[1]" = "latest"
        if test -f $AC_EMACS_LOG_DIR/latest.log
            bat $AC_EMACS_LOG_DIR/latest.log 2>/dev/null || cat $AC_EMACS_LOG_DIR/latest.log
        else
            echo "No daemon logs found"
        end
    else if test "$argv[1]" = "tail"
        if test -f $AC_EMACS_LOG_DIR/crashes.log
            tail -f $AC_EMACS_LOG_DIR/crashes.log
        else
            echo "No crash logs found"
        end
    else if test "$argv[1]" = "list"
        echo "📁 Emacs logs in $AC_EMACS_LOG_DIR:"
        ls -lth $AC_EMACS_LOG_DIR/ 2>/dev/null || echo "No logs directory"
    else
        echo "Usage: ac-emacs-logs [crashes|latest|tail|list]"
        echo ""
        echo "  crashes - View crash history"
        echo "  latest  - View latest daemon startup log"  
        echo "  tail    - Follow crash log in real-time"
        echo "  list    - List all log files"
    end
end

# Clear the crash warning
function ac-emacs-ack
    rm -f /tmp/emacs-crash-warning
    echo "✅ Crash warning cleared"
end

# === SIXEL IMAGE DISPLAY ===

# Display an image (URL or local path) as sixel in the emacs terminal
# Usage: ac-pix <url-or-path> [size]
# Examples:
#   ac-pix https://example.com/image.png
#   ac-pix /tmp/my-image.jpg
#   ac-pix https://example.com/image.png 80  (80 pixels wide)
function ac-pix
    if test (count $argv) -lt 1
        echo "Usage: ac-pix <url-or-path> [size]"
        echo ""
        echo "Display an image as sixel graphics in the emacs terminal."
        echo ""
        echo "Arguments:"
        echo "  url-or-path  URL (http/https/ipfs) or local file path"
        echo "  size         Max pixel dimension (default: 60)"
        echo ""
        echo "Examples:"
        echo "  ac-pix https://example.com/photo.png"
        echo "  ac-pix /tmp/thumbnail.jpg 100"
        echo "  ac-pix ipfs://Qm... 40"
        return 1
    end

    set -l input $argv[1]
    set -l size 60
    if test (count $argv) -ge 2
        set size $argv[2]
    end

    # Find emacsclient PTY for direct sixel output
    set -l pty (pgrep -f 'emacsclient.*-nw' | head -1 | xargs -I{} readlink /proc/{}/fd/0 2>/dev/null)
    if test -z "$pty"
        echo "❌ No emacsclient terminal found"
        return 1
    end

    set -l tmpfile /tmp/ac-pix-(date +%s%N).img

    # Handle different input types
    if string match -rq '^https?://' -- $input
        # HTTP/HTTPS URL
        echo "📥 Fetching $input..."
        if not curl -sL --max-time 15 -o $tmpfile $input
            echo "❌ Failed to download image"
            return 1
        end
    else if string match -rq '^ipfs://' -- $input
        # IPFS URL - try multiple gateways
        set -l cid (string replace 'ipfs://' '' $input)
        set -l gateways \
            "https://cloudflare-ipfs.com/ipfs/" \
            "https://ipfs.io/ipfs/" \
            "https://gateway.pinata.cloud/ipfs/" \
            "https://w3s.link/ipfs/"
        
        set -l downloaded 0
        for gateway in $gateways
            echo "📥 Trying $gateway..."
            if curl -sL --max-time 10 -o $tmpfile "$gateway$cid" 2>/dev/null
                if test -s $tmpfile
                    set downloaded 1
                    break
                end
            end
        end
        
        if test $downloaded -eq 0
            echo "❌ Failed to fetch from IPFS"
            return 1
        end
    else
        # Local file path
        if not test -f $input
            echo "❌ File not found: $input"
            return 1
        end
        set tmpfile $input
    end

    # Verify file has content
    if not test -s $tmpfile
        echo "❌ Empty or invalid image"
        test "$tmpfile" != "$input"; and rm -f $tmpfile
        return 1
    end

    # Convert to sixel and write directly to emacsclient's terminal
    # Note: [0] is ImageMagick syntax for first frame, needs quoting to avoid fish array interpretation
    set -l sixel (/usr/bin/magick "$tmpfile"'[0]' -resize "$size"x"$size>" sixel:- 2>/dev/null)
    if test -z "$sixel"
        echo "❌ Failed to convert image to sixel"
        test "$tmpfile" != "$input"; and rm -f $tmpfile
        return 1
    end

    # Clean up temp file if we created one
    test "$tmpfile" != "$input"; and rm -f $tmpfile

    # Output newline + sixel directly to emacsclient's PTY
    printf '\n%s\n' $sixel > $pty
    echo "" # Add space after sixel in eat
end

# Full aesthetic platform restart (emacs + reconnect artery)
function ac-restart
    echo "🔄 Full aesthetic platform restart..."
    ac-emacs-restart
    if test $status -eq 0
        echo "🩸 Reconnecting to artery..."
        emacsclient -nw -c --eval '(aesthetic-backend (quote "artery"))'
    end
end

# === EMACS TAB TESTING & DIAGNOSTICS ===

# Run emacs tab unit tests
function ac-test-tabs
    /workspaces/aesthetic-computer/scripts/test-emacs-tabs.fish $argv
end

# Quick diagnose current daemon state
function ac-diagnose
    if timeout 3 emacsclient -e '(ac-diagnose-all)' >/dev/null 2>&1
        echo "✅ Diagnostics logged to .emacs-logs/emacs-debug.log"
        echo ""
        tail -30 /workspaces/aesthetic-computer/.emacs-logs/emacs-debug.log | grep -E "DIAGNOSE|BUFFER|TIMER|PROC"
    else
        echo "❌ Daemon not responsive - cannot run diagnostics"
    end
end

# Start emacs CPU profiler
function ac-profile-start
    if emacsclient -e '(ac-profile-start)' >/dev/null 2>&1
        echo "🔬 CPU profiler started"
    else
        echo "❌ Could not start profiler - daemon not responsive?"
    end
end

# Stop profiler and show summary
function ac-profile-stop
    if emacsclient -e '(ac-profile-stop)' >/dev/null 2>&1
        echo "🔬 Profiler stopped - report in .emacs-logs/emacs-profile.log"
        echo ""
        if test -f /workspaces/aesthetic-computer/.emacs-logs/emacs-profile.log
            tail -50 /workspaces/aesthetic-computer/.emacs-logs/emacs-profile.log
        end
    else
        echo "❌ Could not stop profiler - daemon not responsive?"
    end
end

# Show emacs profiler report interactively
function ac-profile-report
    emacsclient -e '(ac-profile-report)'
end

# Watch daemon CPU in real-time
function ac-watch-cpu
    echo "Watching emacs CPU usage (Ctrl+C to stop)..."
    while true
        set cpu (ps aux | grep "emacs.*daemon" | grep -v grep | awk '{print $3}')
        set mem (ps aux | grep "emacs.*daemon" | grep -v grep | awk '{print $4}')
        set responsive "?"
        if timeout 1 emacsclient -e t >/dev/null 2>&1
            set responsive "✓"
        else
            set responsive "✗"
        end
        echo (date '+%H:%M:%S')" CPU: $cpu% MEM: $mem% Responsive: $responsive"
        sleep 2
    end
end

# Start/restart CDP tunnel for VS Code control
function ac-cdp-tunnel
    echo "🩸 Managing CDP tunnel..."
    
    # Kill any existing tunnel
    set -l existing (pgrep -f "ssh.*9333.*(host.docker.internal|172.17.0.1)" 2>/dev/null)
    if test -n "$existing"
        echo "  Killing existing tunnel (PID: $existing)..."
        kill $existing 2>/dev/null
        sleep 1
    end
    
    # Determine host address - try host.docker.internal first, fallback to gateway
    set -l host_addr "host.docker.internal"
    if not getent hosts host.docker.internal >/dev/null 2>&1
        # Use Docker gateway IP as fallback
        set host_addr "172.17.0.1"
    end
    
    # Start new tunnel
    echo "  Starting new CDP tunnel to $host_addr..."
    ssh -f -N -o StrictHostKeyChecking=no -o BatchMode=yes -o ConnectTimeout=5 -L 9333:127.0.0.1:9333 me@$host_addr 2>/dev/null
    
    sleep 1
    
    # Verify
    if nc -zv localhost 9333 2>&1 | grep -q "Connected"
        echo "✅ CDP tunnel online (localhost:9333 → $host_addr:9333)"
        return 0
    else
        echo "❌ CDP tunnel failed - is VS Code running on host with --remote-debugging-port=9333?"
        return 1
    end
end

# Check CDP tunnel status
function ac-cdp-status
    set -l tunnel_pid (pgrep -f "ssh.*9333" 2>/dev/null)
    if test -n "$tunnel_pid"
        echo "🟢 CDP tunnel process running (PID: $tunnel_pid)"
        if nc -zv localhost 9333 2>&1 | grep -q "Connected"
            echo "✅ Port 9333 is accessible"
        else
            echo "⚠️  Port 9333 not accessible (tunnel may be broken)"
        end
    else
        echo "🔴 CDP tunnel not running"
    end
end

# Assume the daemon is running when entering emacs.

# For fast config reloading - simple approach
function platform
    emacsclient -nw -c --eval '(aesthetic-backend (quote "artery"))'
end

function ensure-emacs-daemon-ready
    set -l timeout 240
    set -l silent 0
    set -l force_restart 0
    argparse 't/timeout=' 's/silent' 'f/force' -- $argv
    or return 1

    if set -q _flag_timeout
        set timeout $_flag_timeout
    end
    if set -q _flag_silent
        set silent 1
    end
    if set -q _flag_force
        set force_restart 1
    end

    # Initialize logging
    ac-emacs-init-logs
    set -l timestamp (date +%Y%m%d_%H%M%S)
    set -l log_file $AC_EMACS_LOG_DIR/daemon_$timestamp.log
    set -l config_path /home/me/aesthetic-computer/dotfiles/dot_config/emacs.el
    set -l wait_step 2
    set -l next_report 10

    if test $force_restart -eq 1
        if test $silent -ne 1
            echo "♻️  Forcing emacs daemon restart..."
        end
        pkill -f "emacs.*daemon" 2>/dev/null
        sleep 2
    else if pgrep -f "emacs.*daemon" >/dev/null
        if timeout 3 emacsclient -e t >/dev/null 2>&1
            if test $silent -ne 1
                echo "✅ Emacs daemon already running"
            end
            return 0
        else
            if test $silent -ne 1
                echo "⚠️  Emacs daemon process unresponsive, restarting..."
            end
            pkill -f "emacs.*daemon" 2>/dev/null
            sleep 2
        end
    end

    if not test -f $config_path
        echo "❌ Emacs configuration not found at $config_path"
        return 1
    end

    if test $silent -ne 1
        echo "🚀 Bootstrapping emacs daemon (timeout: $timeout s)"
        echo "📜 Logs: $log_file"
    end

    # Log startup info
    echo "=== Emacs Daemon Start ===" > $log_file
    echo "Timestamp: "(date -Iseconds) >> $log_file
    echo "Config: $config_path" >> $log_file
    echo "===========================" >> $log_file
    
    # Start daemon with output to log file
    command emacs -q --daemon -l $config_path >> $log_file 2>&1 &
    set -l daemon_pid $last_pid
    if test -z "$daemon_pid"
        set daemon_pid (jobs -l | tail -n1 | awk '{print $2}')
    end
    
    echo "PID: $daemon_pid" >> $log_file
    
    # Create symlink to latest log
    ln -sf $log_file $AC_EMACS_LOG_DIR/latest.log

    set -l elapsed 0
    while test $elapsed -lt $timeout
        if timeout 3 emacsclient -e t >/dev/null 2>&1
            if test $silent -ne 1
                echo "✅ Emacs daemon is ready!"
            end
            # Start crash monitor in background (don't let it block)
            fish -c "ac-emacs-crash-monitor" &>/dev/null &
            disown %last 2>/dev/null
            echo "[DEBUG] ensure-emacs-daemon-ready returning 0"
            return 0
        end

        if test -n "$daemon_pid"
            if not kill -0 $daemon_pid 2>/dev/null
                if test $silent -ne 1
                    echo "❌ Emacs daemon exited unexpectedly. Check $log_file for details."
                    if test -f $log_file
                        tail -n 20 $log_file
                    end
                end
                return 1
            end
        end

        sleep $wait_step
        set elapsed (math "$elapsed + $wait_step")

        if test $silent -ne 1
            if test $elapsed -ge $next_report
                echo "⏳ Waiting for emacs daemon... ($elapsed/$timeout s)"
                if test -f $log_file
                    tail -n 5 $log_file
                end
                set next_report (math "$next_report + 10")
            end
        end
    end

    if test $silent -ne 1
        echo "❌ Emacs daemon did not become ready within $timeout seconds."
        if test -f $log_file
            tail -n 20 $log_file
        end
    end

    return 1
end

# ⏲️ Wait on `entry.fish` to touch the `.waiter` file.

function aesthetic
    # Always kill emacs daemon to ensure clean state.
    # Prevents issues where daemon passes connectivity test but aesthetic-backend is corrupted.
    # This is UNCONDITIONAL - every ac-aesthetic implicitly runs ac-emacs-kill first.
    echo "🔄 Killing any existing emacs processes for clean start..."
    ac-emacs-kill
    
    # Check if --no-wait flag is passed
    if test "$argv[1]" = "--no-wait"
        echo "Skipping wait for .waiter file..."
    else
        # Check if entry.fish already completed (container is ready)
        # If dockerd is running and emacs daemon is responsive, we can skip waiting
        set -l skip_wait 0
        if pgrep -x dockerd >/dev/null 2>&1
            if timeout 3 emacsclient -e t >/dev/null 2>&1
                echo "✅ Container already configured, skipping wait..."
                set skip_wait 1
            end
        end
        
        if test $skip_wait -eq 0
            # Docker not up or emacs not ready - wait for waiter file
            clear
            set -l config_count 0
            set -l last_log_hash ""
            set -l entry_log /tmp/entry-fish.log
            
            while not test -f /home/me/.waiter
                # Only redraw if log content changed (reduces flicker)
                set -l current_log_hash ""
                if test -f $entry_log
                    set current_log_hash (tail -n 12 $entry_log 2>/dev/null | md5sum 2>/dev/null | cut -d' ' -f1)
                end
                
                if test "$current_log_hash" != "$last_log_hash" -o $config_count -eq 0 -o (math "$config_count % 10") -eq 0
                    clear
                    
                    # Show banner (static, no animation to reduce flicker)
                    toilet "Configuring..." -f future | lolcat -f
                    
                    # Show entry.fish progress if log exists
                    if test -f $entry_log
                        set -l log_lines (wc -l < $entry_log 2>/dev/null | string trim)
                        if test -n "$log_lines" -a "$log_lines" -gt 0
                            echo ""
                            echo "─────────────────────────────────────────"
                            tail -n 12 $entry_log 2>/dev/null
                            echo "─────────────────────────────────────────"
                        end
                    else
                        echo ""
                        echo "📄 Waiting for entry log at $entry_log"
                    end
                    
                    echo "📄 Log: $entry_log (latest: /home/me/.entry-logs/latest.log)"
                    echo "⏱️  Waited: $config_count s"
                    
                    set last_log_hash $current_log_hash
                end
                
                sleep 1
                set config_count (math $config_count + 1)
            end
            sudo rm /home/me/.waiter 2>/dev/null
        end
    end
    
    echo "[DEBUG] About to call ensure-emacs-daemon-ready..."
    if not ensure-emacs-daemon-ready --timeout=360
        echo "❌ Cannot connect to emacs daemon. Please check your emacs configuration."
        return 1
    end
    
    echo "[DEBUG] ensure-emacs-daemon-ready succeeded!"
    echo "[DEBUG] TTY: "(tty 2>&1)
    echo "[DEBUG] TERM: $TERM"
    
    # Connect to emacs and run aesthetic-backend to create all tabs
    echo "🚀 Connecting to aesthetic platform..."
    emacsclient -nw -c --eval '(aesthetic-backend (quote "artery"))'
    set -l exit_code $status
    if test $exit_code -ne 0
        echo ""
        echo "⚠️  emacsclient exited with code $exit_code"
        echo "💡 If the '💻 Aesthetic' VS Code task is frozen, restart it from the Task menu."
        echo "   Or run: ac-emacs-restart && then restart the task"
    end
    return $exit_code
end

# Convenience alias for skipping the wait
function aesthetic-now
    aesthetic --no-wait
end

# Direct aesthetic function that skips waiting entirely
function aesthetic-direct
    echo "🚀 Starting aesthetic directly (no wait)..."
    
    if not ensure-emacs-daemon-ready --timeout=300 --silent
        echo "❌ Cannot connect to emacs daemon."
        return 1
    end
    
    # Connect to emacs with aesthetic-backend
    emacsclient -nw -c --eval '(aesthetic-backend (quote "artery"))'
end

# TODO: Automatically kill online mode and go to offline mode if necessary.

# Helper function to check emacs daemon status
function check-daemon
    if pgrep -f "emacs.*daemon" >/dev/null
        echo "✅ Emacs daemon process found"
        if emacsclient -e t >/dev/null 2>&1
            echo "✅ Emacs daemon is responsive"
        else
            echo "❌ Emacs daemon found but not responsive"
        end
    else
        echo "❌ No emacs daemon process found"
    end
end

# Helper function to restart emacs daemon
function restart-daemon
    echo "🔄 Restarting emacs daemon..."
    if ensure-emacs-daemon-ready --force --timeout=360
        check-daemon
    else
        echo "❌ Failed to restart emacs daemon."
        check-daemon
    end
end

function ac-site
    cd ~/aesthetic-computer/system
    echo "🐱 Starting site..."
    echo "🔍 Cleaning up any stuck processes..."
    pkill -f "netlify dev" 2>/dev/null
    pkill -f "esbuild" 2>/dev/null
    sleep 1
    echo "🔌 Killing ports..."
    timeout 5 npx kill-port 8880 8888 8889 8080 8000 8111 3333 3000 3001 2>/dev/null; or true
    echo "🔗 Linking netlify..."
    timeout 10 netlify link --id $NETLIFY_SITE_ID 2>/dev/null; or true
    echo "🚀 Starting server..."
    npm run local-dev
end

function ac-media
    cd ~/aesthetic-computer/system
    echo "📦 Starting Caddy media server on :8111..."
    npm run media-server-caddy
end

# ac - Smart command: cd with no args, jump to piece with args  
function ac --description 'cd to aesthetic-computer or jump to piece'
    if test (count $argv) -eq 0
        cd ~/aesthetic-computer
    else
        set piece_path $argv[1]
        echo "🎯 Jumping to: $piece_path"
        set response (curl -s -k -X POST https://localhost:8889/jump -H "Content-Type: application/json" -d "{\"piece\": \"$piece_path\"}")
        
        # Show appropriate feedback based on connection status
        if string match -q "*vscodeConnected*true*" $response
            echo "✅ Sent to VSCode extension"
        else if string match -q "*Jump request sent*" $response
            echo "✅ Sent to browser clients"
        else
            echo "$response"
        end
    end
end

function ac-offline
    echo "🐭 Starting offline mode..."
    ac
    cd system/public
    cp system/offline-index.html system/public/index.html
    npx http-server -p 8888 -c-1 -g -b -S -C ../../ssl-dev/localhost.pem -K ../../ssl-dev/localhost-key.pem
end

function ac-url
    clear
    ac
    npm run -s url $argv
end

# 🖼️ Extension views dev server (3D process tree viz at localhost:5555)
function ac-views
    clear
    cd ~/aesthetic-computer/vscode-extension
    echo "🖼️ Starting Extension Views Dev Server on http://localhost:5555"
    echo "   Open dev.html in browser or toggle local mode in VS Code extension"
    npx --yes serve views -l 5555
end

alias ac-watch 'ac; npm run watch'

# 📱 Dev log monitoring function with dynamic file detection
function ac-dev-log
    set log_dir "/tmp/dev-logs"
    
    if not test -d $log_dir
        echo "🔍 No dev logs directory found at $log_dir"
        echo "💡 Try touching/drawing on your iPhone to generate logs"
        return 1
    end
    
    echo "📱 Monitoring ALL device logs in real-time..."
    echo "🎨 Touch/draw on any device to see live logs..."
    echo "🔄 Watching for new files in: $log_dir"
    echo "----------------------------------------"
    
    # Kill any existing tail/inotify processes for clean start
    pkill -f "tail.*$log_dir" 2>/dev/null
    pkill -f "inotifywait.*$log_dir" 2>/dev/null
    
    # Function to start monitoring a single log file
    function monitor_log_file
        set logfile $argv[1]
        set log_name (basename "$logfile" .log)
        
        tail -f "$logfile" 2>/dev/null | while read line
            # Extract JSON part (everything after first '{')
            set json_start (string match -r '\{.*' "$line")
            if test -n "$json_start"
                # Try to format with jq for colors
                set formatted (echo "$json_start" | jq -C . 2>/dev/null)
                if test $status -eq 0
                    # Extract timestamp/prefix part
                    set prefix (string replace -r '\{.*' '' "$line")
                    echo "📱 $log_name: $prefix$formatted"
                else
                    echo "📱 $log_name: $line"  
                end
            else
                echo "📱 $log_name: $line"
            end
        end &
    end
    
    # Monitor existing log files
    for logfile in $log_dir/*.log
        if test -f "$logfile"
            echo "🔍 Found existing log: "(basename "$logfile")
            monitor_log_file "$logfile"
        end
    end
    
    # Monitor for new log files being created (fallback if inotify not available)
    if command -v inotifywait >/dev/null 2>&1
        echo "�️  Using inotifywait for new file detection"
        inotifywait -m -e create -e moved_to --format '%w%f' "$log_dir" 2>/dev/null | while read new_file
            if string match -q "*.log" "$new_file"
                echo "🆕 New log file detected: "(basename "$new_file")
                monitor_log_file "$new_file"
            end
        end &
    else
        echo "⏰ Using polling for new file detection (inotify not available)"
        # Fallback: poll for new files every 2 seconds
        while true
            for logfile in $log_dir/*.log
                if test -f "$logfile"
                    set log_name (basename "$logfile" .log)
                    # Check if we're already monitoring this file
                    if not pgrep -f "tail.*$logfile" >/dev/null
                        echo "🆕 New log file detected: $log_name"
                        monitor_log_file "$logfile"
                    end
                end
            end
            sleep 2
        end &
    end
    
    # Keep the function running with proper interrupt handling
    echo "✅ Monitoring started. Press Ctrl+C to stop."
    
    # Use read to wait for interrupt (Ctrl+C) - this is more reliable in Fish
    # The read command will be interrupted by Ctrl+C and return control
    echo "Press any key to stop monitoring, or Ctrl+C..."
    read -s
    
    # Cleanup on exit
    echo "🛑 Stopping all monitoring processes..."
    pkill -f "tail.*$log_dir" 2>/dev/null
    pkill -f "inotifywait.*$log_dir" 2>/dev/null
    echo "✅ Monitoring stopped"
end

# 📱 List all available device logs
function ac-dev-logs
    set log_dir "/tmp/dev-logs"
    
    if not test -d $log_dir
        echo "🔍 No dev logs directory found"
        return 1
    end
    
    echo "📱 Available device logs:"
    ls -la $log_dir/*.log 2>/dev/null | while read line
        echo "  $line"
    end
end

# 📱 Clean old device logs
function ac-dev-log-clean
    set log_dir "/tmp/dev-logs"
    
    if test -d $log_dir
        echo "🧹 Cleaning old device logs..."
        rm -f $log_dir/*.log
        echo "✨ Done!"
    else
        echo "🔍 No dev logs directory found"
    end
end

# 📱 Improved dev log monitoring with proper Ctrl+C support (test version)
function ac-dev-log-new
    set log_dir "/tmp/dev-logs"
    
    if not test -d $log_dir
        echo "🔍 No dev logs directory found at $log_dir"
        echo "💡 Try touching/drawing on your iPhone to generate logs"
        return 1
    end
    
    echo "📱 Monitoring ALL device logs in real-time..."
    echo "🎨 Touch/draw on any device to see live logs..."
    echo "🔄 Watching for new files in: $log_dir"
    echo "✅ Press Ctrl+C to stop monitoring"
    echo "----------------------------------------"
    
    # Kill any existing monitoring processes for clean start
    pkill -f "tail.*$log_dir" 2>/dev/null
    pkill -f "inotifywait.*$log_dir" 2>/dev/null
    
    # Trap Ctrl+C to clean up properly
    function cleanup_logs --on-signal INT
        echo ""
        echo "🛑 Stopping log monitoring..."
        pkill -f "tail.*$log_dir" 2>/dev/null
        pkill -f "inotifywait.*$log_dir" 2>/dev/null
        echo "✨ Monitoring stopped!"
        return 0
    end
    
    # Function to start monitoring a single log file
    function start_monitor
        set logfile $argv[1]
        set log_name (basename "$logfile" .log)
        
        tail -f "$logfile" 2>/dev/null | while read line
            # Extract JSON part (everything after first '{')
            set json_start (string match -r '\{.*' "$line")
            if test -n "$json_start"
                # Try to format with jq for colors
                set formatted (echo "$json_start" | jq -C . 2>/dev/null)
                if test $status -eq 0
                    # Extract timestamp/prefix part
                    set prefix (string replace -r '\{.*' '' "$line")
                    echo "📱 $log_name: $prefix$formatted"
                else
                    echo "📱 $log_name: $line"  
                end
            else
                echo "📱 $log_name: $line"
            end
        end &
    end
    
    # Start monitoring existing files
    set -l monitored_files
    for logfile in $log_dir/*.log
        if test -f "$logfile"
            echo "🔍 Found existing log: "(basename "$logfile")
            start_monitor "$logfile"
            set -a monitored_files "$logfile"
        end
    end
    
    # Simple loop that responds quickly to Ctrl+C
    if command -v inotifywait >/dev/null 2>&1
        echo "👁️  Using inotifywait for new file detection"
        
        # Watch for new files
        inotifywait -m -e create -e moved_to --format '%w%f' "$log_dir" 2>/dev/null &
        set inotify_pid $last_pid
        
        while true
            # Read from inotify if available
            if jobs -q %$inotify_pid
                # Process new files...
                # (simplified for testing)
            end
            
            # Check for new files manually as backup
            for logfile in $log_dir/*.log
                if test -f "$logfile"; and not contains "$logfile" $monitored_files
                    set log_name (basename "$logfile" .log)
                    echo "🆕 New log file detected: $log_name"
                    start_monitor "$logfile"
                    set -a monitored_files "$logfile"
                end
            end
            
            sleep 1
        end
    else
        echo "⏰ Using polling for file detection"
        
        while true
            # Check for new files
            for logfile in $log_dir/*.log
                if test -f "$logfile"; and not contains "$logfile" $monitored_files
                    set log_name (basename "$logfile" .log)
                    echo "🆕 New log file detected: $log_name"
                    start_monitor "$logfile"
                    set -a monitored_files "$logfile"
                end
            end
            
            sleep 2
        end
    end
end
# 📱 Simple improved dev log monitoring (test version)
function ac-dev-log-simple
    set log_dir "/tmp/dev-logs"
    
    if not test -d $log_dir
        echo "🔍 No dev logs directory found at $log_dir"
        return 1
    end
    
    echo "📱 Monitoring device logs..."
    echo "✅ Press Ctrl+C to stop"
    echo "----------------------------------------"
    
    # Kill existing processes
    pkill -f "tail.*$log_dir" 2>/dev/null
    
    # Simple approach - just monitor all existing files
    for logfile in $log_dir/*.log
        if test -f "$logfile"
            set log_name (basename "$logfile" .log)
            echo "🔍 Monitoring: $log_name"
            tail -f "$logfile" | while read line
                echo "📱 $log_name: $line"
            end &
        end
    end
    
    # Simple loop that can be interrupted
    echo "🔄 Running... (Ctrl+C to stop)"
    while true
        sleep 1
    end
end
# alias ac-kidlisp 'ac; npm run test:kidlisp'
# Session server (simplified - no Emacs special handling)
function ac-session
    echo "🎮 Starting session server..."
    ac
    cd session-server
    
    echo "🔍 Cleaning up any stuck processes..."
    pkill -f "nodemon.*session.mjs" 2>/dev/null
    sleep 1
    npx kill-port 8889 2>/dev/null
    
    echo "🚀 Starting session server on port 8889..."
    PORT=8889 NODE_ENV=development npx nodemon -I --watch session.mjs session.mjs
end

# Oven service (tape video processing)
function ac-oven
    echo "🔥 Starting oven service..."
    ac
    cd oven
    
    echo "🔍 Cleaning up any stuck processes..."
    pkill -f "node.*server.mjs" 2>/dev/null
    sleep 1
    npx kill-port 3002 2>/dev/null
    
    echo "🚀 Starting oven server on https://localhost:3002..."
    npm run dev
end

# Silo service (data & storage dashboard)
function ac-silo
    echo "🏗️ Starting silo service..."
    ac
    cd silo

    echo "🔍 Cleaning up any stuck processes..."
    pkill -f "node.*silo/server.mjs" 2>/dev/null
    sleep 1
    npx kill-port 3003 2>/dev/null

    # SSH tunnel to DO droplet MongoDB (localhost:27018 → droplet:27017)
    if not pgrep -f "ssh.*27018:localhost:27017.*64.23.151.169" >/dev/null 2>&1
        echo "🔗 Setting up SSH tunnel to DO droplet MongoDB..."
        ssh -fN -L 27018:localhost:27017 -i ~/.ssh/silo-deploy-key root@64.23.151.169 2>/dev/null
        and echo "   tunnel established (localhost:27018)"
        or echo "   tunnel failed (check SSH key)"
    else
        echo "🔗 SSH tunnel already active (localhost:27018)"
    end

    echo "🚀 Starting silo on https://localhost:3003..."
    npm run dev
end

alias ac-stripe-print 'ac; npm run stripe-print-micro'
alias ac-stripe-ticket 'ac; npm run stripe-ticket-micro'
alias ac-extension 'ac; cd vscode-extension; npm run build; ac'

# kidlisp test function - supports watch mode or direct run
function ac-kidlisp
    ac
    if test "$argv[1]" = "watch"
        echo "🔍 Running kidlisp tests in watch mode..."
        npm run test:kidlisp
    else
        echo "🚀 Running kidlisp tests directly..."
        npm run test:kidlisp:direct
    end
end

# Pipe command output to session server build stream (for live build progress)
# Usage: some-command | ac-pipe
function ac-pipe
    while read -l line
        # Escape double quotes in the line for JSON
        set escaped_line (string replace -a '"' '\"' -- $line)
        /usr/bin/curl -s -X POST https://session-server.aesthetic.computer/build-stream --header "Content-Type: application/json" --data "{\"line\": \"$escaped_line\"}" > /dev/null 2>&1
        echo $line  # Also echo to terminal
    end
end

# ATProto PDS Admin - SSH into PDS server and run pdsadmin
function ac-at
    # ASCII art header with colors
    set_color cyan
    echo " █████╗ ████████╗██████╗ ██████╗  ██████╗ ████████╗ ██████╗"
    echo "██╔══██╗╚══██╔══╝██╔══██╗██╔══██╗██╔═══██╗╚══██╔══╝██╔═══██╗"
    echo "███████║   ██║   ██████╔╝██████╔╝██║   ██║   ██║   ██║   ██║"
    echo "██╔══██║   ██║   ██╔═══╝ ██╔══██╗██║   ██║   ██║   ██║   ██║"
    echo "██║  ██║   ██║   ██║     ██║  ██║╚██████╔╝   ██║   ╚██████╔╝"
    echo "╚═╝  ╚═╝   ╚═╝   ╚═╝     ╚═╝  ╚═╝ ╚═════╝    ╚═╝    ╚═════╝"
    set_color blue
    echo "        Personal Data Server - aesthetic.computer"
    set_color cyan
    echo "🕷️  at.aesthetic.computer        💾 165.227.120.137"
    echo "📦 at-blobs-aesthetic-computer   🦋 ATProto Federation"
    set_color normal
    echo ""
    
    if test (count $argv) -eq 0
        # No arguments: drop into interactive shell
        ssh -o StrictHostKeyChecking=no -i ~/.ssh/aesthetic_pds root@165.227.120.137
    else
        # With arguments: run pdsadmin command directly
        ssh -o StrictHostKeyChecking=no -i ~/.ssh/aesthetic_pds root@165.227.120.137 "pdsadmin $argv"
    end
end

# Backward compatibility alias
alias ac-pds='ac-at'

# Claude Code CLI agent
function ac-agent
    claude $argv
end

# Ollama LLM daemon management
function ac-llama
    set -l command $argv[1]
    
    switch $command
        case start
            echo "🦙 Starting Ollama daemon..."
            
            # Check if already running
            if pgrep -f "ollama serve" >/dev/null
                echo "⚠️  Ollama daemon already running"
                if curl -s http://localhost:11434/api/version >/dev/null 2>&1
                    echo "✅ Daemon is responsive"
                    return 0
                else
                    echo "⚠️  Daemon process exists but not responsive, restarting..."
                    pkill -f "ollama serve" 2>/dev/null
                    sleep 2
                end
            end
            
            # Start the daemon
            nohup ollama serve > /tmp/ollama.log 2>&1 &
            set daemon_pid $last_pid
            echo "🚀 Ollama daemon started (PID: $daemon_pid)"
            
            # Wait for it to become responsive
            set -l timeout 30
            set -l elapsed 0
            while test $elapsed -lt $timeout
                if curl -s http://localhost:11434/api/version >/dev/null 2>&1
                    echo "✅ Ollama daemon is ready!"
                    return 0
                end
                sleep 1
                set elapsed (math $elapsed + 1)
            end
            
            echo "❌ Ollama daemon didn't become ready within $timeout seconds"
            echo "📋 Check logs at /tmp/ollama.log"
            return 1
            
        case stop
            echo "🛑 Stopping Ollama daemon..."
            pkill -f "ollama serve" 2>/dev/null
            if test $status -eq 0
                echo "✅ Ollama daemon stopped"
            else
                echo "⚠️  No Ollama daemon was running"
            end
            
        case restart
            echo "🔄 Restarting Ollama daemon..."
            ac-llama stop
            sleep 2
            ac-llama start
            
        case status
            if pgrep -f "ollama serve" >/dev/null
                echo "✅ Ollama daemon process is running"
                if curl -s http://localhost:11434/api/version >/dev/null 2>&1
                    set -l ollama_version (curl -s http://localhost:11434/api/version 2>/dev/null | jq -r '.version' 2>/dev/null)
                    echo "✅ Daemon is responsive (version: $ollama_version)"
                    
                    # List loaded models
                    echo "📦 Checking available models..."
                    set -l models (curl -s http://localhost:11434/api/tags 2>/dev/null | jq -r '.models[]?.name' 2>/dev/null)
                    if test -n "$models"
                        echo "Models available:"
                        for model in $models
                            echo "  • $model"
                        end
                    else
                        echo "  (no models loaded)"
                    end
                else
                    echo "❌ Daemon process exists but not responsive"
                    echo "📋 Check logs at /tmp/ollama.log"
                end
            else
                echo "❌ Ollama daemon is not running"
            end
            
        case logs
            if test -f /tmp/ollama.log
                echo "📋 Ollama logs (last 50 lines):"
                tail -n 50 /tmp/ollama.log
            else
                echo "⚠️  No log file found at /tmp/ollama.log"
            end
            
        case '*'
            echo "Usage: ac-llama [start|stop|restart|status|logs]"
            echo ""
            echo "Commands:"
            echo "  start    - Start the Ollama daemon"
            echo "  stop     - Stop the Ollama daemon"
            echo "  restart  - Restart the Ollama daemon"
            echo "  status   - Check daemon status and list models"
            echo "  logs     - Show recent daemon logs"
            return 1
    end
end

# alias ac-shell 'ac; ac-url; ac-tunnel; fish'
# alias ac-offline 'ac; cd system/public; npx http-server -p 8888 -c-1 -g -b -S -C ../../ssl-dev/localhost.pem -K ../../ssl-dev/localhost-key.pem'
alias ac-redis 'clear; ac; npm run redis'
alias ac-udp 'ssh root@157.245.134.225' # ac monolith udp server management

# Send aesthetic.computer playlist to TV cast coordination
function ac-ff-playlist
    cd ~/aesthetic-computer
    ./rebroadcast.sh
end

# Connect to the aesthetic platform (emacs TUI)
alias ac-aesthetic 'aesthetic-now'

alias ac-servers 'clear; ac; npm run -s servers; env nogreet=true fish'
alias ac-chat-system 'clear; ac; npm run -s chat; cd nanos; npm run chat-system:dev; fish'
alias ac-chat-sotce 'clear; ac; npm run -s chat; cd nanos; npm run chat-sotce:dev; fish'
alias ac-chat-clock 'clear; ac; npm run -s chat; cd nanos; npm run chat-clock:dev; fish'
alias ac-tunnel 'ac; npm run tunnel; fish'
alias ac-logger 'ac; cd system; npx netlify logs:function index'
alias sotce-net 'ac; cd system; npx netlify logs:function sotce-net'
alias acw 'cd ~/aesthetic-computer/system; npm run watch'

# GitHub Copilot CLI (LLM agent in terminal)
# Uses GH_TOKEN from vault for auth, runs interactively
# Default to Claude Opus 4.5 for best quality
# Use --continue to resume last session after reboot
alias ac-llm 'clear; ac; copilot --model claude-opus-4.5'
alias ac-llm-continue 'clear; ac; copilot --model claude-opus-4.5 --continue'
alias ac-llm-resume 'clear; ac; copilot --model claude-opus-4.5 --resume'

# Process viewer (htop for monitoring system resources)
alias ac-top 'clear; htop'

alias cat 'bat -p' # use bat for syntax highlighting instead of the `cat` default

# 🧟 Zombie process monitor and cleanup
function ac-zombie-monitor
    echo "🧟 Starting zombie process monitor..."
    echo "📊 Checking every 30 seconds for zombie accumulation"
    echo "🔪 Will clean up if zombies > 100"
    echo "Press Ctrl+C to stop monitoring"
    
    while true
        set zombie_count (ps aux | grep -c defunct)
        set timestamp (date '+%H:%M:%S')
        
        if test $zombie_count -gt 100
            echo "[$timestamp] ⚠️  Found $zombie_count zombies - cleaning up..."
            # Find parent processes with zombie children and log them
            ps aux | grep defunct | awk '{print $2}' | while read zpid
                set parent_pid (ps -o ppid= -p $zpid 2>/dev/null | string trim)
                if test -n "$parent_pid"
                    set parent_cmd (ps -o cmd= -p $parent_pid 2>/dev/null | string trim)
                    echo "  🧟 Zombie $zpid has parent $parent_pid: $parent_cmd"
                end
            end
            echo "  🧹 Attempting cleanup by reaping zombies..."
            # Signal parent processes to reap their zombie children
            ps aux | grep defunct | awk '{print $2}' | while read zpid
                set parent_pid (ps -o ppid= -p $zpid 2>/dev/null | string trim)
                if test -n "$parent_pid"; and test "$parent_pid" != "1"
                    # Send SIGCHLD to parent to trigger reaping
                    kill -CHLD $parent_pid 2>/dev/null
                end
            end
            sleep 2
            set new_zombie_count (ps aux | grep -c defunct)
            echo "  📊 Zombies after cleanup: $new_zombie_count (was $zombie_count)"
        else
            echo "[$timestamp] ✅ System healthy: $zombie_count zombies"
        end
        
        sleep 30
    end
end

# Quick zombie status check
function ac-zombie-status
    set zombie_count (ps aux | grep -c defunct)
    echo "🧟 Current zombie count: $zombie_count"
    
    if test $zombie_count -gt 10
        echo "📊 Top zombie-producing parents:"
        ps aux | grep defunct | awk '{print $2}' | while read zpid
            set parent_pid (ps -o ppid= -p $zpid 2>/dev/null | string trim)
            if test -n "$parent_pid"
                ps -o pid,cmd= -p $parent_pid 2>/dev/null
            end
        end | sort | uniq -c | sort -rn | head -5
    end
end

# set up an ngrok tunnel

function ac-tunnel
    # Run in foreground for Emacs eat terminals (shows logs)
    # Skip auto-restart loop when in Emacs - just run once
    # Use AC_EMACS_MODE instead of INSIDE_EMACS to avoid conflicts with tools
    if test -n "$AC_EMACS_MODE"
        echo "🚇 Starting tunnel (foreground mode for Emacs)..."
        ac
        npm run tunnel
        return
    end
    
    set tmp (mktemp)
    ngrok start --config ngrok.yml --all 2>$tmp
    set ngrok_exit $status
    set err (cat $tmp)
    rm -f $tmp

    if test $ngrok_exit -ne 0
        if string match -q '*ERR_NGROK_334*' $err
            clear
            echo "🟢 tunnel already online — watching..."
            # Add timeout to prevent infinite loops (5 minutes max)
            set timeout_count 0
            set max_timeout 60  # 60 * 5 seconds = 5 minutes
            while test $timeout_count -lt $max_timeout
                sleep 5
                set timeout_count (math $timeout_count + 1)
                if not curl --silent --max-time 2 --output /dev/null https://local.aesthetic.computer
                    echo "🔁 tunnel down, restarting..."
                    # Kill any existing ngrok processes to ensure clean restart
                    pkill -f ngrok 2>/dev/null
                    sleep 2
                    ac-tunnel
                    return
                end
            end
            echo "⏱️  Tunnel monitoring timeout reached (5 minutes)"
            return
        else
            echo "❌ ngrok error:"
            echo $err
        end
    else
        echo "🚀 tunnel started successfully!"
    end
end

function ac-function
    # Watch Netlify function logs
    # Usage: ac-function [function-name]
    # If no function name is provided, lists all functions
    if test (count $argv) -eq 0
        echo "📋 Watching all Netlify function logs..."
        npx netlify logs:function
    else
        echo "📋 Watching logs for function: $argv[1]"
        npx netlify logs:function $argv[1]
    end
end

# a shell-gpt shortcut (must be all lowercase / otherwise quoted)

function umm
    # Use string escape to handle special characters
    set -l args (string join " " $argv)
    # Pass the joined, escaped string to sgpt
    sgpt --chat umm "$args"
end

function umms
    sgpt --show-chat umm
end

function code
    if set -q argv[1]
        set -l args (string join " " $argv)
        sgpt --code --chat code "$args"
    else
        sgpt --code --editor --chat code
    end
end

function codes
    sgpt --show-chat code
end

function copy
    # Extract everything from the chat after the last "assistant: " line.
    set content (sgpt --show-chat code | tac | sed '/^assistant: /q' | tac | sed '1s/^assistant: //')
    printf "%s\n" $content | xclip -selection clipboard
end

function done
    rm /tmp/chat_cache/code 2>/dev/null
    echo "bye :)"
end

function ok
    rm /tmp/chat_cache/umm 2>/dev/null
    echo "bye :)"
end

function forget
    rm /tmp/chat_cache/umm 2>/dev/null
    echo "umm, i forgot :)"
end

# Increase Node.js heap size

set -x NODE_OPTIONS "--max-old-space-size=4096"

# Set Google Application Credentials

set -x GOOGLE_APPLICATION_CREDENTIALS /home/me/aesthetic-computer/nanos/gcp-service.key.json

set -x PATH /google-cloud-sdk/bin $PATH

alias nvm forget

# use tab to autocomplete the first suggestion

bind \t complete-select-first

function ac-deploy-session
    set script_path ~/aesthetic-computer/session-server/deploy.fish
    if test -f $script_path
        fish $script_path $argv
    else
        echo "❌ Deploy script not found at $script_path"
        return 1
    end
end

function clipboard
    set content $argv
    set -l hosts host.docker.internal $HOST_IP 172.17.0.1
    for host in $hosts
        echo "🧪 trying $host..."
        if echo "" | nc -z -w 0.15 $host 12345 2>/dev/null
            echo "✅ sending to $host"
            printf "%s\n" $content | nc $host 12345
            return
        end
    end
    echo "❌ clipboard: No reachable host for clipboard relay"
end

# Automatically reload the fish config when it changes. 25.04.28.00.12

# if status --is-interactive

#     function __fish_watch_config --description "Watch and reload config.fish on save"

#         while inotifywait --quiet -e close_write ~/.config/fish/config.fish

#             echo "🐟 Reloading config.fish..."

#             source ~/.config/fish/config.fish

#         end

#     end

#     set pidfile ~/.cache/fish-config-watcher.pid

#     if not test -e $pidfile; or not kill (cat $pidfile) ^/dev/null

#         __fish_watch_config & disown

#         echo $last_pid > $pidfile

#     end

# end

# ac-shop - Shopify CLI for Aesthetic Computer
function ac-shop
    node /workspaces/aesthetic-computer/ac-shop/shopify.mjs $argv
end

# ac-tezbot - Tezos daemon helpers
set -gx TEZBOT_SCRIPT /workspaces/aesthetic-computer/tezos/ac-tezbot.mjs

function tezbot --description "Tezos daemon interface"
    if test (count $argv) -eq 0
        node $TEZBOT_SCRIPT status
    else
        node $TEZBOT_SCRIPT $argv
    end
end

function tezbot-start --description "Start tezbot daemon"
    node $TEZBOT_SCRIPT status >/dev/null 2>&1
    if test $status -eq 0
        echo "🟢 Tezbot already running"
    else
        echo "🚀 Starting tezbot..."
        nohup node $TEZBOT_SCRIPT > /tmp/tezbot.log 2>&1 &
        sleep 0.5
        node $TEZBOT_SCRIPT status
    end
end

function tezbot-stop --description "Stop tezbot daemon"
    node $TEZBOT_SCRIPT stop
end

function tz-balance --description "Get Tezos wallet balance via tezbot"
    node $TEZBOT_SCRIPT cmd balance
end

function tz-status --description "Get Tezos contract status via tezbot"
    node $TEZBOT_SCRIPT cmd status
end

function tz-tokens --description "List minted tokens via tezbot"
    node $TEZBOT_SCRIPT cmd tokens
end

# 🎮 KidLisp Playdate Build and Simulate Functions

# Build a KidLisp program for Playdate
# Usage: ac-playdate-build examples/bop.lisp
function ac-playdate-build
    if test (count $argv) -lt 1
        echo "Usage: ac-playdate-build <source.lisp>"
        echo "Example: ac-playdate-build examples/bop.lisp"
        return 1
    end
    
    set -l source_file $argv[1]
    cd /workspaces/aesthetic-computer/kidlisp-playdate
    source build.fish $source_file
end

# Run the last built Playdate game on host simulator
# Usage: ac-playdate-simulate [game-name]
# If no game name provided, uses 'bop' as default
function ac-playdate-simulate
    set -l game_name (test (count $argv) -ge 1; and echo $argv[1]; or echo "bop")
    set -l pdx_path "/workspaces/aesthetic-computer/kidlisp-playdate/build/$game_name.pdx"
    
    if not test -d $pdx_path
        echo "❌ Game not found: $pdx_path"
        echo "💡 Build it first with: ac-playdate-build examples/$game_name.lisp"
        return 1
    end
    
    echo "🎮 Deploying $game_name to host simulator..."
    
    # Kill existing simulator
    ssh jas@172.17.0.1 "pkill -9 -f Playdate" 2>/dev/null
    sleep 0.3
    
    # Copy and run
    scp -r $pdx_path jas@172.17.0.1:~/
    ssh jas@172.17.0.1 "DISPLAY=:0 nohup ~/PlaydateSDK/bin/PlaydateSimulator ~/$game_name.pdx >/dev/null 2>&1 &"
    
    echo "✅ $game_name running on host simulator"
end

# Build and immediately simulate
# Usage: ac-playdate examples/bop.lisp
function ac-playdate
    if test (count $argv) -lt 1
        echo "Usage: ac-playdate <source.lisp>"
        echo "Example: ac-playdate examples/bop.lisp"
        return 1
    end
    
    set -l source_file $argv[1]
    set -l game_name (basename $source_file .lisp)
    
    ac-playdate-build $source_file
    and ac-playdate-simulate $game_name
end

# 🖥️ Electron App Management (SSH to Mac host)
# Restarts the Aesthetic Computer Electron app on the host Mac

function ac-electron-restart --description "Restart the Aesthetic Computer Electron app on host Mac"
    set -l host "jas@host.docker.internal"
    set -l ac_path "/Users/jas/Desktop/code/aesthetic-computer/ac-electron"
    
    echo "🔄 Restarting Aesthetic Computer Electron app..."
    
    # Kill existing Electron processes (our app only, not VS Code)
    ssh -o StrictHostKeyChecking=no $host "pkill -f '$ac_path/node_modules/electron'" 2>/dev/null
    sleep 1
    
    # Start the app again
    ssh -o StrictHostKeyChecking=no $host "cd $ac_path && nohup npm start > /tmp/ac-electron.log 2>&1 &"
    
    echo "✅ Electron app restarting... check /tmp/ac-electron.log on host for output"
end

function ac-electron-stop --description "Stop the Aesthetic Computer Electron app on host Mac"
    set -l host "jas@host.docker.internal"
    set -l ac_path "/Users/jas/Desktop/code/aesthetic-computer/ac-electron"
    
    echo "🛑 Stopping Aesthetic Computer Electron app..."
    ssh -o StrictHostKeyChecking=no $host "pkill -f '$ac_path/node_modules/electron'" 2>/dev/null
    echo "✅ Electron app stopped"
end

function ac-electron-start --description "Start the Aesthetic Computer Electron app on host Mac"
    set -l host "jas@host.docker.internal"
    set -l ac_path "/Users/jas/Desktop/code/aesthetic-computer/ac-electron"
    
    echo "🚀 Starting Aesthetic Computer Electron app..."
    ssh -o StrictHostKeyChecking=no $host "cd $ac_path && nohup npm start > /tmp/ac-electron.log 2>&1 &"
    echo "✅ Electron app started"
end

function ac-electron-dev --description "Toggle dev mode for Electron app (loads files from repo)"
    set -l host "jas@host.docker.internal"
    set -l dev_flag ".ac-electron-dev"
    
    # Check current state
    set -l is_dev (ssh -o StrictHostKeyChecking=no $host "test -f ~/$dev_flag && echo yes || echo no" 2>/dev/null)
    
    if test "$argv[1]" = "status"
        if test "$is_dev" = "yes"
            echo "🔧 Dev mode is ENABLED"
            echo "   Files load from: ~/aesthetic-computer/ac-electron/"
        else
            echo "📦 Dev mode is DISABLED"
            echo "   Files load from: app bundle"
        end
        return 0
    end
    
    if test "$argv[1]" = "on"
        ssh -o StrictHostKeyChecking=no $host "touch ~/$dev_flag"
        echo "🔧 Dev mode ENABLED - restart Electron app to apply"
        echo "   Run: ac-electron-restart"
        return 0
    end
    
    if test "$argv[1]" = "off"
        ssh -o StrictHostKeyChecking=no $host "rm -f ~/$dev_flag"
        echo "📦 Dev mode DISABLED - restart Electron app to apply"
        echo "   Run: ac-electron-restart"
        return 0
    end
    
    # Toggle
    if test "$is_dev" = "yes"
        ssh -o StrictHostKeyChecking=no $host "rm -f ~/$dev_flag"
        echo "📦 Dev mode DISABLED - restart Electron app to apply"
    else
        ssh -o StrictHostKeyChecking=no $host "touch ~/$dev_flag"
        echo "🔧 Dev mode ENABLED - restart Electron app to apply"
    end
    echo "   Run: ac-electron-restart"
end

function ac-electron-reload --description "Reload all Electron windows (dev mode: picks up file changes)"
    set -l host "jas@host.docker.internal"
    
    echo "♻️ Reloading Electron windows..."
    # Use osascript to send Cmd+R to Aesthetic Computer windows
    ssh -o StrictHostKeyChecking=no $host 'osascript -e "tell application \"Aesthetic Computer\" to activate" -e "delay 0.2" -e "tell application \"System Events\" to keystroke \"r\" using command down"' 2>/dev/null
    echo "✅ Reload triggered"
end

# 🎸 Ableton M4L Console Tunnel
# Listen for console.log/error/warn from M4L devices via UDP

function ac-ableton-tunnel --description "Listen for Ableton M4L device console logs"
    set -l port 7777
    set -l host "jas@host.docker.internal"
    
    echo "🎸 AC Ableton Console Tunnel"
    echo "   Listening for M4L device logs on UDP port $port..."
    echo "   (Ctrl+C to stop)"
    echo ""
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    
    # Run nc on the Mac via SSH to listen for UDP messages
    # Max sends to 127.0.0.1:7777 on the Mac
    ssh -o StrictHostKeyChecking=no -t $host "nc -lu $port" 2>/dev/null
end

function ac-ableton-tunnel-simple --description "Simple UDP listener (run on Mac directly)"
    echo "🎸 AC Ableton Console (Simple Mode)"
    echo "   Run this on your Mac (not in devcontainer):"
    echo ""
    echo "   nc -lu 7777"
    echo ""
    echo "   Or with formatting:"
    echo "   nc -lu 7777 | while read line; do echo \"\$(date '+%H:%M:%S') \$line\"; done"
end

# 🖥️ Machine Info / SSH Helpers
# Read machine configs from vault/machines.json

function ac-host --description "Show current host SSH config from machines.json"
    set -l machines_file "/workspaces/aesthetic-computer/aesthetic-computer-vault/machines.json"
    
    if not test -f $machines_file
        echo "❌ machines.json not found at $machines_file"
        return 1
    end
    
    set -l machine_key $argv[1]
    
    if test -z "$machine_key"
        # Show all machines
        echo "📍 Available machines (from vault/machines.json):"
        echo ""
        cat $machines_file | jq -r '.machines | to_entries[] | "\(.value.emoji // "🖥️") \(.key): \(.value.label) [\(.value.os // "unknown")]"'
        echo ""
        echo "Usage: ac-host <machine-key> [ssh|ip|info]"
        echo "Examples:"
        echo "  ac-host jeffrey-macbook ssh   # SSH to the machine"
        echo "  ac-host ff1-dvveklza ip       # Show just the IP"
        echo "  ac-host mac-mini info         # Show full info"
        return 0
    end
    
    set -l action $argv[2]
    set -l machine_data (cat $machines_file | jq -r ".machines[\"$machine_key\"]")
    
    if test "$machine_data" = "null"
        echo "❌ Machine '$machine_key' not found"
        echo "Available: "(cat $machines_file | jq -r '.machines | keys | join(", ")')
        return 1
    end
    
    set -l ip (echo $machine_data | jq -r '.ip // empty')
    set -l user (echo $machine_data | jq -r '.user // "jas"')
    set -l host (echo $machine_data | jq -r '.host // empty')
    set -l label (echo $machine_data | jq -r '.label // .key')
    set -l port (echo $machine_data | jq -r '.port // empty')
    
    # Use host if specified (e.g., host.docker.internal), else ip
    set -l target $host
    if test -z "$target"
        set target $ip
    end
    
    switch $action
        case ssh
            if test -z "$target"
                echo "❌ No IP/host configured for $machine_key"
                return 1
            end
            echo "🔗 Connecting to $label ($user@$target)..."
            ssh -o StrictHostKeyChecking=no $user@$target
        case ip
            if test -n "$ip"
                echo $ip
            else if test -n "$host"
                echo $host
            else
                echo "❌ No IP configured"
                return 1
            end
        case info '*'
            echo "📋 $label"
            echo $machine_data | jq .
    end
end

function ac-machines --description "List all machines from vault/machines.json"
    ac-host
end

function ac-host-nmap --description "Run nmap scan on local network via current host"
    set -l machines_file "/workspaces/aesthetic-computer/aesthetic-computer-vault/machines.json"
    set -l search_term $argv[1]
    
    # Try to find a reachable host to run nmap on
    # Check hosts in order of likelihood: jas-fedora, x1-nano-g2, jeffrey-macbook
    set -l hosts_to_try "jas-fedora" "x1-nano-g2" "jeffrey-macbook" "jeffrey-windows"
    
    for host_key in $hosts_to_try
        set -l host_data (cat $machines_file | jq -r ".machines[\"$host_key\"]")
        if test "$host_data" = "null"
            continue
        end
        
        set -l ip (echo $host_data | jq -r '.ip // empty')
        set -l user (echo $host_data | jq -r '.user // "me"')
        set -l label (echo $host_data | jq -r '.label')
        
        if test -z "$ip"
            continue
        end
        
        # Quick connectivity check (1 second timeout)
        if ssh -o ConnectTimeout=1 -o StrictHostKeyChecking=no -o BatchMode=yes $user@$ip "echo ok" 2>/dev/null | grep -q ok
            echo "🔍 Running nmap via $label ($ip)..."
            
            if test -n "$search_term"
                # Search for specific term
                ssh -o StrictHostKeyChecking=no $user@$ip "nmap -sn 192.168.1.0/24 2>/dev/null | grep -B2 -i '$search_term'"
            else
                # Full scan
                ssh -o StrictHostKeyChecking=no $user@$ip "nmap -sn 192.168.1.0/24 2>/dev/null"
            end
            return $status
        end
    end
    
    echo "❌ No reachable host found to run nmap"
    echo "Tried: $hosts_to_try"
    echo ""
    echo "Make sure one of your machines is online and has the correct IP in machines.json"
    echo "You can update IPs by running on the host: hostname -I | awk '{print \$1}'"
    return 1
end

# 🖼️ FF1 Art Computer Helpers

function __ac_ff1_find_host --description "Find a reachable host to run network commands"
    set -l machines_file "/workspaces/aesthetic-computer/aesthetic-computer-vault/machines.json"
    set -l hosts_to_try "jas-fedora" "x1-nano-g2" "jeffrey-macbook" "jeffrey-windows"
    
    for host_key in $hosts_to_try
        set -l host_data (cat $machines_file | jq -r ".machines[\"$host_key\"]" 2>/dev/null)
        if test "$host_data" = "null" -o -z "$host_data"
            continue
        end
        
        set -l ip (echo $host_data | jq -r '.ip // empty')
        set -l user (echo $host_data | jq -r '.user // "me"')
        
        if test -z "$ip"
            continue
        end
        
        # Quick connectivity check (1 second timeout)
        if ssh -o ConnectTimeout=1 -o StrictHostKeyChecking=no -o BatchMode=yes $user@$ip "echo ok" 2>/dev/null | grep -q ok
            echo "$user@$ip"
            return 0
        end
    end
    return 1
end

function __ac_ff1_scan_network --description "Scan network for FF1 device"
    set -l host_target (__ac_ff1_find_host)
    if test -z "$host_target"
        echo ""
        return 1
    end
    
    # Run nmap on the host and look for FF1
    set -l result (ssh -o StrictHostKeyChecking=no $host_target "nmap -sn 192.168.1.0/24 2>/dev/null | grep -A1 'FF1'" 2>/dev/null)
    if test -n "$result"
        # Extract IP from result like "Nmap scan report for FF1-DVVEKLZA (192.168.1.164)"
        echo $result | grep -oP '\d+\.\d+\.\d+\.\d+' | head -1
    else
        echo ""
    end
end

function __ac_ff1_update_ip --description "Update FF1 IP in machines.json"
    set -l new_ip $argv[1]
    set -l machines_file "/workspaces/aesthetic-computer/aesthetic-computer-vault/machines.json"
    
    # Use jq to update the IP
    set -l tmp_file (mktemp)
    cat $machines_file | jq ".machines[\"ff1-dvveklza\"].ip = \"$new_ip\"" > $tmp_file
    mv $tmp_file $machines_file
end

function ac-ff1 --description "Control FF1 Art Computer (direct network access)"
    set -l machines_file "/workspaces/aesthetic-computer/aesthetic-computer-vault/machines.json"
    set -l ff1_data (cat $machines_file 2>/dev/null | jq -r '.machines["ff1-dvveklza"]')
    set -l ff1_ip (echo $ff1_data | jq -r '.ip')
    set -l ff1_port (echo $ff1_data | jq -r '.port // 1111')
    
    set -l action $argv[1]
    
    # For commands that need FF1 to be online, check connectivity first
    set -l needs_connection false
    switch $action
        case ping cast top colors chords playlist
            set needs_connection true
    end
    
    if test "$needs_connection" = true
        # Quick ping check
        if not curl -s --connect-timeout 2 "http://$ff1_ip:$ff1_port/" >/dev/null 2>&1
            echo "⚠️  FF1 not responding at $ff1_ip:$ff1_port"
            echo "🔍 Scanning network for FF1..."
            
            # Find a host to scan from
            set -l host_target (__ac_ff1_find_host)
            if test -z "$host_target"
                echo "❌ No reachable host to scan from. Update machine IPs in machines.json"
                echo "   Run on your host: hostname -I | awk '{print \$1}'"
                return 1
            end
            
            echo "   Using $host_target for scan..."
            echo -n "   Scanning "
            
            # Run nmap with progress indication
            set -l scan_result (ssh -o StrictHostKeyChecking=no $host_target "nmap -sn 192.168.1.0/24 2>/dev/null" 2>/dev/null)
            echo "done!"
            
            # Look for FF1 in results
            set -l new_ip (echo $scan_result | grep -oP 'FF1[^\(]*\(\K[0-9.]+')
            
            if test -n "$new_ip"
                echo "✅ Found FF1 at $new_ip"
                
                if test "$new_ip" != "$ff1_ip"
                    echo "📝 Updating machines.json ($ff1_ip → $new_ip)"
                    __ac_ff1_update_ip $new_ip
                    set ff1_ip $new_ip
                end
                
                # Verify new IP works
                if curl -s --connect-timeout 2 "http://$ff1_ip:$ff1_port/" >/dev/null 2>&1
                    echo "✅ FF1 responding at new IP!"
                    echo ""
                else
                    echo "❌ FF1 found but not responding on port $ff1_port"
                    return 1
                end
            else
                echo "❌ FF1 not found on network"
                echo ""
                echo "Is the FF1 powered on and connected to WiFi?"
                echo "Full scan results:"
                echo $scan_result | grep -E "^Nmap|Host is up" | head -20
                return 1
            end
        end
    end
    
    switch $action
        case scan
            echo "🔍 Scanning for FF1 on network..."
            set -l host_target (__ac_ff1_find_host)
            if test -z "$host_target"
                echo "❌ No reachable host to scan from"
                return 1
            end
            echo "   Using $host_target..."
            ssh -o StrictHostKeyChecking=no $host_target "nmap -sn 192.168.1.0/24 2>/dev/null | grep -B2 -i 'ff1'"
        case ping
            echo "🏓 Pinging FF1 at $ff1_ip:$ff1_port..."
            curl -s --connect-timeout 3 "http://$ff1_ip:$ff1_port/" >/dev/null 2>&1 && echo "✅ FF1 responding!" || echo "❌ FF1 not responding"
        case cast
            if test (count $argv) -lt 2
                echo "Usage: ac-ff1 cast <piece|url> [options]"
                echo ""
                echo "Pieces starting with \$ are KidLisp and auto-use device.kidlisp.com"
                echo "Regular pieces use aesthetic.computer with ?device param"
                echo ""
                echo "Examples:"
                echo "  ac-ff1 cast \$mtz             # KidLisp → device.kidlisp.com/mtz"
                echo "  ac-ff1 cast \$mtz --perf      # With FPS/performance HUD"
                echo "  ac-ff1 cast \$mtz --socklogs  # Enable remote console logging"
                echo "  ac-ff1 cast ceo              # Regular → aesthetic.computer/ceo?device"
                echo "  ac-ff1 cast https://example.com/art.html"
                echo ""
                echo "Options:"
                echo "  --perf       Enable KidLisp performance/FPS HUD overlay"
                echo "  --socklogs   Enable remote console logs (view with: ac-ff1 logs)"
                echo "  --relay      Use cloud relay (requires API key)"
                return 1
            end
            set -l input $argv[2]
            set -l use_relay false
            set -l use_perf false
            set -l use_socklogs false
            
            # Parse flags
            for arg in $argv[3..-1]
                switch $arg
                    case '--relay'
                        set use_relay true
                    case '--perf'
                        set use_perf true
                    case '--socklogs'
                        set use_socklogs true
                end
            end
            
            # Build URL from input - auto-detect KidLisp ($) vs regular pieces
            set -l url
            # Auto-prepend https:// for domain-like inputs (e.g., top.kidlisp.com)
            if string match -rq '\.[a-z]{2,}(/|$)' $input; and not string match -q 'http*' $input
                set input "https://$input"
            end
            if string match -q 'http*' $input
                # Already a full URL
                # Don't add &device for device/top.kidlisp.com (already device-optimized)
                if string match -q '*device.kidlisp.com*' $input; or string match -q '*top.kidlisp.com*' $input
                    set url $input
                else if string match -q '*?*' $input
                    set url "$input&device"
                else
                    set url "$input?device"
                end
                # Add perf param if requested
                if test "$use_perf" = true
                    if string match -q '*?*' $url
                        set url "$url&perf"
                    else
                        set url "$url?perf"
                    end
                end
                # Add socklogs param if requested
                if test "$use_socklogs" = true
                    if string match -q '*?*' $url
                        set url "$url&socklogs"
                    else
                        set url "$url?socklogs"
                    end
                end
            else if string match -rq '^\$' $input
                # KidLisp piece (starts with $) - use device.kidlisp.com
                # Keep the $ prefix - device.kidlisp.com passes it to aesthetic.computer
                set -l code_id $input
                set -l params ""
                if test "$use_perf" = true
                    set params "perf=true"
                end
                if test "$use_socklogs" = true
                    if test -n "$params"
                        set params "$params&socklogs"
                    else
                        set params "socklogs"
                    end
                end
                if test -n "$params"
                    set url "https://device.kidlisp.com/$code_id?$params"
                else
                    set url "https://device.kidlisp.com/$code_id"
                end
                if test "$use_perf" = true
                    echo "🎨 KidLisp piece detected (perf mode)"
                else
                    echo "🎨 KidLisp piece detected"
                end
                if test "$use_socklogs" = true
                    echo "🔌 Remote logging enabled - run 'ac-ff1 logs' to view"
                end
            else
                # Regular aesthetic.computer piece - add &device if has query, else ?device
                # Also add density=1 by default for FF1 4K display to prevent slow rendering
                if string match -q '*?*' $input
                    set url "https://aesthetic.computer/$input&device&density=1"
                else
                    set url "https://aesthetic.computer/$input?device&density=1"
                end
                # Add perf param if requested
                if test "$use_perf" = true
                    set url "$url&perf"
                end
            end
            
            if test "$use_relay" = true
                # Use Feral File cloud relay API (requires topicId and API key)
                set -l topic_id (echo $ff1_data | jq -r '.topicId')
                set -l api_key (echo $ff1_data | jq -r '.apiKey // ""')
                if test -z "$topic_id" -o "$topic_id" = "null"
                    echo "❌ No topicId configured for FF1 in machines.json"
                    return 1
                end
                if test -z "$api_key" -o "$api_key" = "null"
                    echo "⚠️  Cloud relay requires API-KEY. Get it from FF1 app or use direct mode."
                    echo "   Add 'apiKey' to machines.json or use: ac-ff1 cast <url> (without --relay)"
                    return 1
                end
                echo "☁️ Casting $url to FF1 via cloud relay..."
                set -l payload (printf '{"command":"displayPlaylist","request":{"playlist":{"dpVersion":"1.0.0","items":[{"source":"%s","duration":0}]},"intent":{"action":"now_display"}}}' "$url")
                curl -s -X POST -H 'Content-Type: application/json' -H "topicID: $topic_id" -H "API-KEY: $api_key" "https://artwork-info.feral-file.workers.dev/api/cast" -d "$payload"
            else
                # Direct network access to FF1
                # Check if it's a playlist URL (feed server or contains /playlists/ or /api/playlist)
                if string match -q '*feed.aesthetic.computer*' $url; or string match -q '*playlists*' $url; or string match -q '*/api/playlist*' $url
                    # It's a playlist URL - send as playlistUrl so FF1 fetches and displays it
                    echo "📺 Casting playlist to FF1: $url"
                    curl -s --connect-timeout 5 -X POST -H 'Content-Type: application/json' "http://$ff1_ip:$ff1_port/api/cast" -d "{\"command\":\"displayPlaylist\",\"request\":{\"playlistUrl\":\"$url\",\"intent\":{\"action\":\"now_display\"}}}"
                else
                    # Single URL - wrap in a minimal playlist
                    echo "📺 Casting $url to FF1..."
                    curl -s --connect-timeout 5 -X POST -H 'Content-Type: application/json' "http://$ff1_ip:$ff1_port/api/cast" -d "{\"command\":\"displayPlaylist\",\"request\":{\"dp1_call\":{\"dpVersion\":\"1.1.0\",\"items\":[{\"source\":\"$url\",\"duration\":0}]},\"intent\":{\"action\":\"now_display\"}}}"
                end
            end
        case top
            # Top 100 KidLisp hits - uses TV endpoint directly (no Cloudflare KV dependency)
            set -l limit 100
            set -l duration 24
            
            # Parse options
            for arg in $argv[2..-1]
                if string match -q -- '--limit=*' "$arg"
                    set limit (string replace -- '--limit=' '' "$arg")
                else if string match -q -- '--duration=*' "$arg"
                    set duration (string replace -- '--duration=' '' "$arg")
                end
            end
            
            set -l playlist_url "https://aesthetic.computer/api/tv?types=kidlisp&sort=hits&limit=$limit&format=dp1&duration=$duration"
            echo "🎵 Fetching Top $limit KidLisp Hits from TV endpoint..."
            echo "   URL: $playlist_url"
            echo "📺 Casting playlist to FF1..."
            curl -s --connect-timeout 5 -X POST -H 'Content-Type: application/json' "http://$ff1_ip:$ff1_port/api/cast" -d "{\"command\":\"displayPlaylist\",\"request\":{\"playlistUrl\":\"$playlist_url\",\"intent\":{\"action\":\"now_display\"}}}"
        case colors chords
            # Legacy feed server playlists (may hit KV limits)
            set -l playlist_name $action
            echo "🎵 Looking up $playlist_name playlist from feed server..."
            echo "⚠️  Note: Feed server may hit Cloudflare KV limits. Consider using 'ac-ff1 top' instead."
            
            # Map playlist names to their IDs
            set -l playlist_id
            switch $playlist_name
                case colors
                    set playlist_id "4b872517-e4d8-4433-af8b-a9a4a8204cc9"
                case chords
                    set playlist_id "49f0ee0e-0303-4192-9cb2-aa3c5abb64b5"
            end
            
            set -l playlist_url "https://feed.aesthetic.computer/api/v1/playlists/$playlist_id"
            echo "📺 Casting playlist to FF1: $playlist_url"
            curl -s --connect-timeout 5 -X POST -H 'Content-Type: application/json' "http://$ff1_ip:$ff1_port/api/cast" -d "{\"command\":\"displayPlaylist\",\"request\":{\"playlistUrl\":\"$playlist_url\",\"intent\":{\"action\":\"now_display\"}}}"
        case tunnel
            echo "🚇 Starting SSH tunnel to FF1 (localhost:1111 -> $ff1_ip:$ff1_port)..."
            echo "Press Ctrl+C to stop the tunnel"
            ssh -L 1111:$ff1_ip:$ff1_port jas@host.docker.internal -N
        case playlist
            # Generate and push a DP-1 playlist of top KidLisp hits
            set -l limit 10
            set -l duration 60
            set -l handle ""
            
            # Parse options using string match with -- separator
            for arg in $argv[2..-1]
                if string match -q -- '--limit=*' "$arg"
                    set limit (string replace -- '--limit=' '' "$arg")
                else if string match -q -- '--duration=*' "$arg"
                    set duration (string replace -- '--duration=' '' "$arg")
                else if string match -q -- '--handle=*' "$arg"
                    set handle (string replace -- '--handle=' '' "$arg")
                else if string match -q -- 'handle=*' "$arg"
                    # Support handle=jeffrey without --
                    set handle (string replace -- 'handle=' '' "$arg")
                end
            end
            
            # Build playlist API URL
            set -l playlist_url "https://aesthetic.computer/api/playlist?limit=$limit&duration=$duration&density=8"
            if test -n "$handle"
                set playlist_url "$playlist_url&handle=$handle"
                echo "🎵 Fetching top $limit KidLisp hits by @$handle..."
            else
                echo "🎵 Fetching top $limit KidLisp hits..."
            end
            
            # Fetch playlist from API
            set -l playlist_json (curl -s "$playlist_url")
            
            if test -z "$playlist_json"
                echo "❌ Failed to fetch playlist"
                return 1
            end
            
            # Extract codes for display
            set -l codes (echo $playlist_json | jq -r '.items[].title | ltrimstr("$")')
            set codes (string split \n -- $codes)
            
            echo "📺 Pushing playlist to FF1 ("(count $codes)" items, "$duration"s each)..."
            echo "   Codes:" $codes
            
            # Extract items array from playlist response
            set -l items_json (echo $playlist_json | jq -c '.items | map({source, duration})')
            
            # Build full DP-1 payload
            set -l playlist_payload (printf '{"command":"displayPlaylist","request":{"dp1_call":{"dpVersion":"1.0.0","items":%s},"intent":{"action":"now_display"}}}' "$items_json")
            
            # Send to FF1 directly
            curl -s --connect-timeout 5 -X POST -H 'Content-Type: application/json' "http://$ff1_ip:$ff1_port/api/cast" -d "$playlist_payload"
        case logs
            # Stream remote console logs from devices with ?socklogs enabled
            echo "👁️ Connecting to session-server for remote logs..."
            echo "   Cast with: ac-ff1 cast \$code --socklogs"
            echo "   Press Ctrl+C to stop"
            echo ""
            
            # Determine session server URL - use local if available, else production
            # Production uses direct IP since Cloudflare SSL isn't configured for WebSocket origin
            set -l session_url "ws://157.245.134.225:8889/socklogs?role=viewer"
            set -l websocat_opts ""
            # Check if local session server is running
            if nc -z localhost 8889 2>/dev/null
                set session_url "wss://localhost:8889/socklogs?role=viewer"
                set websocat_opts "-k" # Skip cert verification for local dev
                echo "📡 Using local session server (localhost:8889)"
            else
                echo "📡 Using production session server (direct IP)"
            end
            
            # Use websocat if available, otherwise fall back to node script
            if command -v websocat >/dev/null 2>&1
                websocat $websocat_opts "$session_url" 2>/dev/null | while read -l line
                    set -l json $line
                    set -l type (echo $json | jq -r '.type // ""' 2>/dev/null)
                    if test "$type" = "log"
                        set -l level (echo $json | jq -r '.level // "info"' 2>/dev/null)
                        set -l msg (echo $json | jq -r '.message // ""' 2>/dev/null)
                        set -l device (echo $json | jq -r '.deviceId // "unknown"' 2>/dev/null)
                        set -l ts (echo $json | jq -r '.timestamp // 0' 2>/dev/null)
                        set -l time_str (date -d "@"(math $ts / 1000) "+%H:%M:%S" 2>/dev/null; or echo "??:??:??")
                        
                        # Color based on level
                        switch $level
                            case error
                                set_color red
                            case warn
                                set_color yellow
                            case debug
                                set_color brblack
                            case '*'
                                set_color normal
                        end
                        printf "[%s] %s [%s] %s\n" $time_str $device (string upper $level) $msg
                        set_color normal
                    else if test "$type" = "status"
                        set -l device_count (echo $json | jq -r '.devices | length' 2>/dev/null)
                        set_color cyan
                        echo "📱 $device_count device(s) connected"
                        set_color normal
                    else if test "$type" = "device-connected"
                        set -l device (echo $json | jq -r '.deviceId // "unknown"' 2>/dev/null)
                        set_color green
                        echo "📱 Device connected: $device"
                        set_color normal
                    else if test "$type" = "device-disconnected"
                        set -l device (echo $json | jq -r '.deviceId // "unknown"' 2>/dev/null)
                        set_color red
                        echo "📱 Device disconnected: $device"
                        set_color normal
                    end
                end
            else
                echo "⚠️ websocat not installed. Install with: cargo install websocat"
                echo "   Or run: npm install -g wscat && wscat -c '$session_url'"
            end
        case info '*'
            set -l topic_id (echo $ff1_data | jq -r '.topicId // "not set"')
            echo "🖼️ FF1 Art Computer"
            echo "   IP: $ff1_ip"
            echo "   Port: $ff1_port"
            echo "   Device ID: "(echo $ff1_data | jq -r '.deviceId')
            echo "   Topic ID: $topic_id"
            echo ""
            echo "Commands:"
            echo "  ac-ff1                        - Show this help"
            echo "  ac-ff1 scan                   - Find FF1 via mDNS"
            echo "  ac-ff1 ping                   - Check if FF1 is responding"  
            echo "  ac-ff1 cast \$mtz              - Cast KidLisp piece (auto device.kidlisp.com)"
            echo "  ac-ff1 cast \$mtz --perf       - Cast with FPS/performance HUD"
            echo "  ac-ff1 cast ceo               - Cast regular piece (auto ?device param)"
            echo "  ac-ff1 cast <url>             - Cast any URL"
            echo "  ac-ff1 top                    - Cast Top KidLisp Hits playlist"
            echo "  ac-ff1 colors                 - Cast KidLisp Colors playlist"
            echo "  ac-ff1 chords                 - Cast KidLisp Chords playlist"
            echo "  ac-ff1 playlist [options]     - Push dynamic KidLisp playlist"
            echo "  ac-ff1 tunnel                 - Create SSH tunnel for local dev"
            echo "  ac-ff1 logs                   - Stream remote console logs (requires ?socklogs)"
            echo ""
            echo "Playlist options:"
            echo "  --limit=N     Number of items (default: 10)"
            echo "  --duration=S  Seconds per item (default: 60)"
            echo "  --handle=H    Filter by user handle"
    end
end

# 📊 Bundle Telemetry - query performance data from inline bundles
function ac-telemetry --description "Query bundle telemetry from MongoDB"
    set -l action $argv[1]
    set -l limit 20
    set -l piece ""
    
    # Parse options
    for arg in $argv[2..-1]
        if string match -q -- '--limit=*' "$arg"
            set limit (string replace -- '--limit=' '' "$arg")
        else if string match -q -- '--piece=*' "$arg"
            set piece (string replace -- '--piece=' '' "$arg")
        end
    end
    
    set -l base_url "https://aesthetic.computer/api/bundle-telemetry-query"
    set -l piece_param ""
    if test -n "$piece"
        set piece_param "&piece=$piece"
    end
    
    switch $action
        case recent boot
            echo "📊 Recent bundle boots (last $limit)..."
            curl -s "$base_url?type=boot&limit=$limit$piece_param" | jq -r '
                .results[] | 
                "\(.timestamp | split(".")[0] | gsub("T"; " ")) | \(.piece // "?") | boot:\(.bootTime // "?")ms | density:\(.density // "?") | \(.screen // "?")"
            ' 2>/dev/null || echo "❌ Query failed"
        case fps perf
            echo "📈 FPS data from recent sessions..."
            curl -s "$base_url?type=perf&limit=$limit$piece_param" | jq -r '
                .results[] | 
                "\(.timestamp | split(".")[0] | gsub("T"; " ")) | \(.piece // "?") | samples:\(.samples | length // 0) | density:\(.density // "?")"
            ' 2>/dev/null || echo "❌ Query failed"
        case errors error
            echo "❌ Recent bundle errors..."
            curl -s "$base_url?type=error&limit=$limit$piece_param" | jq -r '
                .results[] | 
                "\(.timestamp | split(".")[0] | gsub("T"; " ")) | \(.piece // "?") | \(.message // "unknown")"
            ' 2>/dev/null || echo "❌ Query failed"
        case summary
            echo "📊 Bundle telemetry summary (last 24h)..."
            curl -s "$base_url?type=summary" | jq '.' 2>/dev/null || echo "❌ Query failed"
        case raw
            echo "🔍 Raw telemetry documents..."
            curl -s "$base_url?type=boot&limit=$limit$piece_param" | jq '.' 2>/dev/null || echo "❌ Query failed"
        case '*'
            echo "📊 Bundle Telemetry Query Tool"
            echo ""
            echo "Usage: ac-telemetry <command> [options]"
            echo ""
            echo "Commands:"
            echo "  ac-telemetry recent   - Show recent bundle boots"
            echo "  ac-telemetry fps      - Show FPS data from sessions"
            echo "  ac-telemetry errors   - Show recent errors"
            echo "  ac-telemetry summary  - Show 24h summary stats"
            echo "  ac-telemetry raw      - Show raw JSON"
            echo ""
            echo "Options:"
            echo "  --limit=N      Number of results (default: 20)"
            echo "  --piece=NAME   Filter by piece name"
    end
end

# 🎮 Xbox helper - connect to Xbox on local network
function ac-xbox --description "Interact with Xbox on 1244 Innes Ave LAN"
    set -l vault_path "$HOME/aesthetic-computer/aesthetic-computer-vault/machines.json"
    if not test -f $vault_path
        echo "❌ machines.json not found"
        return 1
    end
    
    set -l xbox_ip (jq -r '.machines.xbox.ip' $vault_path)
    set -l portal_port (jq -r '.machines.xbox.devicePortalPort' $vault_path)
    
    if test -z "$xbox_ip" -o "$xbox_ip" = "null"
        echo "❌ Xbox not configured in machines.json"
        return 1
    end
    
    switch $argv[1]
        case ping
            echo "🏓 Pinging Xbox at $xbox_ip..."
            ssh jas@host.docker.internal "ping -c 3 $xbox_ip"
        case portal
            echo "🌐 Xbox Device Portal: https://$xbox_ip:$portal_port"
            echo "   Opening in browser..."
            ssh jas@host.docker.internal "open 'https://$xbox_ip:$portal_port'"
        case info
            echo "🎮 Xbox Series X"
            echo "   IP: $xbox_ip"
            echo "   Device Portal: https://$xbox_ip:$portal_port"
            echo "   Network: 1244 Innes Ave Home LAN"
            echo ""
            echo "Deploy from Mac:"
            echo "   1. Push to trigger GitHub Actions build"
            echo "   2. Download .msix from Actions artifacts"
            echo "   3. ac-xbox deploy <path-to.msix>"
            echo ""
            echo "Or open Device Portal: ac-xbox portal"
        case tunnel
            echo "🚇 Creating SSH tunnel to Xbox Device Portal..."
            echo "   Local: https://localhost:11443 → Xbox: $xbox_ip:$portal_port"
            echo "   Press Ctrl+C to stop"
            ssh -L 11443:$xbox_ip:$portal_port jas@host.docker.internal -N
        case deploy
            if test (count $argv) -lt 2
                echo "Usage: ac-xbox deploy <package.msix>"
                echo ""
                echo "Get the package from GitHub Actions:"
                echo "  1. Push changes to xbox/ folder"
                echo "  2. Go to Actions → Build Xbox App → Download artifact"
                echo "  3. ac-xbox deploy ~/Downloads/AestheticComputer.msix"
                return 1
            end
            set -l pkg $argv[2]
            if not test -f $pkg
                echo "❌ Package not found: $pkg"
                return 1
            end
            echo "📦 Deploying $pkg to Xbox..."
            # Upload via Mac host curl to Device Portal
            ssh jas@host.docker.internal "curl -k -X POST -F 'file=@$pkg' 'https://$xbox_ip:$portal_port/api/app/packagemanager/package'"
        case build
            echo "🔨 Triggering GitHub Actions build..."
            echo "   This will build the Xbox app in the cloud."
            gh workflow run xbox-build.yml
            echo ""
            echo "   Watch progress: gh run watch"
            echo "   Download when done: gh run download"
        case '*'
            echo "🎮 Xbox Helper (1244 Innes Ave LAN)"
            echo ""
            echo "Usage: ac-xbox <command>"
            echo ""
            echo "Commands:"
            echo "  ac-xbox ping      - Check if Xbox is reachable"
            echo "  ac-xbox portal    - Open Device Portal in browser"
            echo "  ac-xbox info      - Show connection info"
            echo "  ac-xbox tunnel    - Create SSH tunnel to Device Portal"
            echo "  ac-xbox build     - Trigger GitHub Actions build"
            echo "  ac-xbox deploy <file.msix> - Deploy package to Xbox"
            echo ""
            echo "Xbox IP: $xbox_ip"
    end
end

if set -q AC_TASK_LAUNCH
    set -e AC_TASK_LAUNCH  # Clear it so nested shells don't re-trigger
    aesthetic
end
