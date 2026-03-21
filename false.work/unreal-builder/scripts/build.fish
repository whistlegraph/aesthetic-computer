#!/usr/bin/env fish
# Unified SpiderLily Build Controller
# Run from the devcontainer to build for any platform
#
# Usage:
#   ./build.fish              # Interactive menu
#   ./build.fish windows      # Build Windows
#   ./build.fish mac          # Build Mac
#   ./build.fish ios          # Build iOS (device)
#   ./build.fish ios-sim      # Build iOS Simulator
#   ./build.fish all          # Build all platforms

set script_dir (dirname (status filename))

function show_header
    echo ""
    echo "╔════════════════════════════════════════╗"
    echo "║   🕷️  SpiderLily Build Controller      ║"
    echo "╠════════════════════════════════════════╣"
    echo "║                                        ║"
    echo "║  All builds run from devcontainer via  ║"
    echo "║  SSH to the target build machine.      ║"
    echo "║                                        ║"
    echo "╚════════════════════════════════════════╝"
    echo ""
end

function show_menu
    echo "  Select a platform to build:"
    echo ""
    echo "  1) 🪟  Windows   (via host.docker.internal)"
    echo "  2) 🍎  Mac       (via Mac mini SSH)"
    echo "  3) 📱  iOS       (device, via Mac mini)"
    echo "  4) 📱  iOS Sim   (simulator, via Mac mini)"
    echo "  5) 🌐  All       (Windows + Mac + iOS)"
    echo ""
    echo "  q) Exit"
    echo ""
    echo -n "  Choice: "
end

function build_windows
    echo ""
    echo "🪟 ═══════════════════════════════════════"
    echo "   Starting Windows build..."
    echo "═══════════════════════════════════════════"
    echo ""
    fish /workspaces/aesthetic-computer/windows/remote-update-and-build.fish
    return $status
end

function build_mac
    echo ""
    echo "🍎 ═══════════════════════════════════════"
    echo "   Starting Mac build..."
    echo "═══════════════════════════════════════════"
    echo ""
    fish /workspaces/aesthetic-computer/false.work/unreal-builder/scripts/remote-mac-build.fish
    return $status
end

function build_ios
    set build_type $argv[1]
    if test -z "$build_type"
        set build_type "device"
    end
    
    echo ""
    echo "📱 ═══════════════════════════════════════"
    echo "   Starting iOS ($build_type) build..."
    echo "═══════════════════════════════════════════"
    echo ""
    fish /workspaces/aesthetic-computer/false.work/unreal-builder/scripts/remote-ios-build.fish $build_type
    return $status
end

function build_all
    echo ""
    echo "🌐 ═══════════════════════════════════════"
    echo "   Building ALL platforms..."
    echo "═══════════════════════════════════════════"
    echo ""
    
    set failed_builds
    
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo "  [1/3] Windows"
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    if not build_windows
        set -a failed_builds "Windows"
    end
    
    echo ""
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo "  [2/3] Mac"
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    if not build_mac
        set -a failed_builds "Mac"
    end
    
    echo ""
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo "  [3/3] iOS"
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    if not build_ios "device"
        set -a failed_builds "iOS"
    end
    
    echo ""
    echo "═══════════════════════════════════════════"
    if test (count $failed_builds) -eq 0
        echo "✅ All builds completed successfully!"
    else
        echo "⚠️  Some builds failed: $failed_builds"
    end
    echo "═══════════════════════════════════════════"
    echo ""
end

# Main execution
set platform $argv[1]

show_header

if test -z "$platform"
    # Interactive mode
    show_menu
    read choice
    
    switch $choice
        case 1 windows w
            set platform "windows"
        case 2 mac m
            set platform "mac"
        case 3 ios i
            set platform "ios"
        case 4 ios-sim sim s
            set platform "ios-sim"
        case 5 all a
            set platform "all"
        case q Q exit
            echo "  Goodbye!"
            exit 0
        case '*'
            echo "  ❌ Invalid choice: $choice"
            exit 1
    end
end

switch $platform
    case windows win w
        build_windows
    case mac m
        build_mac
    case ios i
        build_ios "device"
    case ios-sim sim
        build_ios "simulator"
    case all a
        build_all
    case '*'
        echo "❌ Unknown platform: $platform"
        echo ""
        echo "Valid options:"
        echo "  windows, mac, ios, ios-sim, all"
        exit 1
end
