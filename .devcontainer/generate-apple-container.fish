#!/usr/bin/env fish
# generate-apple-container.fish
# Automatically generates Containerfile.apple from the main Dockerfile
# Transforms x86_64/aarch64 conditionals to ARM64-only for Apple Silicon

set -l script_dir (dirname (status filename))
set -l dockerfile "$script_dir/Dockerfile"
set -l output "$script_dir/Containerfile.apple"

if not test -f $dockerfile
    echo "❌ Dockerfile not found at $dockerfile"
    exit 1
end

echo "🔄 Generating Apple Container Containerfile from Dockerfile..."

# Start with header
echo '# Aesthetic Computer - Apple Container Runtime Edition
# Auto-generated from .devcontainer/Dockerfile for native macOS ARM64 virtualization
# Regenerate with: .devcontainer/generate-apple-container.fish
# Optimized for Apple Silicon (M1/M2/M3 Pro/Max/Ultra)
' > $output

# Process the Dockerfile
set -l in_skip_block false
set -l skip_patterns \
    "qemu-kvm" "qemu-img" "qemu-system" "qemu-user-static" \
    "ocaml" "opam" "bubblewrap" \
    "libX11" "libxkbfile" "libsecret" "libxshmfence" "libXtst" \
    "nss" "atk" "pango" "cups-libs" "libXcomposite" "libXcursor" \
    "libXi" "libXdamage" "libXrandr" "alsa-lib" "gtk3" \
    "xorg-x11-server-Xvfb" "psmisc" "lsof" "chromium" \
    "docker pull" "SmartPy" "smartpy" "Octez" "octez"

while read -l line
    # Skip empty lines at start
    if test -z "$line"
        echo "" >> $output
        continue
    end

    # Skip Docker-in-Docker pulls (not needed for Apple Container)
    if string match -q "*docker pull*" $line
        continue
    end
    
    # Skip SmartPy (x86_64 only)
    if string match -q "*smartpy*" $line; or string match -q "*SmartPy*" $line
        continue
    end
    
    # Skip Octez binary downloads (use Docker method anyway)
    if string match -q "*octez*" $line; or string match -q "*Octez*" $line
        continue
    end

    # Transform architecture conditionals to ARM64 only
    if string match -q '*case "$(uname -m)"*' $line
        # Skip the whole case block and use ARM64 directly
        set in_skip_block true
        continue
    end
    
    if test "$in_skip_block" = true
        if string match -q "*esac*" $line
            set in_skip_block false
        end
        continue
    end

    # Replace x86_64 arch detection with arm64 hardcoded
    set line (string replace -r 'ARCH=\$\(uname -m\).*amd64.*arm64.*' 'ARCH=arm64' $line)
    set line (string replace -r 'ARCH=amd64' 'ARCH=arm64' $line)
    set line (string replace -r 'PS_ARCH="x64".*PS_ARCH="arm64"' 'PS_ARCH="arm64"' $line)
    set line (string replace -r '\$\{ARCH\}' 'arm64' $line)
    
    # Skip lines that check for x86_64
    if string match -q '*if \[ "$(uname -m)" = "x86_64" \]*' $line
        continue
    end

    echo $line >> $output
end < $dockerfile

echo "✅ Generated $output"
echo ""
echo "To build and run with Apple Container:"
echo "  container build -t ac-dev -f $output $script_dir"
echo "  container run --name ac-dev -it --ssh \\"
echo "    -v ~/Desktop/code/aesthetic-computer:/workspaces/aesthetic-computer \\"
echo "    -p 8888:8888 -p 8889:8889 -p 3000:3000 \\"
echo "    -w /workspaces/aesthetic-computer ac-dev"
