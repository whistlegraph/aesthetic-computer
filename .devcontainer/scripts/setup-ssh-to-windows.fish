#!/usr/bin/env fish
# Setup / verify SSH connection from this devcontainer to the Windows host
# (jeffrey's Windows 11 tower, computer name 'Aesthetic').
#
# This script is IDEMPOTENT and SAFE to re-run. The heavy lifting lives in
# entry.fish (the bridge container is ensured automatically on every container
# start). This script is for manual diagnostics, recovering after a broken
# state, or first-time setup on a new host.
#
# Topology:
#   devcontainer (172.17.0.2)
#     --> 172.17.0.1:2222 (socat container 'ac-ssh-bridge' on Fedora's Docker
#                          in --network=host mode)
#     --> 172.19.64.1:22  (WSL2 NAT gateway, from Fedora WSL distro's view)
#     --> Windows OpenSSH (binds 0.0.0.0:22, key authorized at
#                          C:\ProgramData\ssh\administrators_authorized_keys)
#
# For the full writeup see:
#   ~/.claude/projects/-workspaces-aesthetic-computer/memory/ssh_bridge_to_windows.md

echo "🪟 Setup / verify SSH to Windows host"
echo ""

# -----------------------------------------------------------------------------
# Step 1: Verify vault id_rsa is in place
# -----------------------------------------------------------------------------
if not test -f ~/.ssh/id_rsa
    echo "❌ ~/.ssh/id_rsa not found"
    echo "   Run devault.fish or let entry.fish restore it from the vault:"
    echo "   cd /workspaces/aesthetic-computer/aesthetic-computer-vault && fish devault.fish"
    exit 1
end
echo "✅ ~/.ssh/id_rsa present"

# -----------------------------------------------------------------------------
# Step 2: Verify ~/.ssh/config has a 'Host aesthetic' (or 'windows-host') block
# -----------------------------------------------------------------------------
if not grep -qE '^Host .*(aesthetic|windows-host)' ~/.ssh/config 2>/dev/null
    echo "❌ No 'Host aesthetic' or 'Host windows-host' block in ~/.ssh/config"
    echo "   The vault's home/.ssh/config should contain it. Run devault.fish"
    echo "   to restore it, or add the block manually. Expected content:"
    echo ""
    echo "   Host aesthetic aesthetic-windows jeffrey-windows windows-host"
    echo "       HostName 172.17.0.1"
    echo "       Port 2222"
    echo "       User me"
    echo "       IdentityFile ~/.ssh/id_rsa"
    echo "       IdentitiesOnly yes"
    exit 1
end
echo "✅ ~/.ssh/config has Host block for aesthetic/windows-host"

# -----------------------------------------------------------------------------
# Step 3: Verify / (re)start the socat bridge container on the Fedora host
# -----------------------------------------------------------------------------
if not test -S /var/run/docker.sock
    echo "❌ No /var/run/docker.sock — cannot manage the bridge from inside the devcontainer"
    echo "   The devcontainer needs the Docker socket mounted to start the bridge."
    exit 1
end

set bridge_state (sudo -n docker inspect -f '{{.State.Running}}' ac-ssh-bridge 2>/dev/null)
if test "$bridge_state" = "true"
    echo "✅ ac-ssh-bridge container is running"
else
    echo "🔧 ac-ssh-bridge not running, starting it..."

    # Detect the Windows host IP by running a helper in --network=host mode.
    # Its default route IS the Fedora host's default route, which in WSL2 NAT
    # mode points at the Windows host.
    set win_ip (sudo -n docker run --rm --network=host alpine sh -c 'ip route | awk "/default/ {print \$3}"' 2>/dev/null)
    if test -z "$win_ip"
        echo "❌ Could not detect Windows host IP via helper container"
        exit 1
    end
    echo "   Detected Windows host at $win_ip (from Fedora namespace)"

    sudo -n docker rm -f ac-ssh-bridge >/dev/null 2>&1
    if sudo -n docker run -d --name ac-ssh-bridge --restart unless-stopped --network=host \
        alpine/socat "TCP-LISTEN:2222,fork,reuseaddr" "TCP:$win_ip:22" >/dev/null
        echo "✅ ac-ssh-bridge started (forwarding :2222 -> $win_ip:22)"
    else
        echo "❌ Failed to start ac-ssh-bridge container"
        exit 1
    end
end

# -----------------------------------------------------------------------------
# Step 4: Test the end-to-end connection
# -----------------------------------------------------------------------------
echo ""
echo "🧪 Testing ssh aesthetic ..."
if ssh -o BatchMode=yes -o ConnectTimeout=5 aesthetic 'powershell -Command "$env:COMPUTERNAME"' 2>/dev/null
    echo "✅ SSH to Windows host works"
    echo ""
    echo "You can now run e.g.:"
    echo "  ssh aesthetic 'powershell -Command \"Get-Process\"'"
    echo "  scp -P 2222 myfile.txt me@172.17.0.1:'C:/Users/me/Desktop/'"
else
    echo "❌ SSH to Windows host failed"
    echo ""
    echo "Debug checklist:"
    echo "  1. Is Windows OpenSSH running?  ssh aesthetic -v  (look at handshake)"
    echo "  2. Is the key installed on Windows?"
    echo "     C:\\ProgramData\\ssh\\administrators_authorized_keys should contain ~/.ssh/id_rsa.pub"
    echo "  3. Is Windows Firewall blocking? Set-NetFirewallRule -DisplayName 'OpenSSH SSH Server (sshd)' -Profile Any"
    echo "  4. Is the bridge forwarding the right IP?"
    echo "     sudo docker logs ac-ssh-bridge"
    exit 1
end
