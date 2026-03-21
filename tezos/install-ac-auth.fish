#!/usr/bin/env fish
#
# ac-login wrapper for fish shell
#

function ac-login
    node /workspaces/aesthetic-computer/tezos/ac-login.mjs $argv
end

function ac-logout
    node /workspaces/aesthetic-computer/tezos/ac-login.mjs logout
end

function ac-status
    node /workspaces/aesthetic-computer/tezos/ac-login.mjs status
end

function ac-token
    node /workspaces/aesthetic-computer/tezos/ac-login.mjs token
end

# Make functions available
funcsave ac-login >/dev/null 2>&1
funcsave ac-logout >/dev/null 2>&1
funcsave ac-status >/dev/null 2>&1
funcsave ac-token >/dev/null 2>&1

echo "✅ AC authentication functions installed:"
echo "   ac-login   - Login to Aesthetic Computer"
echo "   ac-logout  - Logout"
echo "   ac-status  - Check login status"
echo "   ac-token   - Get access token"
