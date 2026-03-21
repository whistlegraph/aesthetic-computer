#!/usr/bin/env fish
# ⚡🐍 Electric Snake Bite
# 
# Triggers a reload of the Electron development window from inside the devcontainer.
# Works by outputting a special escape sequence that the Electron terminal watches for.
#
# Usage: electric-snake-bite
#        esb  (alias)

function electric-snake-bite --description "⚡🐍 Reload Electron dev window"
    # Output the magic escape sequence
    # OSC 9999 is a custom/unused OSC code we're hijacking
    printf '\e]9999;electric-snake-bite\a'
    echo "⚡🐍 Electric Snake Bite sent! Window should reload..."
end

# Short alias
function esb --description "⚡🐍 Alias for electric-snake-bite"
    electric-snake-bite
end

# If called directly, run it
if status is-interactive
    # Just define the functions
else
    # Called as script - execute
    electric-snake-bite
end
