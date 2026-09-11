# Aesthetic Code command integration for Fish.
# Preserve older Aesthetic Computer helpers under explicit names before the
# product claims `ac` and `aesthetic`.
if functions -q ac; and not functions -q ac-repo
    functions --copy ac ac-repo
end

if functions -q aesthetic; and not functions -q aesthetic-platform
    functions --copy aesthetic aesthetic-platform
end

functions --erase ac aesthetic

function ac --description 'Open Aesthetic Code'
    command $HOME/.local/bin/aesthetic $argv
end

function aesthetic --description 'Open Aesthetic Code'
    command $HOME/.local/bin/aesthetic $argv
end
