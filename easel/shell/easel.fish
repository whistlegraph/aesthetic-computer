# Aesel command integration for Fish.
#
# Aesel claims `ac` and `easel`. It deliberately does not claim `aesthetic`
# any more: that name belongs to the Aesthetic Computer platform helper, and
# the tool only held it while it was still called Aesthetic Code.
if functions -q ac; and not functions -q ac-repo
    functions --copy ac ac-repo
end

functions --erase ac easel

function ac --description 'Open Aesel in this terminal'
    command $HOME/.local/bin/ac $argv
end

function easel --description 'Open Aesel'
    command $HOME/.local/bin/easel $argv
end

function aesel --description 'Open native Aesel GUI'
    command $HOME/.local/bin/aesel $argv
end

function a --description 'Open Aesel TUI in pro mode'
    command $HOME/.local/bin/a $argv
end
