# Easel command integration for Fish.
#
# Easel claims `ac` and `easel`. It deliberately does not claim `aesthetic`
# any more: that name belongs to the Aesthetic Computer platform helper, and
# the tool only held it while it was still called Aesthetic Code.
if functions -q ac; and not functions -q ac-repo
    functions --copy ac ac-repo
end

functions --erase ac easel

function ac --description 'Open Easel'
    command $HOME/.local/bin/easel $argv
end

function easel --description 'Open Easel'
    command $HOME/.local/bin/easel $argv
end
