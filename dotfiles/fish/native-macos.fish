# Shared native shell for the Mac family. Machine overrides: config.local.fish.
set -gx AC_ROOT (path resolve (status dirname)/../..)
set -gx AESTHETIC $USER
set -gx PATH /opt/homebrew/bin /opt/homebrew/sbin /usr/local/bin $HOME/.local/bin $PATH
if test -d $AC_ROOT/aesthetic-computer-vault
    set -gx AESTHETIC_VAULT $AC_ROOT/aesthetic-computer-vault
else if test -d $HOME/aesthetic-computer-vault
    set -gx AESTHETIC_VAULT $HOME/aesthetic-computer-vault
end
if not status is-interactive
    set -gx nogreet true
end
if type -q fnm
    fnm env --use-on-cd --shell fish | source
end
set -gx PAGER cat
set -gx GIT_PAGER cat
set -gx MANPAGER cat
source $AC_ROOT/.devcontainer/config.fish
set -g fish_function_path $AC_ROOT/dotfiles/fish/functions $fish_function_path
source $AC_ROOT/dotfiles/fish/functions/ac-piece-logs.fish
# The shared devcontainer config still carries a Linux-only reload alias.
alias reload 'source ~/.config/fish/config.fish'
if test -f $AC_ROOT/easel/shell/easel.fish
    source $AC_ROOT/easel/shell/easel.fish
end
if test -f $HOME/.config/fish/config.local.fish
    source $HOME/.config/fish/config.local.fish
end
