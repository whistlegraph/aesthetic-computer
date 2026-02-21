# Emacs Tab Cleanup Plan

## Changes

- [x] 1. **Artery tab first** - Make artery the first tab created (before boot), so it appears leftmost in the tab bar
- [x] 2. **Combine boot + crash** - Merge boot progress display and crash diary into a single "boot" tab
- [x] 3. **Fix top tab** - `htop` not installed; change `ac-top` alias from `htop` to `top`
- [x] 4. **Fix llm tab** - `copilot` command not found; replace with `claude` in `ac-llm`, `ac-llm-continue`, `ac-llm-resume` aliases
- [x] 5. **Combine views into web 2/2** - Move views command into web 2/2 tab and remove standalone views tab

## Files Modified

- `dotfiles/dot_config/emacs.el` - Tab creation order, boot steps, tab specs
- `.devcontainer/config.fish` - `ac-top`, `ac-llm*` aliases
