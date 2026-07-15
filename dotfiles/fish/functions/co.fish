function co --wraps=codex --description 'Shorthand for codex (slab-tracked)'
    # codex-slab registers the session as a prompt rock (menubar + prox), then
    # runs codex. Falls back to bare codex if the launcher isn't installed.
    if command -q codex-slab
        codex-slab $argv
    else
        codex $argv
    end
end
