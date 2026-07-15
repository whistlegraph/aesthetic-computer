function cor --wraps=codex --description 'Shorthand for codex resume (slab-tracked; picker, --last for most recent)'
    if command -q codex-slab
        codex-slab resume $argv
    else
        codex resume $argv
    end
end
