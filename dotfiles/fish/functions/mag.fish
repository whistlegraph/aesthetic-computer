function mag --description 'Privately submit a magnet link and return an opaque transfer ID'
    if test (count $argv) -eq 0
        echo "usage: mag 'magnet:?xt=urn:btih:…'" >&2
        return 2
    end

    command node "$HOME/plugins/private-magnet/scripts/private-magnet.mjs" add "$argv[1]"
end
