function go --description 'go dark|light → flip macOS appearance on neo + reachable tailscale macs; anything else → real go'
    switch "$argv[1]"
        case dark light
            set -l mode $argv[1]
            set -l val true
            test "$mode" = light; and set val false

            # AppleScript that flips Dark Mode for the logged-in GUI session.
            set -l osa "tell application \"System Events\" to tell appearance preferences to set dark mode to $val"

            # Local host (this machine, neo).
            osascript -e "$osa" >/dev/null 2>&1
            printf '  %-9s → %s\n' (hostname -s) $mode

            # Remote macs over ssh, flipped in parallel; unreachable ones skipped.
            for h in panda chicken blueberry
                begin
                    if ssh -o ConnectTimeout=4 -o BatchMode=yes $h "osascript -e '$osa'" >/dev/null 2>&1
                        printf '  %-9s → %s\n' $h $mode
                    else
                        printf '  %-9s — unreachable\n' $h
                    end
                end &
            end
            wait
            return 0
        case '*'
            command go $argv
    end
end
