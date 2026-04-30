function hp7585bpen
    if test (count $argv) -lt 1
        echo "usage: hp7585bpen N   (0 = stow, 1-8 = carousel slot)"
        return 1
    end

    set n $argv[1]
    if not string match -qr '^[0-8]$' -- $n
        echo "pen number must be 0..8"
        return 1
    end

    hp7585bsend "SP$n;"
end
