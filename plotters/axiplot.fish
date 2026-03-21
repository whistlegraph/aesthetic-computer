function axiplot
    if test (count $argv) -lt 1
        echo "usage: axiplot file.svg [optional axicli args]"
        return 1
    end

    axicli $argv
end
