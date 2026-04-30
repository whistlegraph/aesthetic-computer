function hp7585bsend
    if test (count $argv) -lt 1
        echo 'usage: hp7585bsend "PU;PA0,0;"'
        return 1
    end

    if not set -q HP7585B_TTY
        echo "HP7585B_TTY not set. e.g. set -Ux HP7585B_TTY /dev/cu.usbserial-XXXX"
        return 1
    end

    stty -f $HP7585B_TTY 9600 cs8 -cstopb -parenb -clocal ixon ixoff
    printf "%s" "$argv" > $HP7585B_TTY
end
