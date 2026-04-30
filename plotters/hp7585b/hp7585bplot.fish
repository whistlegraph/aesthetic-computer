function hp7585bplot
    if test (count $argv) -lt 1
        echo "usage: hp7585bplot file.hpgl"
        return 1
    end

    if not set -q HP7585B_TTY
        echo "HP7585B_TTY not set. e.g. set -Ux HP7585B_TTY /dev/cu.usbserial-XXXX"
        return 1
    end

    if not test -e $HP7585B_TTY
        echo "device $HP7585B_TTY does not exist"
        return 1
    end

    # 9600 8N1, Xon/Xoff. -clocal so flow control is honored on macOS.
    stty -f $HP7585B_TTY 9600 cs8 -cstopb -parenb -clocal ixon ixoff

    cat $argv[1] > $HP7585B_TTY
end
