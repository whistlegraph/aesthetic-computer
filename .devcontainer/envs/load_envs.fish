function load_envs
    set -l env_file /home/me/envs/devcontainer.env

    if not test -f $env_file
        echo "ℹ️  No devcontainer.env found at $env_file"
        return 0
    end

    set -l loaded 0
    while read -l line
        set line (string trim $line)
        if test -z "$line"
            continue
        end
        if string match -q "#*" $line
            continue
        end
        if not string match -q "*=*" $line
            continue
        end

        if string match -q 'export *' $line
            set line (string replace -r '^export\s+' '' $line)
        end

        set -l kv (string split -m 1 '=' $line)
        set -l key (string trim $kv[1])
        set -l value (string trim $kv[2])

        if test -z "$key"
            continue
        end

        set value (string trim --chars "'" (string trim --chars '"' $value))

        set -gx $key $value
        set loaded (math $loaded + 1)
    end < $env_file

    if test $loaded -gt 0
        echo "🔧 Loaded $loaded environment variable(s) from devcontainer.env"
    else
        echo "ℹ️  devcontainer.env contained no loadable variables"
    end
end
