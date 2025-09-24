# Import devcontainer env files.
function load_envs
    for env_file in /home/me/envs/*.env
        if test -f $env_file
            echo "Loading environment variables from: $env_file"
            for line in (cat $env_file)
                # Skip empty lines and comments
                if test -n "$line" -a (string sub -s 1 -l 1 "$line") != "#"
                    # Split only on the first '=' to handle values with '=' in them
                    set key_value (string split -m 1 "=" $line)
                    if test (count $key_value) -eq 2
                        set -gx $key_value[1] $key_value[2]
                    else
                        echo "  Skipped invalid line: $line"
                    end
                end
            end
        end
    end
end
