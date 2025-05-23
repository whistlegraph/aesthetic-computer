# Import devcontainer env files.
function load_envs
    for env_file in /home/me/envs/*.env
        if test -f $env_file
            for line in (cat $env_file)
                set key_value (string split "=" $line)
                set -gx $key_value[1] $key_value[2]
            end
        end
    end
end
