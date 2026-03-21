#!/usr/bin/env fish
# Load .env and run create-account.mjs with environment variables

# Load environment variables from vault .env
set env_file (dirname (status -f))/../../aesthetic-computer-vault/at/.env

if test -f $env_file
    for line in (cat $env_file | grep -v '^#' | grep -v '^$')
        set -x (string split -m 1 '=' $line)
    end
else
    echo "❌ .env file not found: $env_file"
    exit 1
end

# Run create-account.mjs with all arguments
node (dirname (status -f))/create-account.mjs $argv
