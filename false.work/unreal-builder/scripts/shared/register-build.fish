#!/usr/bin/env fish
# Register a new build via Netlify function (stores in MongoDB)
# Usage: source this file, then call:
#   register_build PLATFORM VERSION TIMESTAMP SIZE_MB DOWNLOAD_URL [LEVEL] [UE_VERSION] [LOG_URL] [CHANGELIST] [BUILD_TYPE]
#
# Example:
#   register_build "windows" "2025.11.26-1500" "2025-11-26T15:00:00" "1342" \
#     "https://assets.aesthetic.computer/false.work/spiderlily-windows-2025.11.26-1500.zip" \
#     "L_VerticalSlice_Demo" "UE_5.6" \
#     "https://assets.aesthetic.computer/false.work/spiderlily-windows-2025.11.26-1500.txt"

function register_build
    set platform $argv[1]
    set version $argv[2]
    set timestamp $argv[3]
    set size_mb $argv[4]
    set download_url $argv[5]
    set level $argv[6]
    set ue_version $argv[7]
    set log_url $argv[8]
    set changelist $argv[9]
    set build_type $argv[10]

    # Validate required args
    if test -z "$platform" -o -z "$version" -o -z "$timestamp" -o -z "$size_mb" -o -z "$download_url"
        echo "❌ Usage: register_build PLATFORM VERSION TIMESTAMP SIZE_MB DOWNLOAD_URL [LEVEL] [UE_VERSION] [LOG_URL] [CHANGELIST] [BUILD_TYPE]"
        return 1
    end

    # Load API key from vault
    set VAULT_DIR /workspaces/aesthetic-computer/aesthetic-computer-vault/false.work
    set API_KEY_FILE $VAULT_DIR/builds-api-key.txt

    if not test -f $API_KEY_FILE
        echo "❌ API key not found at: $API_KEY_FILE"
        return 1
    end

    set API_KEY (cat $API_KEY_FILE | tr -d '\n')

    if test -z "$API_KEY"
        echo "❌ API key file is empty"
        return 1
    end

    echo "📤 Registering build: $platform $version"

    # Build JSON payload using jq for proper escaping
    set json_payload (jq -n \
        --arg platform "$platform" \
        --arg version "$version" \
        --arg timestamp "$timestamp" \
        --argjson sizeMB "$size_mb" \
        --arg downloadUrl "$download_url" \
        --arg level "$level" \
        --arg ueVersion "$ue_version" \
        --arg logUrl "$log_url" \
        --arg changelist "$changelist" \
        --arg buildType "$build_type" \
        '{
            platform: $platform,
            version: $version,
            timestamp: $timestamp,
            sizeMB: $sizeMB,
            downloadUrl: $downloadUrl
        } + (if $level != "" then {level: $level} else {} end)
          + (if $ueVersion != "" then {ueVersion: $ueVersion} else {} end)
          + (if $logUrl != "" then {logUrl: $logUrl} else {} end)
          + (if $changelist != "" then {changelist: $changelist} else {} end)
          + (if $buildType != "" then {buildType: $buildType} else {} end)')

    # Call Netlify function
    set response (curl -s -w "\n%{http_code}" \
        -X POST \
        -H "Content-Type: application/json" \
        -H "X-API-Key: $API_KEY" \
        -d "$json_payload" \
        "https://aesthetic.computer/.netlify/functions/register-build")

    set http_code (echo "$response" | tail -1)
    set body (echo "$response" | sed '$d')

    switch $http_code
        case 201
            echo "✅ Build registered successfully: $platform $version"
            return 0
        case 409
            echo "⚠️  Build already exists: $platform $version"
            return 0
        case 401
            echo "❌ Authentication failed - check API key"
            return 1
        case 400
            echo "❌ Bad request: $body"
            return 1
        case '*'
            echo "❌ Failed to register build (HTTP $http_code)"
            echo "   Response: $body"
            return 1
    end
end
