#!/usr/bin/env fish
# Register a new build via Netlify function (stores in MongoDB)
# Usage: source this file, then call:
#   register_build PLATFORM VERSION TIMESTAMP SIZE_MB DOWNLOAD_URL [LEVEL] [UE_VERSION] [LOG_URL] [CHANGELIST] [BUILD_TYPE] [TAGS]
#
# Example:
#   register_build "windows" "2025.11.26-1500" "2025-11-26T15:00:00" "1342" \
#     "https://assets.aesthetic.computer/false.work/spiderlily-windows-2025.11.26-1500.zip" \
#     "L_VerticalSlice_Demo" "UE_5.6" \
#     "https://assets.aesthetic.computer/false.work/spiderlily-windows-2025.11.26-1500.txt" \
#     "" "" "ultra,cinematic"

function register_build
    set build_platform $argv[1]
    set build_version $argv[2]
    set build_timestamp $argv[3]
    set size_mb $argv[4]
    set download_url $argv[5]
    set level $argv[6]
    set ue_version $argv[7]
    set log_url $argv[8]
    set changelist $argv[9]
    set build_type $argv[10]
    set tags $argv[11]

    # Validate required args
    if test -z "$build_platform" -o -z "$build_version" -o -z "$build_timestamp" -o -z "$size_mb" -o -z "$download_url"
        echo "❌ Usage: register_build PLATFORM VERSION TIMESTAMP SIZE_MB DOWNLOAD_URL [LEVEL] [UE_VERSION] [LOG_URL] [CHANGELIST] [BUILD_TYPE] [TAGS]"
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

    echo "📤 Registering build: $build_platform $build_version"

    # Convert comma-separated tags to JSON array
    set tags_json "[]"
    if test -n "$tags"
        set tags_json (echo "$tags" | tr ',' '\n' | jq -R . | jq -s .)
    end

    # Build JSON payload using jq for proper escaping
    set json_payload (jq -n \
        --arg platform "$build_platform" \
        --arg version "$build_version" \
        --arg timestamp "$build_timestamp" \
        --argjson sizeMB "$size_mb" \
        --arg downloadUrl "$download_url" \
        --arg level "$level" \
        --arg ueVersion "$ue_version" \
        --arg logUrl "$log_url" \
        --arg changelist "$changelist" \
        --arg buildType "$build_type" \
        --argjson tags "$tags_json" \
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
          + (if $buildType != "" then {buildType: $buildType} else {} end)
          + (if ($tags | length) > 0 then {tags: $tags} else {} end)')

    # Call Netlify function - use temp file to separate body and http_code
    set tmp_file (mktemp)
    set http_code (curl -s -w '%{http_code}' -o "$tmp_file" \
        -X POST \
        -H "Content-Type: application/json" \
        -H "X-API-Key: $API_KEY" \
        -d "$json_payload" \
        "https://aesthetic.computer/.netlify/functions/register-build")
    set body (cat "$tmp_file")
    rm -f "$tmp_file"

    switch $http_code
        case 201
            echo "✅ Build registered successfully: $build_platform $build_version"
            return 0
        case 409
            echo "⚠️  Build already exists: $build_platform $build_version"
            return 0
        case 401
            echo "❌ Authentication failed - check API key or ensure BUILDS_API_KEY is set in Netlify"
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
