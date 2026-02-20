#!/usr/bin/env fish

# Update the Top 100 KidLisp playlist on feed.aesthetic.computer
# DEPRECATED: Use silo-update-top100.mjs on silo instead — it handles
# all 3 dynamic playlists (Top 100, Top @jeffrey, Top @fifi).
# This script only replaces slot 0 (Top 100) and is kept as a fallback.
#
# Usage:
#   fish update-top100.fish          # Run from codespace / dev machine
#   fish update-top100.fish --local  # Run directly on silo (no SSH tunnel needed)

set SCRIPT_DIR (dirname (status --current-filename))
set VAULT_DIR "$SCRIPT_DIR/../aesthetic-computer-vault"
set VAULT_ENV "$VAULT_DIR/feed/.env"

set LOCAL false
if contains -- --local $argv
    set LOCAL true
end

# Load vault env
if test $LOCAL = true
    if not set -q FEED_API_SECRET
        echo "error: FEED_API_SECRET not set"
        exit 1
    end
    if not set -q MONGODB_CONNECTION_STRING
        echo "error: MONGODB_CONNECTION_STRING not set"
        exit 1
    end
else
    if not test -f $VAULT_ENV
        echo "error: vault env not found at $VAULT_ENV"
        exit 1
    end
    for line in (cat $VAULT_ENV | grep -v '^#' | grep '=')
        set -l parts (string split -m1 '=' $line)
        set -l val (string trim -c '"' $parts[2])
        set -gx $parts[1] $val
    end
end

set FEED_URL "https://feed.aesthetic.computer/api/v1"
set CHANNEL_ID "$KIDLISP_CHANNEL_ID"
if test -z "$CHANNEL_ID"
    set CHANNEL_ID "156c4235-4b24-4001-bec9-61ce0ac7c25e"
end

echo (date -u +"%Y-%m-%d %H:%M:%S UTC")" updating top 100..."

# Step 1: Get current channel
set channel_json (curl -s "$FEED_URL/channels/$CHANNEL_ID" -H "Authorization: Bearer $FEED_API_SECRET")

# Step 2: Generate new Top 100 playlist
cd $SCRIPT_DIR
set output (node create-top-kidlisp-playlist.mjs 2>&1)
set new_top100_id (echo $output | grep -oP 'ID: \K[a-f0-9-]+' | head -1)

if test -z "$new_top100_id"
    echo "error: failed to create Top 100 playlist"
    echo $output
    exit 1
end

echo "  new Top 100: $new_top100_id"

# Step 3: Build updated channel via node (avoid Fish JSON quoting issues)
set update_result (node -e "
const channelJson = JSON.parse(process.argv[1]);
const newTop100Id = process.argv[2];
const feedUrl = process.argv[3];
const apiSecret = process.argv[4];
const channelId = process.argv[5];

// Keep non-Top-100 playlists (Colors, Chords), replace Top 100
const staticPlaylists = (channelJson.playlists || []).filter(url => {
    // Top 100 playlist URLs contain the old top100 ID — skip them
    // Static playlists (Colors, Chords) stay
    return true; // we'll replace all and rebuild
});

// Find static playlist IDs (not the first one, which is Top 100)
const playlistUrls = channelJson.playlists || [];
const staticUrls = playlistUrls.slice(1); // skip old Top 100

// New channel playlists: [new Top 100, ...static]
const newPlaylists = [
    feedUrl + '/playlists/' + newTop100Id,
    ...staticUrls
];

const body = {
    title: channelJson.title,
    curator: channelJson.curator,
    summary: channelJson.summary,
    playlists: newPlaylists
};

// Print old Top 100 ID for cleanup
const oldTop100Url = playlistUrls[0] || '';
const oldTop100Id = oldTop100Url.split('/').pop();
console.log(JSON.stringify({ body, oldTop100Id }));
" "$channel_json" "$new_top100_id" "$FEED_URL" "$FEED_API_SECRET" "$CHANNEL_ID" 2>/dev/null)

set update_body (echo $update_result | python3 -c "import sys,json; print(json.dumps(json.load(sys.stdin)['body']))")
set old_top100_id (echo $update_result | python3 -c "import sys,json; print(json.load(sys.stdin)['oldTop100Id'])")

# Step 4: Update channel
curl -s -X PUT "$FEED_URL/channels/$CHANNEL_ID" -H "Content-Type: application/json" -H "Authorization: Bearer $FEED_API_SECRET" -d "$update_body" > /dev/null
echo "  channel updated"

# Step 5: Delete old Top 100 playlist
if test -n "$old_top100_id"; and test "$old_top100_id" != "$new_top100_id"
    curl -s -X DELETE "$FEED_URL/playlists/$old_top100_id" -H "Authorization: Bearer $FEED_API_SECRET" > /dev/null
    echo "  deleted old: $old_top100_id"
end

# Step 6: Clean up orphaned channels from create-top-kidlisp-playlist.mjs
set orphans (curl -s "$FEED_URL/channels" -H "Authorization: Bearer $FEED_API_SECRET" | python3 -c "
import sys, json
d = json.load(sys.stdin)
for c in d.get('items', []):
    if c['id'] != '$CHANNEL_ID':
        print(c['id'])
" 2>/dev/null)
for cid in $orphans
    curl -s -X DELETE "$FEED_URL/channels/$cid" -H "Authorization: Bearer $FEED_API_SECRET" > /dev/null
end

echo (date -u +"%Y-%m-%d %H:%M:%S UTC")" done."
