#!/usr/bin/env fish
# Bulk-archive an Instagram account into portraits/jeffrey/ig-archive/.
# Requires a session file (from ig-import-cookies.py or ig-login.py) to exist.
#
# Usage:
#   ./bin/ig-archive.fish whistlegraph
#   ./bin/ig-archive.fish aesthetic.computer
#
# Sessions live at portraits/jeffrey/sessions/<account>.
# Archive lands at portraits/jeffrey/ig-archive/<account>/.

set username $argv[1]
if test -z "$username"
    echo "usage: ig-archive.fish <username>"
    exit 64
end

set repo_root (realpath (status dirname)/../../..)
set session_file "$repo_root/portraits/jeffrey/sessions/$username"
# Instaloader's --dirname-pattern={profile} creates the per-user subdir; root
# above it so we don't end up with .../whistlegraph/whistlegraph/.
set out_dir "$repo_root/portraits/jeffrey/ig-archive"

if not test -f "$session_file"
    echo "session not found: $session_file"
    echo "bootstrap one of:"
    echo "  bin/ig-import-cookies.py chrome $username    # safer for 2FA accounts"
    echo "  IG_PASSWORD='...' bin/ig-login.py $username  # password flow"
    exit 1
end

mkdir -p "$out_dir"
cd "$out_dir"

# --no-profile-pic: skip avatar (we already have headshots in shoot/)
# --no-captions: skip captions txt; metadata is in the .json
# --no-resume: don't resume from a partial dir; tracks state via fast-update
# --fast-update: skip files we already have (de-dupe between runs)
# --post-metadata-txt '': suppress per-post txt files
# --geotags: include geotag JSON (tiny; useful provenance)
instaloader \
    --login=$username \
    --sessionfile=$session_file \
    --dirname-pattern={profile} \
    --filename-pattern={date_utc:%Y-%m-%d}_{shortcode} \
    --fast-update \
    --no-captions \
    --no-profile-pic \
    --post-metadata-txt='' \
    profile $username

echo ""
echo "archive: $out_dir/$username"
echo "post count: "(ls $out_dir/$username/*.jpg 2>/dev/null | wc -l)
