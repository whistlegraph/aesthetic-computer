#!/usr/bin/env fish
# Upload a patched Unreal plugin + optional test project to the
# assets-aesthetic-computer DigitalOcean Space under `plugins/`.
#
# Mirrors the pattern of upload-to-spaces.sh but for plugin artifacts.
# Game builds still use upload-to-spaces.sh; this script handles the
# "we forked a plugin and want to host the patched version" case.
#
# Reads creds from: aesthetic-computer-vault/false.work/builds-spaces.env
#
# Usage:
#   fish upload-plugin-to-spaces.fish <plugin-slug> <plugin-source-dir> [test-project-dir] [patch-file]
#
# Example:
#   fish upload-plugin-to-spaces.fish serialcom-ue56 \
#       /tmp/serialcom-work/SerialCOM \
#       "C:/Users/me/Documents/Unreal Projects/SpiderlilySerialTest" \
#       /workspaces/aesthetic-computer/false.work/patches/serialcom-ue5.6.patch
#
# Produces and uploads:
#   plugins/<slug>-src.tar.gz               (source only, no binaries)
#   plugins/<slug>-packaged-win64.tar.gz    (source + Binaries/Win64/*.dll, no pdbs)  [if Binaries/ present]
#   plugins/<slug>-testproject.zip          (cleaned test project with plugin dropped in) [if test-project-dir given]
#   plugins/<slug>.patch                    (patch vs upstream) [if patch-file given]
#   plugins/<slug>-*-latest.*               (copy-aliases pointing at the newest upload)
#
# The script is idempotent: re-running with the same slug overwrites the previous files.

set -l script_dir (dirname (status -f))
set -l repo_root (realpath "$script_dir/../../..")
set -l vault_dir "$repo_root/aesthetic-computer-vault/false.work"
set -l env_file "$vault_dir/builds-spaces.env"

if not test -f "$env_file"
    echo "❌ $env_file not found — decrypt the vault first:"
    echo "   cd $repo_root/aesthetic-computer-vault && fish vault-tool.fish unlock"
    exit 1
end

# Source the env file (bash-style KEY=VALUE)
for line in (cat "$env_file" | grep -v '^#' | grep '=')
    set -l kv (string split -m 1 '=' $line)
    if test (count $kv) -eq 2
        set -gx $kv[1] $kv[2]
    end
end

for var in BUILDS_SPACES_KEY BUILDS_SPACES_SECRET BUILDS_SPACES_BUCKET BUILDS_SPACES_REGION
    if not set -q $var
        echo "❌ $var not set after sourcing $env_file"
        exit 1
    end
end

if test (count $argv) -lt 2
    echo "Usage: fish upload-plugin-to-spaces.fish <slug> <plugin-source-dir> [test-project-dir] [patch-file]"
    exit 1
end

set -l slug $argv[1]
set -l plugin_dir $argv[2]
set -l testproject_dir ""
set -l patch_file ""
if test (count $argv) -ge 3
    set testproject_dir $argv[3]
end
if test (count $argv) -ge 4
    set patch_file $argv[4]
end

if not test -d "$plugin_dir"
    echo "❌ Plugin source dir not found: $plugin_dir"
    exit 1
end
if test -n "$patch_file"; and not test -f "$patch_file"
    echo "❌ Patch file not found: $patch_file"
    exit 1
end

# Stage files in a temp workspace with a clean "SerialCOM" (or whatever) root dir
set -l work (mktemp -d /tmp/plugin-upload-XXXXXX)
set -l plugin_name (basename $plugin_dir)
cp -r "$plugin_dir" "$work/$plugin_name"

# Build the source-only tarball (exclude Binaries, Intermediate, HostProject)
echo "📦 Packaging $slug-src.tar.gz (source only)..."
tar czf "$work/$slug-src.tar.gz" \
    --exclude='Binaries' \
    --exclude='Intermediate' \
    --exclude='HostProject*' \
    --exclude='*.pdb' \
    -C "$work" $plugin_name

# If the source dir has prebuilt Binaries/Win64/*.dll, build a packaged tarball too
if test -d "$plugin_dir/Binaries/Win64"
    echo "📦 Packaging $slug-packaged-win64.tar.gz (source + Win64 .dll, no pdb)..."
    tar czf "$work/$slug-packaged-win64.tar.gz" \
        --exclude='Intermediate' \
        --exclude='HostProject*' \
        --exclude='*.pdb' \
        -C "$work" $plugin_name
end

# If a test project dir was passed, zip it up (excluding build artifacts)
if test -n "$testproject_dir"; and test -d "$testproject_dir"
    set -l tp_name (basename "$testproject_dir")
    set -l tp_staging "$work/tp-staging"
    mkdir -p "$tp_staging"
    cp -r "$testproject_dir" "$tp_staging/$tp_name"
    echo "📦 Packaging $slug-testproject.zip..."
    cd "$tp_staging"
    zip -qr "$work/$slug-testproject.zip" $tp_name \
        -x '*/Intermediate/*' '*/Saved/*' '*/.vs/*' '*/*.sln' \
           '*/*.vcxproj' '*/*.vcxproj.*' '*.pdb' '*/*.target'
    cd -
end

# If a patch file was passed, stage it with a stable slug-based CDN name.
if test -n "$patch_file"
    echo "📦 Staging $slug.patch..."
    cp "$patch_file" "$work/$slug.patch"
end

# Build s3cmd config
set -l s3cfg (mktemp /tmp/s3cfg-XXXXXX)
printf '[default]
access_key = %s
secret_key = %s
host_base = %s.digitaloceanspaces.com
host_bucket = %%(bucket)s.%s.digitaloceanspaces.com
use_https = True
' $BUILDS_SPACES_KEY $BUILDS_SPACES_SECRET $BUILDS_SPACES_REGION $BUILDS_SPACES_REGION > "$s3cfg"
chmod 600 "$s3cfg"

# Upload everything we built
set -l uploaded
for f in (ls "$work"/$slug-*.tar.gz "$work"/$slug-*.zip "$work"/$slug.patch 2>/dev/null)
    set -l name (basename $f)
    echo "☁️  Uploading $name..."
    if s3cmd -c "$s3cfg" put "$f" "s3://$BUILDS_SPACES_BUCKET/plugins/$name" \
        --acl-public --no-mime-magic --guess-mime-type --no-progress 2>&1 | tail -2
        set uploaded $uploaded $name
    end
end

# Create -latest aliases
for name in $uploaded
    # serialcom-ue56-src.tar.gz -> serialcom-ue56-src-latest.tar.gz
    # serialcom-ue56.patch -> serialcom-ue56-latest.patch
    set -l base $name
    set -l ext ""
    if string match -q "*.tar.gz" -- $name
        set base (string replace -r '\.tar\.gz$' '' $name)
        set ext ".tar.gz"
    else if string match -q "*.zip" -- $name
        set base (string replace -r '\.zip$' '' $name)
        set ext ".zip"
    else if string match -q "*.patch" -- $name
        set base (string replace -r '\.patch$' '' $name)
        set ext ".patch"
    end
    set -l latest "$base-latest$ext"
    echo "🔗 Aliasing $name -> $latest"
    s3cmd -c "$s3cfg" cp "s3://$BUILDS_SPACES_BUCKET/plugins/$name" \
        "s3://$BUILDS_SPACES_BUCKET/plugins/$latest" --acl-public 2>&1 | tail -1
end

rm -f "$s3cfg"
rm -rf "$work"

echo ""
echo "✅ Done. CDN URLs:"
for name in $uploaded
    echo "   https://$BUILDS_SPACES_BUCKET.$BUILDS_SPACES_REGION.cdn.digitaloceanspaces.com/plugins/$name"
end
echo ""
echo "Next step: register the plugin in MongoDB so it shows up on builds.false.work:"
echo "   fish $script_dir/register-serialcom-plugin.fish  # (or your plugin-specific registration script)"
