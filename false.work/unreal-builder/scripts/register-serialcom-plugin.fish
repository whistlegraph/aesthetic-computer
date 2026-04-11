#!/usr/bin/env fish
# Register the patched SerialCOM plugin with the builds.false.work database.
# Posts metadata + CDN URLs to /.netlify/functions/register-plugin so the plugin
# shows up under the "🔌 Plugins" tab on https://builds.false.work/.
#
# Prerequisite: the artifacts must already be uploaded to
# s3://assets-aesthetic-computer/plugins/ (use upload-plugin-to-spaces.fish).
#
# Usage:
#   fish register-serialcom-plugin.fish [--production | --local]
#
# Default target is production (https://aesthetic.computer). Use --local to
# hit http://localhost:8888 for dev testing.

set -l script_dir (dirname (status -f))
set -l repo_root (realpath "$script_dir/../../..")
set -l vault_dir "$repo_root/aesthetic-computer-vault/false.work"
set -l api_key_file "$vault_dir/builds-api-key.txt"

if not test -f "$api_key_file"
    echo "❌ $api_key_file not found — decrypt the vault first:"
    echo "   cd $repo_root/aesthetic-computer-vault && fish vault-tool.fish unlock"
    exit 1
end

set -l api_key (cat "$api_key_file" | string trim)

set -l base_url "https://aesthetic.computer"
if contains -- --local $argv
    set base_url "http://localhost:8888"
end

set -l endpoint "$base_url/.netlify/functions/register-plugin"
set -l cdn_base "https://assets-aesthetic-computer.sfo3.cdn.digitaloceanspaces.com/plugins"

# File sizes on CDN (approx, for display in the UI)
set -l size_src 0.10
set -l size_pkg 0.15
set -l size_tp  0.19
set -l size_patch 0.003

echo "Registering SerialCOM plugin at $endpoint"

set -l payload (printf '{
  "name": "SerialCOM",
  "displayName": "Communication Serial Port (Serial COM)",
  "version": "4.5.1.1.1-ue56",
  "engineVersion": "5.6.1",
  "platforms": ["Win64"],
  "category": "Input",
  "description": "Serial/COM port communication for UE 5.6.1 — fork of videofeedback/Unreal_Engine_SerialCOM_Plugin (Ramiro Montes De Oca) with two UE 5.6 compat patches: EngineVersion bump in the .uplugin, and TArray::Pop(bool) -> Pop(EAllowShrinking::No). Win64 only; wraps Win32 CreateFile/ReadFile/WriteFile + DCB. Blueprint-callable (Open Port, Read/Write String/Bytes, Flush, etc.). Verified via RunUAT BuildPlugin and full project build on 2026-04-11.",
  "upstream": "https://github.com/videofeedback/Unreal_Engine_SerialCOM_Plugin",
  "artifacts": [
    {
      "kind": "source",
      "label": "Source (patched)",
      "filename": "serialcom-ue56-src.tar.gz",
      "sizeMB": %s,
      "downloadUrl": "%s/serialcom-ue56-src.tar.gz"
    },
    {
      "kind": "packaged-win64",
      "label": "Prebuilt Win64",
      "filename": "serialcom-ue56-packaged-win64.tar.gz",
      "sizeMB": %s,
      "downloadUrl": "%s/serialcom-ue56-packaged-win64.tar.gz"
    },
    {
      "kind": "test-project",
      "label": "Blank test project",
      "filename": "serialcom-ue56-testproject.zip",
      "sizeMB": %s,
      "downloadUrl": "%s/serialcom-ue56-testproject.zip"
    },
    {
      "kind": "patch",
      "label": "Diff vs upstream",
      "filename": "serialcom-ue56.patch",
      "sizeMB": %s,
      "downloadUrl": "%s/serialcom-ue56.patch"
    }
  ]
}' $size_src $cdn_base $size_pkg $cdn_base $size_tp $cdn_base $size_patch $cdn_base)

curl -sS -X POST "$endpoint" \
    -H "Content-Type: application/json" \
    -H "x-api-key: $api_key" \
    -d "$payload"

echo ""
