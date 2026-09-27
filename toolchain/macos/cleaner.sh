#!/bin/bash
# Cleaner — safe, user-level macOS disk inventory/cleanup for the AC fleet Macs.
# Default is report-only; known regenerable caches require --apply.
set -euo pipefail

APPLY=0
INSTALL=0
THIN_SNAPSHOTS=0
REMOTE_BACKED=0
AUDIT=0
MESSAGES_CACHE=""
UNINSTALL=()

# Open-source tools the cleaner leans on when present (installed by --install):
#   dust          — the audit tree (like du, sorted, one screen)
#   mac-cleanup   — mac-cleanup-py; runs a curated module set after our own pass
#   pearcleaner   — app removal that also takes the app's leftovers
# Each is optional: the cleaner degrades to its own bash lanes without them.
MAC_CLEANUP_CONFIG="$HOME/.mac_cleanup_py"
# Modules that only touch regenerable tool caches. Deliberately absent:
# system_caches/chrome/arc/chromium_caches (no running-app gate), system_log
# (logs are kept on this fleet), ios_backups/ios_apps/dropbox/google_drive/
# steam (user data), docker/xcode_simulators (report-only surfaces above),
# inactive_memory (not disk).
MAC_CLEANUP_MODULES="trash brew gem pyenv npm pnpm yarn bun pod go poetry java_cache gradle composer cacher nuget_cache conan dns_cache wget_logs jetbrains adobe obsidian_caches telegram microsoft_teams"

usage() {
  cat <<'EOF'
Usage: cleaner [--apply] [--remote-backed] [--thin-snapshots] [--audit]
               [--messages-cache[=YYYY-MM-DD]] [--uninstall App.app ...] [--install]

  (no args)          Report disk use and cleanup candidates; change nothing.
  --audit            Add a dust tree of the home folder and the AC checkout,
                     plus Pearcleaner's orphan count, to the report.
  --apply            Clear known regenerable user caches when their app is idle,
                     then run mac-cleanup-py's curated module set if installed.
  --remote-backed    With --apply, verify and clear ignored AC media already
                     recoverable from DigitalOcean Spaces/CDN.
  --thin-snapshots   With --apply, ask tmutil to reclaim local snapshots.
  --messages-cache   With --apply, move iMessage attachments for messages older
                     than the date (default: one year ago) to the Trash. Only
                     runs when Messages in iCloud is on, so they redownload.
  --uninstall APP    Remove an application and its leftovers via Pearcleaner
                     (falls back to the Trash). Repeatable; explicit only.
  --install          Install in ~/.local/bin, enable a weekly LaunchAgent, and
                     brew-install dust, mac-cleanup-py, and Pearcleaner.

The report inventories application caches, developer/build caches, AC generated
trees, remote-backed mirrors, worktrees, vault Git health, and protected media.

Protected: Git history/worktrees, node_modules, Downloads, Documents/Shelf,
model weights, Codex/Claude state, mail archives, photo/video libraries, and
caches owned by a currently running application. Remote-backed cleanup, the
Messages lane, and uninstalls are never part of the unattended weekly run.
EOF
}

while (($#)); do
  case "$1" in
    --apply) APPLY=1 ;;
    --remote-backed) REMOTE_BACKED=1 ;;
    --install) INSTALL=1 ;;
    --thin-snapshots) THIN_SNAPSHOTS=1 ;;
    --audit) AUDIT=1 ;;
    --messages-cache) MESSAGES_CACHE=$(date -v-1y '+%Y-%m-%d') ;;
    --messages-cache=*) MESSAGES_CACHE=${1#--messages-cache=} ;;
    --uninstall)
      [[ $# -ge 2 ]] || { echo "--uninstall needs an application path" >&2; exit 2; }
      UNINSTALL+=("$2"); shift ;;
    -h|--help) usage; exit 0 ;;
    *) echo "Unknown option: $1" >&2; usage >&2; exit 2 ;;
  esac
  shift
done

if ((REMOTE_BACKED && !APPLY)); then
  echo "--remote-backed requires --apply" >&2
  exit 2
fi
if [[ -n "$MESSAGES_CACHE" ]] && ((!APPLY)); then
  echo "--messages-cache requires --apply" >&2
  exit 2
fi
if [[ -n "$MESSAGES_CACHE" ]] && ! date -j -f '%Y-%m-%d' "$MESSAGES_CACHE" '+%s' >/dev/null 2>&1; then
  echo "--messages-cache wants a real YYYY-MM-DD date, got: $MESSAGES_CACHE" >&2
  exit 2
fi

have() { command -v "$1" >/dev/null 2>&1; }

resolve_repo() {
  local candidate script_root
  script_root=$(cd "$(dirname "$0")/../.." 2>/dev/null && pwd || true)
  for candidate in "${AC_REPO:-}" "$HOME/aesthetic-computer" "$script_root"; do
    [[ -n "$candidate" && -d "$candidate/.git" && -f "$candidate/package.json" ]] || continue
    (cd "$candidate" && pwd)
    return 0
  done
  return 1
}

REPO_ROOT=$(resolve_repo || true)
VAULT_ROOT=""
if [[ -n "$REPO_ROOT" && -d "$REPO_ROOT/aesthetic-computer-vault/.git" ]]; then
  VAULT_ROOT="$REPO_ROOT/aesthetic-computer-vault"
elif [[ -d "$HOME/aesthetic-computer-vault/.git" ]]; then
  VAULT_ROOT="$HOME/aesthetic-computer-vault"
fi

human_size() {
  if [[ -e "$1" ]]; then
    du -sh -- "$1" 2>/dev/null | awk '{print $1}' || echo "?"
  else
    echo "0"
  fi
}

human_kb() {
  awk -v kb="${1:-0}" 'BEGIN {
    split("KiB MiB GiB TiB", units, " "); n = kb; i = 1
    while (n >= 1024 && i < 4) { n /= 1024; i++ }
    if (i == 1) printf "%.0f%s", n, units[i]; else printf "%.1f%s", n, units[i]
  }'
}

human_total() {
  local path total=0 size
  for path in "$@"; do
    [[ -e "$path" ]] || continue
    size=$(du -sk -- "$path" 2>/dev/null | awk '{print $1}' || true)
    [[ "$size" =~ ^[0-9]+$ ]] && total=$((total + size))
  done
  human_kb "$total"
}

named_size() {
  local root="$1" name="$2" kind="${3:-d}" total
  [[ -d "$root" ]] || { echo 0; return; }
  total=$(find "$root" -name "$name" -type "$kind" -prune -exec du -sk -- {} + 2>/dev/null |
    awk '{sum += $1} END {print sum + 0}')
  human_kb "$total"
}

row() {
  printf '%-38s %10s  %s\n' "$1" "$2" "$3"
}

data_volume() {
  [[ -d /System/Volumes/Data ]] && echo /System/Volumes/Data || echo /
}

disk_used_kb() {
  df -k "$(data_volume)" | awk 'NR == 2 {print $3}'
}

report() {
  local volume snapshots repo_label
  volume=$(data_volume)
  snapshots=$(tmutil listlocalsnapshots / 2>/dev/null | grep -c '^com\.apple\.' || true)
  repo_label=${REPO_ROOT:-not-found}

  echo "Cleaner on $(scutil --get LocalHostName 2>/dev/null || hostname) — $(date '+%Y-%m-%dT%H:%M:%S%z')"
  echo
  df -h "$volume" | awk 'NR == 1 || NR == 2'
  echo "Local snapshots: $snapshots"
  echo "AC repository: $repo_label"
  echo
  row "Path/category" "Size" "Policy"
  row "--------------------------------------" "----------" "------"
  echo "APPLICATION + USER CACHES"
  row "Chrome caches" "$(human_total "$HOME/Library/Caches/Google" "$HOME/Library/Caches/com.google.Chrome")" "safe when Chrome idle"
  row "Browser automation downloads" "$(human_total "$HOME/.cache/puppeteer" "$HOME/Library/Caches/ms-playwright" "$HOME/Library/Caches/Cypress")" "safe when browser tooling idle"
  row "Electron/build downloads" "$(human_total "$HOME/Library/Caches/electron" "$HOME/Library/Caches/electron-builder" "$HOME/.cache/electron" "$HOME/.cache/node-gyp")" "safe when JS builds idle"
  row "Spotify cache" "$(human_size "$HOME/Library/Caches/com.spotify.client")" "safe when Spotify idle"
  row "Final Cut cache" "$(human_total "$HOME/Library/Caches/com.apple.FinalCut" "$HOME/Library/Caches/com.apple.FinalCutTrial")" "safe when Final Cut idle"
  row "Xcode DerivedData" "$(human_size "$HOME/Library/Developer/Xcode/DerivedData")" "safe when Xcode idle"
  row "SwiftPM caches" "$(human_total "$HOME/Library/Caches/org.swift.swiftpm" "$HOME/.swiftpm/cache")" "safe when Swift idle"
  row "pip cache" "$(human_size "$HOME/Library/Caches/pip")" "safe when pip idle"
  row "npm content cache" "$(human_size "$HOME/.npm/_cacache")" "safe when npm idle"
  row "Homebrew cache" "$(human_size "$HOME/Library/Caches/Homebrew")" "safe when brew idle"
  row "mu mail index vestige" "$(human_size "$HOME/.cache/mu")" "safe; canonical mail lives on jasellite"
  row "Messages cache" "$(human_size "$HOME/Library/Messages/Caches")" "safe when Messages idle"
  row "Messages attachments" "$(human_size "$HOME/Library/Messages/Attachments")" "$(messages_attachments_policy)"
  row "Slab recordings" "$(human_size "$HOME/.local/share/slab/sessions")" "keeps last 7 days"
  row "Trash" "$(human_size "$HOME/.Trash")" "safe with --apply"

  echo
  echo "DEVELOPER + AC WORKSPACE SURFACES"
  if [[ -n "$REPO_ROOT" ]]; then
    row "AC Git history" "$(human_size "$REPO_ROOT/.git")" "PROTECTED"
    row "AC node_modules (root + system)" "$(human_total "$REPO_ROOT/node_modules" "$REPO_ROOT/system/node_modules")" "PROTECTED; dependency state"
    row "Swift .build trees" "$(named_size "$REPO_ROOT" .build)" "safe when Swift idle"
    row "Whistlegraph heavy downloads" "$(human_total "$REPO_ROOT/toolchain/whistlegraph/downloads/video" "$REPO_ROOT/toolchain/whistlegraph/downloads/site" "$REPO_ROOT/toolchain/whistlegraph/downloads/glyphs" "$REPO_ROOT/toolchain/whistlegraph/downloads/longest")" "REMOTE-BACKED; verified mode only"
    row "Local assets mirror" "$(human_size "$REPO_ROOT/system/public/assets")" "REMOTE-BACKED; tracked files preserved"
    row "Pop out/ generations" "$(named_size "$REPO_ROOT/pop" out)" "REMOTE-BACKED; verify private workspace"
    row "Recap out + venv + models" "$(human_total "$REPO_ROOT/recap/out" "$REPO_ROOT/recap/.venv" "$REPO_ROOT/recap/models")" "REPORT; models/costly outputs protected"
    row "Lith generated output" "$(human_size "$REPO_ROOT/lith/scripts/out")" "REPORT; generated"
    row "Attached Git worktrees" "$(human_size "$REPO_ROOT/.worktrees")" "PROTECTED; may be dirty"
  else
    row "AC repository surfaces" "not found" "set AC_REPO to inventory a nonstandard checkout"
  fi
  if [[ -n "$VAULT_ROOT" ]]; then
    row "Vault Git database" "$(human_size "$VAULT_ROOT/.git")" "PROTECTED; use git gc, never manual deletion"
    row "Vault tmp_pack garbage" "$(named_size "$VAULT_ROOT/.git/objects/pack" 'tmp_pack_*' f)" "REPORT; reclaim only through git gc"
    row "Vault film out/ trees" "$(named_size "$VAULT_ROOT/film" out)" "PROTECTED; archive takes before pruning"
  else
    row "Vault surfaces" "not found" "private checkout not present"
  fi

  echo
  echo "LARGE PROTECTED / MANUAL SURFACES"
  row "Documents/Shelf" "$(human_size "$HOME/Documents/Shelf")" "PROTECTED; curate manually"
  row "Final Cut libraries" "$(named_size "$HOME/Movies" '*.fcpbundle')" "PROTECTED; project media"
  row "Photos library" "$(human_size "$HOME/Pictures/Photos Library.photoslibrary")" "PROTECTED"
  row "Agent state (.codex + .claude)" "$(human_total "$HOME/.codex" "$HOME/.claude")" "PROTECTED; conversation/session state"
  row "Local model stores" "$(human_total "$HOME/.cache/huggingface" "$HOME/.insightface" "$HOME/.brainglobe" "$HOME/.whisper-models")" "PROTECTED; inspect model-by-model"
  row "Mail archive vestige" "$(human_size "$HOME/.mail-all")" "PROTECTED; confirm against jasellite first"
  row "Docker VM/data" "$(human_size "$HOME/Library/Containers/com.docker.docker/Data")" "REPORT; prune through Docker"
  row "CoreSimulator user data" "$(human_size "$HOME/Library/Developer/CoreSimulator")" "REPORT; use simctl"
  row "CoreSimulator runtimes" "$(human_size /Library/Developer/CoreSimulator/Volumes)" "REPORT; use simctl"
  echo
  echo "OPEN-SOURCE HELPERS"
  row "dust (audit tree)" "$(tool_state dust)" "cleaner --audit"
  row "mac-cleanup-py (curated modules)" "$(tool_state mac-cleanup)" "$(mac_cleanup_state)"
  row "Pearcleaner (app + leftovers)" "$(tool_state pearcleaner)" "cleaner --uninstall App.app"
  if ((AUDIT)); then
    audit
  fi
  echo
  if ((APPLY == 0)); then
    echo "Report only. Run 'cleaner --apply' to clear safe caches."
    echo "For bucket-backed AC media: cleaner --apply --remote-backed"
    echo "For the iMessage attachment cache: cleaner --apply --messages-cache"
  fi
}

tool_state() {
  have "$1" && echo "installed" || echo "missing"
}

mac_cleanup_state() {
  local estimate
  if ! have mac-cleanup; then echo "cleaner --install adds it"
  elif [[ ! -f "$MAC_CLEANUP_CONFIG" ]]; then echo "unconfigured; cleaner --install writes the module set"
  else
    # Its dry run ends in a Continue? prompt; answering n keeps it an estimate.
    estimate=$(echo n | mac-cleanup -n -f 2>/dev/null | grep -oE 'Approx [0-9.]+ [KMGT]?B' | sed 's/Approx //' || true)
    echo "${estimate:+~$estimate; }runs after --apply"
  fi
}

messages_icloud_on() {
  [[ -d "$HOME/Library/Messages/CloudKitMetaData" ]]
}

messages_attachments_policy() {
  if messages_icloud_on; then echo "REMOTE-BACKED (Messages in iCloud); --messages-cache"
  else echo "PROTECTED; Messages in iCloud is off"
  fi
}

# The dust tree replaces a dozen du invocations: one screen per root, biggest
# last, no percent bars so it reads in a log.
audit() {
  local root
  echo
  echo "AUDIT (dust, depth 1, top 14)"
  if ! have dust; then
    echo "  dust is not installed; cleaner --install adds it"
    return
  fi
  for root in "$HOME" "$HOME/Library" "${REPO_ROOT:-}"; do
    [[ -n "$root" && -d "$root" ]] || continue
    echo "  $root"
    dust -d 1 -n 14 -c -b -x -- "$root" 2>/dev/null | sed 's/^/    /'
  done
  if have pearcleaner; then
    local orphans
    orphans=$(pearcleaner list-orphaned 2>/dev/null | grep -c '^/' || true)
    echo "  Pearcleaner orphan candidates: ${orphans:-0} (report only — its list includes"
    echo "  extensions of installed apps, so never automate remove-orphaned)"
  fi
}

clean_contents() {
  [[ -d "$1" ]] || return 0
  if ! find "$1" -mindepth 1 -maxdepth 1 -exec rm -rf -- {} +; then
    skip "could not fully clear $1; continuing with other caches"
  fi
}

skip() {
  echo "  skip: $1"
}

clean_ignored() {
  [[ -n "$REPO_ROOT" ]] || return 1
  git -C "$REPO_ROOT" clean -fdX -- "$@"
}

verify_whistlegraph_remote() {
  local downloads posts local_ids remote_ids sample_url
  downloads="$REPO_ROOT/toolchain/whistlegraph/downloads"
  posts="$REPO_ROOT/system/public/whistlegraph.org/posts.json"
  [[ -d "$downloads" && -f "$posts" ]] || return 1
  command -v jq >/dev/null && command -v curl >/dev/null || return 1
  local_ids=$(mktemp /tmp/cleaner-whistlegraph-local.XXXXXX)
  remote_ids=$(mktemp /tmp/cleaner-whistlegraph-remote.XXXXXX)
  find "$downloads/video" -type f -name '*.mp4' -exec basename {} .mp4 \; 2>/dev/null |
    sed -E 's/^whistlegraph-//' | sort -u >"$local_ids"
  jq -r '.posts[].src // empty' "$posts" |
    sed -nE 's#.*/([0-9]+)\.mp4.*#\1#p' | sort -u >"$remote_ids"
  if [[ -s "$local_ids" ]] && comm -23 "$local_ids" "$remote_ids" | grep -q .; then
    rm -f "$local_ids" "$remote_ids"
    return 1
  fi
  sample_url=$(jq -r 'first(.posts[].src // empty)' "$posts")
  rm -f "$local_ids" "$remote_ids"
  [[ "$sample_url" == https://assets.aesthetic.computer/* ]] &&
    curl -fsSIL --max-time 15 "$sample_url" >/dev/null
}

verify_assets_remote() {
  local env_file key value output
  command -v aws >/dev/null || return 1
  [[ -d "$REPO_ROOT/system/public/assets" ]] || return 0
  # Match the media helpers: use existing environment credentials first, then
  # privately load only the Spaces keys needed by aws. Values are never logged.
  env_file="$REPO_ROOT/aesthetic-computer-vault/silo/.env"
  if [[ -f "$env_file" && ( -z "${AWS_ACCESS_KEY_ID:-}" || -z "${AWS_SECRET_ACCESS_KEY:-}" ) ]]; then
    while IFS='=' read -r key value; do
      case "$key" in
        SPACES_KEY|DO_SPACES_KEY)
          value=${value%$'\r'}; value=${value#\"}; value=${value%\"}; value=${value#\'}; value=${value%\'}
          [[ -n "${AWS_ACCESS_KEY_ID:-}" ]] || export AWS_ACCESS_KEY_ID="$value"
          ;;
        SPACES_SECRET|DO_SPACES_SECRET)
          value=${value%$'\r'}; value=${value#\"}; value=${value%\"}; value=${value#\'}; value=${value%\'}
          [[ -n "${AWS_SECRET_ACCESS_KEY:-}" ]] || export AWS_SECRET_ACCESS_KEY="$value"
          ;;
      esac
    done <"$env_file"
  fi
  output=$(aws s3 sync "$REPO_ROOT/system/public/assets" s3://assets-aesthetic-computer \
    --endpoint-url https://sfo3.digitaloceanspaces.com \
    --exclude '*.DS_Store' --exclude 'false.work/spiderlily-*.zip*' \
    --size-only --dryrun 2>&1) || return 1
  [[ -z "$output" ]]
}

verify_pop_remote() {
  local helper="$REPO_ROOT/pop/bin/remote-workspace.mjs"
  [[ -f "$helper" ]] || return 1
  command -v node >/dev/null || return 1
  node "$helper" verify >/dev/null
}

apply_remote_backed_cleanup() {
  [[ -n "$REPO_ROOT" ]] || { skip "AC repository not found; remote-backed surfaces kept"; return; }
  echo "Verifying remote-backed AC media before local pruning..."

  if verify_whistlegraph_remote; then
    clean_ignored \
      toolchain/whistlegraph/downloads/video \
      toolchain/whistlegraph/downloads/site \
      toolchain/whistlegraph/downloads/glyphs \
      toolchain/whistlegraph/downloads/longest
  else
    skip "Whistlegraph CDN/index verification failed; local media kept"
  fi

  if verify_assets_remote; then
    clean_ignored system/public/assets
  else
    skip "assets Spaces dry-run found pending uploads or could not authenticate; mirror kept"
  fi

  if verify_pop_remote; then
    while IFS= read -r out; do
      clean_ignored "${out#"$REPO_ROOT/"}"
    done < <(find "$REPO_ROOT/pop" -mindepth 2 -maxdepth 2 -type d -name out 2>/dev/null)
  else
    skip "Pop private-workspace verification failed; out/ generations kept"
  fi
}

apply_cleanup() {
  local before after reclaimed
  before=$(disk_used_kb)
  echo "Applying safe user-cache cleanup..."

  if pgrep -f '/Google Chrome( |$)|Google Chrome Helper|[p]uppeteer|[p]laywright|[c]ypress' >/dev/null; then
    skip "Chrome/browser automation is active; browser caches kept"
  else
    clean_contents "$HOME/Library/Caches/Google"
    clean_contents "$HOME/Library/Caches/com.google.Chrome"
    clean_contents "$HOME/.cache/puppeteer"
    clean_contents "$HOME/Library/Caches/ms-playwright"
    clean_contents "$HOME/Library/Caches/Cypress"
  fi

  if pgrep -x Spotify >/dev/null; then
    skip "Spotify is active; its cache kept"
  else
    clean_contents "$HOME/Library/Caches/com.spotify.client"
  fi

  if pgrep -x Messages >/dev/null; then
    skip "Messages is active; its cache kept"
  else
    clean_contents "$HOME/Library/Messages/Caches"
  fi

  if pgrep -f '(^|/)(pip|pip3)( |$)' >/dev/null; then
    skip "pip is active; its cache kept"
  else
    clean_contents "$HOME/Library/Caches/pip"
  fi

  if pgrep -f '(^|/)(npm|pnpm|yarn|node-gyp|electron-builder)( |$)' >/dev/null; then
    skip "JS package/build tooling is active; npm/Electron caches kept"
  else
    clean_contents "$HOME/.npm/_cacache"
    clean_contents "$HOME/Library/Caches/electron"
    clean_contents "$HOME/Library/Caches/electron-builder"
    clean_contents "$HOME/.cache/electron"
    clean_contents "$HOME/.cache/node-gyp"
  fi

  if pgrep -f '(^|/)(swift|swiftc|xcodebuild)( |$)' >/dev/null; then
    skip "Swift/Xcode build is active; SwiftPM and .build caches kept"
  else
    clean_contents "$HOME/Library/Caches/org.swift.swiftpm"
    clean_contents "$HOME/.swiftpm/cache"
    [[ -n "$REPO_ROOT" ]] && find "$REPO_ROOT" -name .build -type d -prune -exec rm -rf -- {} + 2>/dev/null || true
  fi

  if pgrep -x Xcode >/dev/null || pgrep -f '(^|/)xcodebuild( |$)' >/dev/null; then
    skip "Xcode is active; DerivedData kept"
  else
    clean_contents "$HOME/Library/Developer/Xcode/DerivedData"
  fi

  if pgrep -x 'Final Cut Pro' >/dev/null || pgrep -x FinalCutPro >/dev/null; then
    skip "Final Cut is active; its caches kept"
  else
    clean_contents "$HOME/Library/Caches/com.apple.FinalCut"
    clean_contents "$HOME/Library/Caches/com.apple.FinalCutTrial"
  fi

  clean_contents "$HOME/.cache/mu"
  if [[ -d "$HOME/.local/share/slab/sessions" ]]; then
    find "$HOME/.local/share/slab/sessions" -maxdepth 1 -type f -mtime +7 -delete
  fi

  if [[ -n "$MESSAGES_CACHE" ]]; then
    thin_messages_cache "$MESSAGES_CACHE"
  fi

  clean_contents "$HOME/.Trash"

  if have mac-cleanup && [[ -f "$MAC_CLEANUP_CONFIG" ]]; then
    echo "Running mac-cleanup-py (curated modules)..."
    # -f accepts its own prompts; the module set in $MAC_CLEANUP_CONFIG is the
    # only thing it touches. Its summary line is the one worth keeping.
    mac-cleanup -f 2>&1 | grep -E -i 'removed|freed|error' | sed 's/^/  /' || true
  fi

  if ! pgrep -x brew >/dev/null; then
    if [[ -x /opt/homebrew/bin/brew ]]; then
      /opt/homebrew/bin/brew cleanup --prune=all >/dev/null 2>&1 || true
    elif [[ -x /usr/local/bin/brew ]]; then
      /usr/local/bin/brew cleanup --prune=all >/dev/null 2>&1 || true
    fi
  fi

  if ((THIN_SNAPSHOTS)); then
    tmutil thinlocalsnapshots / 10000000000 4 >/dev/null 2>&1 || skip "tmutil could not thin snapshots without administrator approval"
  fi

  if ((REMOTE_BACKED)); then
    apply_remote_backed_cleanup
  fi

  after=$(disk_used_kb)
  reclaimed=$((before - after))
  ((reclaimed < 0)) && reclaimed=0
  echo "Cleanup complete: $(awk -v kb="$reclaimed" 'BEGIN {printf "%.1f MiB", kb / 1024}') reclaimed."
  df -h "$(data_volume)" | awk 'NR == 1 || NR == 2'
}

# iMessage attachments are a cache only when Messages in iCloud is on: every
# file here was pulled from the cloud and comes back on view. The chat database
# maps each file to its message date, so the lane keeps a recent window and
# trashes the rest. Messages must be closed; the lane never quits it for you.
thin_messages_cache() {
  local cutoff="$1" db att dest list rel path count=0 bytes=0 size
  db="$HOME/Library/Messages/chat.db"
  att="$HOME/Library/Messages/Attachments"
  dest="$HOME/.Trash/messages-attachments-before-$cutoff"
  if ! messages_icloud_on; then skip "Messages in iCloud is off; attachments are the only copy, kept"; return; fi
  if pgrep -x Messages >/dev/null; then skip "Messages is open; quit it and rerun --messages-cache"; return; fi
  if ! have sqlite3 || [[ ! -r "$db" ]]; then skip "cannot read chat.db (grant Full Disk Access to the terminal); attachments kept"; return; fi
  list=$(mktemp /tmp/cleaner-messages.XXXXXX)
  sqlite3 -readonly "$db" "select replace(a.filename,'~/','$HOME/') from attachment a
    join message_attachment_join j on j.attachment_id = a.ROWID
    join message m on m.ROWID = j.message_id
    where a.filename is not null
      and (m.date / 1000000000 + 978307200) < cast(strftime('%s', '$cutoff') as integer)
    group by a.ROWID;" >"$list" 2>/dev/null || { rm -f "$list"; skip "chat.db query failed; attachments kept"; return; }
  while IFS= read -r path; do
    [[ -f "$path" && "$path" == "$att"/* ]] || continue
    rel=${path#"$att"/}
    size=$(stat -f %z -- "$path" 2>/dev/null || echo 0)
    mkdir -p "$dest/$(dirname "$rel")"
    mv -- "$path" "$dest/$rel" && { count=$((count + 1)); bytes=$((bytes + size)); }
  done <"$list"
  rm -f "$list"
  find "$att" -type d -empty -delete 2>/dev/null || true
  echo "  Messages attachments before $cutoff: $count files, $(human_kb $((bytes / 1024))) moved to the Trash (redownload from iCloud on view)"
}

# Pearcleaner takes the bundle and the leftovers our own lanes never see
# (Application Support, Caches, Preferences, launch agents). Without it the app
# still goes to the Trash so the space comes back on --apply.
uninstall_apps() {
  local app
  for app in "${UNINSTALL[@]}"; do
    [[ "$app" == /* ]] || app="/Applications/$app"
    [[ "$app" == *.app ]] || app="$app.app"
    if [[ ! -d "$app" ]]; then skip "$app is not there"; continue; fi
    if pgrep -f "^$app/Contents/MacOS/" >/dev/null; then skip "$app is running; quit it first"; continue; fi
    if have pearcleaner; then
      echo "Uninstalling $(basename "$app" .app) with Pearcleaner (bundle + leftovers)..."
      pearcleaner uninstall-all "$app" 2>&1 | sed 's/^/  /' || skip "Pearcleaner could not remove $app"
    else
      echo "Pearcleaner missing; moving $(basename "$app") to the Trash (leftovers stay)..."
      if [[ -w "$app" ]]; then mv -- "$app" "$HOME/.Trash/"; else sudo -n mv -- "$app" "$HOME/.Trash/" || skip "$app is root-owned; sudo declined"; fi
    fi
  done
}

# --install also fetches the helpers so every fleet Mac has the same toolset.
# mac-cleanup-py insists on an interactive module picker until a config exists,
# so the curated set is written straight to its TOML file.
ensure_helpers() {
  local brew module
  if [[ -x /opt/homebrew/bin/brew ]]; then brew=/opt/homebrew/bin/brew
  elif [[ -x /usr/local/bin/brew ]]; then brew=/usr/local/bin/brew
  else echo "Homebrew missing; dust, mac-cleanup-py, and Pearcleaner not installed"; return; fi
  have dust || "$brew" install dust >/dev/null 2>&1 || echo "brew install dust failed"
  have mac-cleanup || "$brew" install mac-cleanup-py >/dev/null 2>&1 || echo "brew install mac-cleanup-py failed"
  have pearcleaner || "$brew" install --cask pearcleaner >/dev/null 2>&1 || echo "brew install --cask pearcleaner failed"
  if have mac-cleanup && [[ ! -f "$MAC_CLEANUP_CONFIG" ]]; then
    {
      printf 'enabled = ['
      for module in $MAC_CLEANUP_MODULES; do printf '"%s", ' "$module"; done
      printf ']\n'
    } >"$MAC_CLEANUP_CONFIG"
    echo "Wrote mac-cleanup-py module set to $MAC_CLEANUP_CONFIG"
  fi
  echo "Helpers: dust $(tool_state dust), mac-cleanup $(tool_state mac-cleanup), pearcleaner $(tool_state pearcleaner)"
}

install_utility() {
  local bin_dir agent_dir plist source uid
  bin_dir="$HOME/.local/bin"
  agent_dir="$HOME/Library/LaunchAgents"
  plist="$agent_dir/computer.aesthetic.cleaner.plist"
  source=$(cd "$(dirname "$0")" && pwd)/$(basename "$0")
  uid=$(id -u)
  mkdir -p "$bin_dir" "$agent_dir" "$HOME/Library/Logs"
  if [[ "$source" != "$bin_dir/cleaner" ]]; then
    install -m 0755 "$source" "$bin_dir/cleaner"
  else
    chmod 0755 "$bin_dir/cleaner"
  fi
  ln -sfn cleaner "$bin_dir/ac-disk-clean"

  cat >"$plist" <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><dict>
  <key>Label</key><string>computer.aesthetic.cleaner</string>
  <key>ProgramArguments</key><array>
    <string>$bin_dir/cleaner</string><string>--apply</string>
  </array>
  <key>StartCalendarInterval</key><dict>
    <key>Weekday</key><integer>1</integer><key>Hour</key><integer>3</integer><key>Minute</key><integer>15</integer>
  </dict>
  <key>StandardOutPath</key><string>$HOME/Library/Logs/cleaner.log</string>
  <key>StandardErrorPath</key><string>$HOME/Library/Logs/cleaner.log</string>
</dict></plist>
EOF

  plutil -lint "$plist"
  launchctl bootout "gui/$uid/computer.aesthetic.cleaner" >/dev/null 2>&1 || true
  launchctl bootstrap "gui/$uid" "$plist"
  echo "Installed $bin_dir/cleaner (compatibility alias: ac-disk-clean) and weekly LaunchAgent."
  ensure_helpers
}

if ((INSTALL)); then
  install_utility
  exit 0
fi

if ((${#UNINSTALL[@]})); then
  uninstall_apps
  ((APPLY)) || exit 0
fi

report
if ((APPLY)); then
  echo
  apply_cleanup
fi
