#!/usr/bin/env fish
# ac-piece-logs — Inspect per-piece runtime telemetry (client console capture)
# stored in MongoDB `piece-runs` by /api/piece-log.
#
# Auth: SSHes to lith (same SSH key as lith/deploy.fish) and runs
# system/backend/piece-logs-cli.mjs with the deployed env loaded.
# The piece-logs CLI ships with lith on every deploy.

set -l LITH_HOST lith.aesthetic.computer
set -l LITH_USER root
set -l REMOTE_DIR /opt/ac

# Find the vault SSH key relative to wherever aesthetic-computer lives.
# Matches the logic in lith/deploy.fish.
function _ac_piece_logs_ssh_key
    for candidate in \
        "$HOME/aesthetic-computer-vault/home/.ssh/id_rsa" \
        "/workspaces/aesthetic-computer-vault/home/.ssh/id_rsa" \
        "$AESTHETIC_VAULT/home/.ssh/id_rsa"
        if test -n "$candidate" -a -f "$candidate"
            echo $candidate
            return 0
        end
    end
    return 1
end

function _ac_piece_logs_run --no-scope-shadowing
    set -l key (_ac_piece_logs_ssh_key)
    if test -z "$key"
        echo "❌ No vault SSH key found. Set AESTHETIC_VAULT or clone aesthetic-computer-vault beside this repo." >&2
        return 1
    end
    # Source the deployed env so MONGODB_CONNECTION_STRING / MONGODB_NAME
    # are available to the CLI, then run it with whatever args came in.
    ssh -i $key $LITH_USER@$LITH_HOST \
        "set -a; source $REMOTE_DIR/system/.env; cd $REMOTE_DIR && node system/backend/piece-logs-cli.mjs $argv"
end

function ac-piece-logs --description "Recent piece-runs (default 20). Pass a slug to filter: ac-piece-logs notepat"
    if test (count $argv) -gt 0; and not string match -q -- '--*' $argv[1]
        _ac_piece_logs_run --slug $argv[1] $argv[2..-1]
    else
        _ac_piece_logs_run $argv
    end
end

function ac-piece-logs-events --description "Recent piece-runs with captured console events. ac-piece-logs-events notepat"
    if test (count $argv) -gt 0; and not string match -q -- '--*' $argv[1]
        _ac_piece_logs_run --events --slug $argv[1] $argv[2..-1]
    else
        _ac_piece_logs_run --events $argv
    end
end

function ac-piece-logs-errors --description "Piece-runs with status=error (default last 60m, 10 results)"
    _ac_piece_logs_run --errors-only --since 60 --limit 10 $argv
end

function ac-piece-logs-grep --description "Search console-event text across recent piece-runs. ac-piece-logs-grep 'drumMode'"
    if test (count $argv) -eq 0
        echo "Usage: ac-piece-logs-grep <regex> [extra flags...]"
        return 1
    end
    _ac_piece_logs_run --grep $argv[1] $argv[2..-1]
end

function ac-piece-logs-json --description "Raw JSON output of recent piece-runs"
    _ac_piece_logs_run --json $argv
end

function ac-piece-logs-help --description "Show piece-logs command help"
    echo "piece-logs — client-side console telemetry stored in MongoDB piece-runs"
    echo ""
    echo "Commands:"
    echo "  ac-piece-logs [slug]         — recent runs (optionally filtered by slug)"
    echo "  ac-piece-logs-events [slug]  — recent runs with captured console output"
    echo "  ac-piece-logs-errors         — runs with status=error (last 60m)"
    echo "  ac-piece-logs-grep <regex>   — runs whose events match regex"
    echo "  ac-piece-logs-json           — raw JSON for scripting"
    echo ""
    echo "Pass-through flags: --slug, --host, --status, --since, --limit, --events, --json"
    echo ""
    echo "Examples:"
    echo "  ac-piece-logs notepat --limit 5"
    echo "  ac-piece-logs-events notepat --since 30"
    echo "  ac-piece-logs-errors"
    echo "  ac-piece-logs-grep 'Invalid note'"
end
