#!/usr/bin/env fish
# 🐜 Aesthetic Ant Colony
# A dumb ant wakes every N minutes, reads the score, does one small thing.
#
# Usage: fish ants/colony.fish [--once] [--interval MINUTES] [--provider PROVIDER] [--model MODEL]
#
# Options:
#   --once              Run one ant and exit (for testing)
#   --interval N        Minutes between runs (default: 30)
#   --provider PROVIDER LLM provider (default: gh-models). See brain.fish.
#   --model MODEL       Model name (default: per provider)

set -g COLONY_DIR (realpath (dirname (status filename)))
set -g REPO_DIR (realpath "$COLONY_DIR/..")
set -g SCORE_FILE "$COLONY_DIR/score.md"
set -g LOG_FILE "$COLONY_DIR/colony.log"
set -g PHEROMONE_FILE "$COLONY_DIR/pheromones.log"
set -g BRAIN "$COLONY_DIR/brain.fish"
set -g INTERVAL 30
set -g PROVIDER "gh-models"
set -g MODEL ""
set -g ONCE false

# Parse args
set -l i 1
while test $i -le (count $argv)
    switch $argv[$i]
        case --once
            set ONCE true
        case --interval
            set i (math $i + 1)
            set INTERVAL $argv[$i]
        case --provider
            set i (math $i + 1)
            set PROVIDER $argv[$i]
        case --model
            set i (math $i + 1)
            set MODEL $argv[$i]
    end
    set i (math $i + 1)
end

function log_msg
    set -l msg (date "+%Y-%m-%d %H:%M:%S")" 🐜 $argv"
    echo $msg
    echo $msg >> $LOG_FILE
end

function log_pheromone
    set -l msg (date "+%Y-%m-%d %H:%M:%S")" $argv"
    echo $msg >> $PHEROMONE_FILE
end

function call_brain --argument-names system_prompt user_prompt
    set -l args --provider $PROVIDER
    if test -n "$MODEL"
        set args $args --model $MODEL
    end
    fish $BRAIN $args --system "$system_prompt" --prompt "$user_prompt" 2>&1
end

function run_ant
    set -l run_id (date "+%Y%m%d-%H%M%S")
    log_msg "Ant $run_id waking up..."

    cd $REPO_DIR

    # Check for dirty tracked files (queen might be working)
    set -l dirty (git diff --name-only HEAD 2>/dev/null | head -1)
    set -l staged (git diff --cached --name-only 2>/dev/null | head -1)
    if test -n "$dirty" -o -n "$staged"
        log_msg "Tracked files modified/staged — queen is working. Sleeping."
        log_pheromone "IDLE: ant $run_id — dirty tree, deferred to queen"
        return 1
    end

    # Read the score
    if not test -f $SCORE_FILE
        log_msg "ERROR: Score not found at $SCORE_FILE"
        return 1
    end
    set -l score_content (cat $SCORE_FILE)

    # Read recent pheromones
    set -l recent_pheromones "(none yet)"
    if test -f $PHEROMONE_FILE
        set -l trail (tail -20 $PHEROMONE_FILE)
        if test -n "$trail"
            set recent_pheromones (string join \n $trail)
        end
    end

    # Gather context
    set -l test_output (cd $REPO_DIR; npm test 2>&1 | tail -30)
    set -l recent_commits (cd $REPO_DIR; git log --oneline -10 2>/dev/null)

    # --- Phase 1: SCOUT — pick a task and target file ---
    log_msg "Phase 1: Scouting..."

    set -l scout_system "You are a dumb but careful ant. Respond with exactly one line starting with SCOUT:"
    set -l scout_prompt "You are aesthetic ant $run_id. You follow the score.

## The Score
$score_content

## Recent Pheromones (what other ants did)
$recent_pheromones

## Current Test Output (last 30 lines)
$test_output

## Recent Git History
$recent_commits

---

Pick ONE small task from the Current Tasks in the score.
Based on the test output and context, decide what specific file to look at.

Respond with EXACTLY one line in this format:
SCOUT: <task> | <file path relative to repo root> | <plan in one sentence>

If nothing to do:
SCOUT: IDLE | none | <reason>

Respond with ONLY the SCOUT line. Nothing else."

    set -l scout_output (call_brain "$scout_system" "$scout_prompt")

    mkdir -p "$COLONY_DIR/runs"
    echo "=== SCOUT ===" > "$COLONY_DIR/runs/$run_id.log"
    echo "$scout_output" >> "$COLONY_DIR/runs/$run_id.log"

    set -l scout_line (echo "$scout_output" | grep "^SCOUT:" | tail -1)
    if test -z "$scout_line"
        log_msg "Scout returned no SCOUT line. Raw output saved to runs/$run_id.log"
        log_pheromone "ERROR: ant $run_id — scout failed (no SCOUT line)"
        return 1
    end

    log_msg "Scout: $scout_line"

    if string match -q "*IDLE*" "$scout_line"
        set -l reason (echo $scout_line | sed 's/.*| *//')
        log_msg "Nothing to do: $reason"
        log_pheromone "IDLE: ant $run_id — $reason"
        return 0
    end

    # Parse: SCOUT: task | path | plan
    set -l parts (string split "|" (string replace "SCOUT:" "" "$scout_line"))
    set -l task_name (string trim $parts[1])
    set -l target_file (string trim $parts[2])
    set -l plan (string trim $parts[3])

    if test -z "$target_file" -o "$target_file" = "none"
        log_msg "No target file."
        log_pheromone "IDLE: ant $run_id — no target"
        return 0
    end

    log_msg "Target: $target_file"
    log_msg "Plan: $plan"

    # Resolve the file
    set -l full_path "$REPO_DIR/$target_file"
    if not test -f "$full_path"
        log_msg "File not found: $full_path"
        log_pheromone "FAILURE: ant $run_id — file not found: $target_file"
        return 1
    end

    # --- Phase 2: WORK — read the file and produce a diff ---
    log_msg "Phase 2: Working on $target_file..."

    set -l file_content (head -200 "$full_path")
    set -l line_count (wc -l < "$full_path" | string trim)

    set -l work_system "You output precise unified diffs. No preamble, no explanation outside the required format."
    set -l work_prompt "You are aesthetic ant $run_id editing: $target_file ($line_count lines, showing first 200)
Plan: $plan

## File Content
$file_content

## Test Output
$test_output

## Rules
- Make the SMALLEST change that accomplishes your plan.
- You must be 98% confident your change is correct.
- Do NOT change anything unrelated to your plan.
- Include 3 lines of context before and after each hunk.

## Response Format
If you have a change, respond:
WORK: CHANGE | <one-line description>
\`\`\`diff
--- a/$target_file
+++ b/$target_file
@@ <hunk header> @@
 context
-old line
+new line
 context
\`\`\`
WORK_END

If not confident enough:
WORK: ABORT | <reason>"

    set -l work_output (call_brain "$work_system" "$work_prompt")

    echo "" >> "$COLONY_DIR/runs/$run_id.log"
    echo "=== WORK ===" >> "$COLONY_DIR/runs/$run_id.log"
    echo "$work_output" >> "$COLONY_DIR/runs/$run_id.log"

    if string match -q "*ABORT*" "$work_output"
        set -l reason (echo "$work_output" | grep "ABORT" | sed 's/.*ABORT *| *//')
        log_msg "Aborted: $reason"
        log_pheromone "IDLE: ant $run_id — aborted: $reason"
        return 0
    end

    # Extract the diff block
    set -l diff_content (echo "$work_output" | sed -n '/^```diff/,/^```/{/^```/d;p}')
    if test -z "$diff_content"
        # Try without fenced code block — raw diff
        set diff_content (echo "$work_output" | sed -n '/^--- a\//,/WORK_END/{/WORK_END/d;p}')
    end

    if test -z "$diff_content"
        log_msg "No valid diff produced."
        log_pheromone "FAILURE: ant $run_id — no valid diff"
        return 1
    end

    set -l description (echo "$work_output" | grep "^WORK: CHANGE" | sed 's/WORK: CHANGE *| *//')
    if test -z "$description"
        set description "$plan"
    end

    # --- Phase 3: APPLY & VERIFY ---
    log_msg "Phase 3: Applying..."

    set -l diff_file (mktemp /tmp/ant-XXXXXX.patch)
    printf '%s\n' $diff_content > $diff_file

    cd $REPO_DIR

    # Dry run
    git apply --check $diff_file 2>/dev/null
    if test $status -ne 0
        log_msg "Diff doesn't apply cleanly."
        echo "" >> "$COLONY_DIR/runs/$run_id.log"
        echo "=== FAILED DIFF ===" >> "$COLONY_DIR/runs/$run_id.log"
        cat $diff_file >> "$COLONY_DIR/runs/$run_id.log"
        rm -f $diff_file
        log_pheromone "FAILURE: ant $run_id — diff didn't apply cleanly"
        return 1
    end

    # Apply for real
    git apply $diff_file 2>&1
    set -l apply_exit $status
    rm -f $diff_file

    if test $apply_exit -ne 0
        log_msg "Apply failed."
        git checkout . 2>/dev/null
        log_pheromone "FAILURE: ant $run_id — apply failed"
        return 1
    end

    # Check we actually changed something
    set -l changes (git diff --name-only 2>/dev/null)
    if test -z "$changes"
        log_msg "No changes after apply (diff was a no-op)."
        log_pheromone "IDLE: ant $run_id — no-op diff"
        return 0
    end

    # Verify tests pass
    log_msg "Verifying tests..."
    set -l verify_output (npm test 2>&1)
    set -l verify_exit $status

    echo "" >> "$COLONY_DIR/runs/$run_id.log"
    echo "=== TEST VERIFY ===" >> "$COLONY_DIR/runs/$run_id.log"
    echo "Exit: $verify_exit" >> "$COLONY_DIR/runs/$run_id.log"
    echo "$verify_output" | tail -10 >> "$COLONY_DIR/runs/$run_id.log"

    if test $verify_exit -ne 0
        log_msg "Tests FAILED after apply. Reverting."
        git checkout . 2>/dev/null
        log_pheromone "REVERTED: ant $run_id — tests failed after change"
        return 1
    end

    log_msg "Tests pass. Changed: $changes"

    # Commit
    git add -A
    git commit -m "ant: $description

Ant-ID: $run_id
Provider: $PROVIDER
Model: $MODEL
Verified: tests pass" --no-verify 2>&1

    if test $status -eq 0
        log_msg "Committed! 🐜✅"
        log_pheromone "SUCCESS: ant $run_id ($PROVIDER/$MODEL) — $description"
    else
        log_msg "Commit failed. Reverting."
        git checkout . 2>/dev/null
        git reset HEAD . 2>/dev/null
        log_pheromone "ERROR: ant $run_id — commit failed"
        return 1
    end

    return 0
end

# --- Main Loop ---

log_msg "Colony starting. Interval: "$INTERVAL"m | Provider: $PROVIDER | Model: $MODEL | Once: $ONCE"
log_msg "Score: $SCORE_FILE"
log_msg "Repo: $REPO_DIR"

if test "$ONCE" = true
    run_ant
    set -l result $status
    log_msg "Single run complete. Exit: $result"
    exit $result
end

while true
    run_ant
    log_msg "Sleeping for $INTERVAL minutes..."
    sleep (math "$INTERVAL * 60")
end
