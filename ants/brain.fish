#!/usr/bin/env fish
# 🧠 Ant Brain — middleware for LLM calls
# Abstracts the LLM provider so ants can use any backend.
#
# Usage: fish brain.fish --provider <provider> --model <model> --system <text> --prompt <text>
#
# Providers:
#   gh-models     — GitHub Models via `gh models run` (free with Copilot)
#   claude-code   — Claude Code CLI via `claude --print` (needs auth)
#   openai        — OpenAI API via curl (needs OPENAI_API_KEY)
#   ollama        — Local Ollama via curl (needs ollama running)
#   custom        — Custom curl endpoint (needs ANT_API_URL, ANT_API_KEY)
#
# Output: prints the LLM response to stdout. Exit code 0 = success, 1 = error.
#
# The brain does NOT have tools/agency. It receives context and returns text.
# The colony script is responsible for acting on the response.

set -g BRAIN_DIR (realpath (dirname (status filename)))

# Defaults
set -g PROVIDER "gh-models"
set -g MODEL ""
set -g SYSTEM_PROMPT ""
set -g USER_PROMPT ""
set -g MAX_TOKENS 4096

# Parse args
set -l i 1
while test $i -le (count $argv)
    switch $argv[$i]
        case --provider
            set i (math $i + 1)
            set PROVIDER $argv[$i]
        case --model
            set i (math $i + 1)
            set MODEL $argv[$i]
        case --system
            set i (math $i + 1)
            set SYSTEM_PROMPT $argv[$i]
        case --prompt
            set i (math $i + 1)
            set USER_PROMPT $argv[$i]
        case --max-tokens
            set i (math $i + 1)
            set MAX_TOKENS $argv[$i]
    end
    set i (math $i + 1)
end

# Default models per provider
if test -z "$MODEL"
    switch $PROVIDER
        case gh-models
            set MODEL "openai/gpt-4o-mini"
        case claude-code
            set MODEL "sonnet"
        case openai
            set MODEL "gpt-4o-mini"
        case ollama
            set MODEL "llama3.2"
        case custom
            set MODEL "default"
    end
end

function brain_gh_models
    # GitHub Models via `gh models run`
    # System prompt goes via --system-prompt, user prompt is the positional arg
    if test -n "$SYSTEM_PROMPT"
        gh models run $MODEL "$USER_PROMPT" \
            --system-prompt "$SYSTEM_PROMPT" \
            --max-tokens "$MAX_TOKENS" 2>&1
    else
        gh models run $MODEL "$USER_PROMPT" \
            --max-tokens "$MAX_TOKENS" 2>&1
    end
    return $status
end

function brain_claude_code
    # Claude Code in headless mode (agentic — has tools)
    set -l _saved_key "$ANTHROPIC_API_KEY"
    set -e ANTHROPIC_API_KEY

    if test -n "$SYSTEM_PROMPT"
        claude --print \
            --model $MODEL \
            --system-prompt "$SYSTEM_PROMPT" \
            --dangerously-skip-permissions \
            --allowedTools "Bash,Read,Edit,Write" \
            --max-budget-usd 0.10 \
            --no-session-persistence \
            "$USER_PROMPT" 2>&1
    else
        claude --print \
            --model $MODEL \
            --dangerously-skip-permissions \
            --allowedTools "Bash,Read,Edit,Write" \
            --max-budget-usd 0.10 \
            --no-session-persistence \
            "$USER_PROMPT" 2>&1
    end

    set -l result $status

    if test -n "$_saved_key"
        set -gx ANTHROPIC_API_KEY $_saved_key
    end

    return $result
end

function brain_openai
    # OpenAI API via curl
    if test -z "$OPENAI_API_KEY"
        echo "ERROR: OPENAI_API_KEY not set" >&2
        return 1
    end

    set -l messages "[]"
    if test -n "$SYSTEM_PROMPT"
        set messages (printf '[{"role":"system","content":"%s"},{"role":"user","content":"%s"}]' \
            (echo $SYSTEM_PROMPT | sed 's/"/\\"/g; s/\n/\\n/g') \
            (echo $USER_PROMPT | sed 's/"/\\"/g; s/\n/\\n/g'))
    else
        set messages (printf '[{"role":"user","content":"%s"}]' \
            (echo $USER_PROMPT | sed 's/"/\\"/g; s/\n/\\n/g'))
    end

    set -l response (curl -s https://api.openai.com/v1/chat/completions \
        -H "Content-Type: application/json" \
        -H "Authorization: Bearer $OPENAI_API_KEY" \
        -d "{\"model\":\"$MODEL\",\"messages\":$messages,\"max_tokens\":$MAX_TOKENS}" 2>&1)

    # Extract content from response
    echo $response | python3 -c "import sys,json; r=json.load(sys.stdin); print(r['choices'][0]['message']['content'])" 2>/dev/null
    return $status
end

function brain_ollama
    # Local Ollama via curl
    set -l ollama_url (test -n "$OLLAMA_URL" && echo $OLLAMA_URL || echo "http://localhost:11434")

    set -l messages "[]"
    if test -n "$SYSTEM_PROMPT"
        set messages (printf '[{"role":"system","content":"%s"},{"role":"user","content":"%s"}]' \
            (echo $SYSTEM_PROMPT | sed 's/"/\\"/g; s/\n/\\n/g') \
            (echo $USER_PROMPT | sed 's/"/\\"/g; s/\n/\\n/g'))
    else
        set messages (printf '[{"role":"user","content":"%s"}]' \
            (echo $USER_PROMPT | sed 's/"/\\"/g; s/\n/\\n/g'))
    end

    set -l response (curl -s "$ollama_url/api/chat" \
        -d "{\"model\":\"$MODEL\",\"messages\":$messages,\"stream\":false}" 2>&1)

    echo $response | python3 -c "import sys,json; r=json.load(sys.stdin); print(r['message']['content'])" 2>/dev/null
    return $status
end

function brain_custom
    # Custom API endpoint (OpenAI-compatible)
    if test -z "$ANT_API_URL"
        echo "ERROR: ANT_API_URL not set" >&2
        return 1
    end

    set -l messages "[]"
    if test -n "$SYSTEM_PROMPT"
        set messages (printf '[{"role":"system","content":"%s"},{"role":"user","content":"%s"}]' \
            (echo $SYSTEM_PROMPT | sed 's/"/\\"/g; s/\n/\\n/g') \
            (echo $USER_PROMPT | sed 's/"/\\"/g; s/\n/\\n/g'))
    else
        set messages (printf '[{"role":"user","content":"%s"}]' \
            (echo $USER_PROMPT | sed 's/"/\\"/g; s/\n/\\n/g'))
    end

    set -l headers "-H 'Content-Type: application/json'"
    if test -n "$ANT_API_KEY"
        set headers "$headers -H 'Authorization: Bearer $ANT_API_KEY'"
    end

    set -l response (curl -s "$ANT_API_URL" \
        -H "Content-Type: application/json" \
        -H "Authorization: Bearer $ANT_API_KEY" \
        -d "{\"model\":\"$MODEL\",\"messages\":$messages,\"max_tokens\":$MAX_TOKENS}" 2>&1)

    echo $response | python3 -c "import sys,json; r=json.load(sys.stdin); print(r['choices'][0]['message']['content'])" 2>/dev/null
    return $status
end

# --- Dispatch ---

switch $PROVIDER
    case gh-models
        brain_gh_models
    case claude-code
        brain_claude_code
    case openai
        brain_openai
    case ollama
        brain_ollama
    case custom
        brain_custom
    case '*'
        echo "ERROR: Unknown provider '$PROVIDER'" >&2
        exit 1
end

exit $status
