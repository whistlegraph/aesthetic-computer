#!/usr/bin/env fish

# Test the censor filter endpoint
set message $argv[1]

if test -z "$message"
    echo "Usage: ./test-filter.fish \"message to test\""
    exit 1
end

set prompt "You are a moderator determining whether user conduct is appropriate. We are trying to make a child-friendly program, and want to make sure that users are not passing past our moderation requirements, including bullying, sexual content, swear words, excluding vulnerable groups, or trolling. I'm going to give you a user prompt, and I need you to reply \"pass\" if the content provided in the user's message is appropriate, \"fail\" if the content is inappropriate. Please reply with a json that has a rationale, as well as a \"pass\" or \"fail\" value based on the rationale.

For example:
input: \"you are nice\".
output: {\"rationale\": \"it's a simple, sweet message about the other person\", \"outcome\": \"pass\"}

Example:
input: \"let's do a sex\".
output: {\"rationale\": \"this is explicitly sexual content and is not child-friendly\", \"outcome\": \"fail\"}

Input:
$message"

curl -X POST http://localhost:8080/censor \
    -H "Content-Type: application/json" \
    -d (printf '{"model":"qwen3:0.6b","prompt":%s,"stream":false,"format":"json"}' (echo $prompt | jq -Rs .)) | jq -r '.response'
