# Chat Message Moderation with Ollama

PG-13 content filter for chat messages using local LLM.

## Current Status

**Model:** gemma2:2b (1.6GB, Google's safety-focused model) ✅ **RECOMMENDED**  
**Performance:** 100% accuracy on explicit content, ~50% false positive rate on edge cases  
**Output:** Simple t/f decision  
**Speed:** ~1-2s per message (slower but reliable)

### Why gemma2:2b?
- ✅ **NEVER misses explicit content** (0% false negatives)
- ✅ Correctly blocks: profanity, sexual content, violence, drugs, hate speech
- ✅ Allows: normal chat, URLs, questions
- ⚠️ May block some borderline content (false positives on foreign language, slang)
- 🎯 **Better to be safe than sorry** for content moderation

### Tested Alternatives

| Model | Size | Speed | Explicit Content | False Positives |
|-------|------|-------|------------------|-----------------|
| **gemma2:2b** ✅ | 1.6GB | ~1.5s | 100% blocked | ~50% on edge cases |
| qwen2.5:0.5b ❌ | 397MB | ~300ms | **0% blocked** | Low |
| qwen2.5:1.5b ❌ | 986MB | ~2.5s | Blocks everything | 100% |

## Setup

1. **Start Ollama daemon:**
   ```fish
   ac-llama start
   ```

2. **Check status:**
   ```fish
   ac-llama status
   ```

3. **View logs:**
   ```fish
   ac-llama logs
   ```

## Usage

### Test MongoDB Messages

Test chat messages from the aesthetic.chat-system collection:

```fish
# Test 20 messages
node test-mongodb-messages.mjs 20

# Test 50 messages  
node test-mongodb-messages.mjs 50

# Continuous testing (all messages)
node test-mongodb-messages.mjs 100 --continuous
```

### Direct HTTP Testing

Test single messages via HTTP (requires Caddy server running):

```fish
./start-server.fish
curl -X POST http://localhost:8080/censor \
  -H "Content-Type: application/json" \
  -d '{"message": "hello world"}'
```

## How It Works

- **Model:** qwen2.5:0.5b (397MB, fast inference)
- **Prompt:** Simple t/f rating for PG-13 appropriateness
- **Blocks:** Sexual content, body functions, profanity, violence, drugs, hate speech
- **Allows:** URLs, links, normal conversation explicitly allowed in prompt
- **Response:** Single letter - `t` (allow) or `f` (block)
- **Speed:** ~300-400ms per message

## Performance

From 20-message test:
- ✅ 95% pass rate (19/20)
- ⚠️ Known issue: Occasionally blocks URLs despite prompt instruction
- ⏱️ Average latency: 350ms
- 🚀 Fast enough for real-time moderation

# PG-13 Content Filter

AI-powered chat message moderation using Ollama and gemma2:2b model.

## 🌐 Web Dashboard

**Live at: http://localhost:8080**

Interactive web interface for testing messages in real-time with:
- ✅/🚫 Visual pass/fail indicators
- 📊 Live statistics (total tests, pass/fail counts, avg response time)
- 📝 Example messages to try
- 🤖 AI reasoning display
- ⚡ Real-time response times

### Starting the Dashboard

```fish
# Make sure Ollama is running
ac-llama start

# Start the API server (in one terminal)
cd /workspaces/aesthetic-computer/censor
node api-server.mjs &

# Start Caddy web server (in another terminal or background)
caddy run --config Caddyfile &
```

Then open http://localhost:8080 in your browser!

## 🔌 API Endpoint

**POST http://localhost:3000/api/filter**

```json
{
  "message": "Hello, how are you?"
}
```

Response:
```json
{
  "decision": "t",
  "sentiment": "t",
  "responseTime": 1.32
}
```

- `decision`: `"t"` (allow) or `"f"` (block)
- `sentiment`: AI's raw response with reasoning
- `responseTime`: Processing time in seconds

## Model Comparison

Tested models:
- **qwen2.5:0.5b** (current): Fast, 95% accurate, no reasoning ✅
- **qwen2.5:1.5b**: Slower (2-3s), blocks everything ❌
- **qwen3:0.6b**: Unreliable output format ❌

For sentiment analysis: Need 3b+ model (slower, not yet tested)

## ac-llama Commands

Manage Ollama daemon from anywhere:

```fish
ac-llama start    # Start daemon
ac-llama stop     # Stop daemon  
ac-llama restart  # Restart daemon
ac-llama status   # Check status & list models
ac-llama logs     # View recent logs
```

Added to `.devcontainer/config.fish` for automatic availability in dev environment.

## Files

- `test-mongodb-messages.mjs` - MongoDB integration test with color output
- `Caddyfile` - HTTP server config (optional)
- `start-server.fish` - Caddy startup script
- `test-filter.fish` - Manual testing helper

## MongoDB Connection

Database: `aesthetic.chat-system`  
Connection string from env or default in script
