#!/usr/bin/env fish

# Create and use the custom moderation model

echo "🔨 Creating custom moderation model..."

ollama create censor -f /workspaces/aesthetic-computer/censor/Modelfile

echo ""
echo "✅ Model 'censor' created successfully!"
echo ""
echo "To use it, update test-mongodb-messages.mjs to use model: 'censor' instead of 'qwen3:0.6b'"
