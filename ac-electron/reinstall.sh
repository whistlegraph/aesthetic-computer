#!/bin/bash
# Quick rebuild and reinstall of Aesthetic Computer app

set -e

cd "$(dirname "$0")"

echo "🔄 Closing running instances..."
pkill -f "Aesthetic Computer" 2>/dev/null || true
sleep 0.5

echo "📦 Building macOS app..."
npm run build:mac

echo "🗑️  Removing old installation..."
rm -rf "/Applications/Aesthetic Computer.app"

echo "📲 Installing to Applications..."
cp -R "dist/mac-universal/Aesthetic Computer.app" /Applications/

echo "🚀 Launching app..."
open "/Applications/Aesthetic Computer.app"

echo "✅ Done!"
