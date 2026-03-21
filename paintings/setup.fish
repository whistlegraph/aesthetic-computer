#!/usr/bin/env fish

# Quick setup script for painting tools
# Run: ./setup.fish

echo "🎨 Setting up painting tools..."
echo ""

# Check if .env exists
if not test -f .env
    echo "📝 Creating .env from template..."
    cp .env.template .env
    echo "⚠️  Please edit .env and add your credentials"
    echo ""
end

# Install dependencies
echo "📦 Installing dependencies..."
npm install
echo ""

echo "✅ Setup complete!"
echo ""
echo "🔍 Quick start commands:"
echo ""
echo "  npm run inspect:mongodb -- --stats"
echo "  npm run inspect:spaces -- --list"
echo "  npm run inspect:api -- --tv"
echo ""
echo "💡 See README.md for full documentation"
echo ""
