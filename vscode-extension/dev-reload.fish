#!/usr/bin/env fish
# Development reload script for VSCode extension in devcontainer

echo "🔨 Compiling extension..."
npm run compile

if test $status -ne 0
    echo "❌ Compilation failed!"
    exit 1
end

echo "📦 Packaging extension..."
npx vsce package

if test $status -ne 0
    echo "❌ Packaging failed!"
    exit 1
end

echo "🔄 Installing extension..."
/vscode/vscode-server/bin/linux-x64/*/bin/remote-cli/code --install-extension (ls -t aesthetic-computer-code-*.vsix | head -n1)

if test $status -ne 0
    echo "❌ Installation failed!"
    exit 1
end

echo "✅ Extension updated! Now reload the VSCode window:"
echo "   Press: Cmd/Ctrl + Shift + P → 'Developer: Reload Window'"
