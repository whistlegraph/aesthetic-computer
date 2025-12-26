function devup --description "Start aesthetic devcontainer and attach VS Code"
    set -l workspace ~/Desktop/code/aesthetic-computer
    
    echo "🧹 Removing old container..."
    docker rm -f aesthetic 2>/dev/null
    
    echo "🚀 Starting devcontainer..."
    cd $workspace
    devcontainer up --workspace-folder .
    
    if test $status -eq 0
        echo "✅ Container ready!"
        echo "🔗 Opening VS Code attached to container..."
        code --folder-uri "vscode-remote://attached-container+"(printf aesthetic | xxd -p)"/workspaces/aesthetic-computer"
    else
        echo "❌ Failed to start container"
        return 1
    end
end
