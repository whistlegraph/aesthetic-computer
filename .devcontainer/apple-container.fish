#!/usr/bin/env fish
# apple-container.fish
# Helper script to build, run, and manage Apple Container for aesthetic-computer
# Usage: ./apple-container.fish [build|run|start|stop|exec|delete|status]

set -l script_dir (dirname (status filename))
set -l containerfile "$script_dir/Containerfile.apple"
set -l container_name "ac-dev"
set -l image_name "ac-dev"

# Detect workspace path on macOS
set -l workspace_path ""
if test -d ~/Desktop/code/aesthetic-computer
    set workspace_path ~/Desktop/code/aesthetic-computer
else if test -d ~/aesthetic-computer
    set workspace_path ~/aesthetic-computer
else if test -d /workspaces/aesthetic-computer
    set workspace_path /workspaces/aesthetic-computer
else
    echo "❌ Could not find aesthetic-computer workspace"
    echo "   Looked in: ~/Desktop/code/aesthetic-computer, ~/aesthetic-computer, /workspaces/aesthetic-computer"
    exit 1
end

function show_help
    echo "🍎 Apple Container Helper for Aesthetic Computer"
    echo ""
    echo "Usage: ./apple-container.fish <command>"
    echo ""
    echo "Commands:"
    echo "  build     Build the container image from Containerfile.apple"
    echo "  run       Create and start a new container (removes existing)"
    echo "  start     Start an existing stopped container"
    echo "  stop      Stop the running container"
    echo "  exec      Execute fish shell in the running container"
    echo "  delete    Delete the container"
    echo "  status    Show container status"
    echo "  logs      Show container logs"
    echo "  generate  Regenerate Containerfile.apple from Dockerfile"
    echo ""
    echo "Workspace: $workspace_path"
end

function cmd_build
    echo "🔨 Building Apple Container image..."
    container build -t $image_name -f $containerfile $script_dir
end

function cmd_run
    echo "🚀 Creating and starting container..."
    
    # Stop and remove existing container if it exists
    if container ls -a | grep -q $container_name
        echo "   Removing existing container..."
        container stop $container_name 2>/dev/null
        container rm $container_name 2>/dev/null
    end
    
    container create --name $container_name -it --ssh \
        -v $workspace_path:/workspaces/aesthetic-computer \
        -p 8888:8888 -p 8889:8889 -p 8111:8111 -p 3000:3000 \
        -p 8083:8083 -p 8084:8084 -p 8085:8085 \
        -w /workspaces/aesthetic-computer \
        --cpus 4 --memory 8G \
        $image_name /usr/bin/fish
    
    container start $container_name
    echo "✅ Container started! Run './apple-container.fish exec' to enter."
end

function cmd_start
    echo "▶️  Starting container..."
    container start $container_name
end

function cmd_stop
    echo "⏹️  Stopping container..."
    container stop $container_name
end

function cmd_exec
    echo "🐚 Entering container shell..."
    container exec -it $container_name /usr/bin/fish
end

function cmd_delete
    echo "🗑️  Deleting container..."
    container stop $container_name 2>/dev/null
    container rm $container_name
end

function cmd_status
    echo "📊 Container status:"
    container ls -a | head -1
    container ls -a | grep $container_name; or echo "   No container named '$container_name' found"
    echo ""
    if container ls | grep -q $container_name
        echo "📈 Resource usage:"
        container stats $container_name --no-stream
    end
end

function cmd_logs
    container logs $container_name
end

function cmd_generate
    fish $script_dir/generate-apple-container.fish
end

# Main
switch $argv[1]
    case build
        cmd_build
    case run
        cmd_run
    case start
        cmd_start
    case stop
        cmd_stop
    case exec
        cmd_exec
    case delete
        cmd_delete
    case status
        cmd_status
    case logs
        cmd_logs
    case generate
        cmd_generate
    case help -h --help ""
        show_help
    case "*"
        echo "❌ Unknown command: $argv[1]"
        show_help
        exit 1
end
