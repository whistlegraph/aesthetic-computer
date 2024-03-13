#!/usr/bin/env fish

function rebuild_aesthetic
    # Stop and remove containers and networks created by 'docker-compose up'
    docker-compose down
    # Remove any containers created by 'docker-compose run'
    docker rm -f $(docker ps -aq --filter "name=aesthetic-run")
    # Remove the specific Docker image for the aesthetic service
    docker rmi aesthetic-micro -f
    # Rebuild and run the service
    docker-compose run --build aesthetic

    # Check the exit code
    set exit_code $status

    # If the exit code is 70, call the function again
    if test $exit_code -eq 70
        echo "Reloading..."
        rebuild_aesthetic
    end
end

# Initial build and run
rebuild_aesthetic
