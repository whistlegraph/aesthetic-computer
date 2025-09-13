#!/bin/bash

# Build and push the dev container image to avoid rebuild issues
IMAGE_NAME="aesthetic-computer-dev:latest"

echo "Building dev container image..."
docker build -t $IMAGE_NAME .

echo "Image built successfully: $IMAGE_NAME"
echo "Image size:"
docker images $IMAGE_NAME

# Optionally push to a registry (uncomment and configure as needed)
# echo "Pushing to registry..."
# docker tag $IMAGE_NAME your-registry/aesthetic-computer-dev:latest
# docker push your-registry/aesthetic-computer-dev:latest

echo "Done! You can now update devcontainer.json to use: $IMAGE_NAME"
