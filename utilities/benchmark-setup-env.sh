#!/bin/bash
# Helper script to export environment variables for benchmark
# Source this file: source utilities/benchmark-setup-env.sh

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;36m'
NC='\033[0m' # No Color

echo -e "${BLUE}Setting up environment variables for MongoDB benchmark${NC}"
echo ""

# Try to find Atlas credentials from existing .env files
ENV_FILE=""

# Check common locations
for file in system/.env netlify.toml .env; do
  if [ -f "$file" ] && grep -q "MONGODB_CONNECTION_STRING" "$file" 2>/dev/null; then
    ENV_FILE="$file"
    break
  fi
done

if [ -n "$ENV_FILE" ]; then
  echo -e "${GREEN}Found credentials in: $ENV_FILE${NC}"

  # Export Atlas variables
  export MONGODB_CONNECTION_STRING=$(grep MONGODB_CONNECTION_STRING "$ENV_FILE" | cut -d '=' -f2- | tr -d '"' | tr -d "'")
  export MONGODB_NAME=$(grep MONGODB_NAME "$ENV_FILE" | cut -d '=' -f2- | tr -d '"' | tr -d "'")

  if [ -n "$MONGODB_CONNECTION_STRING" ]; then
    echo -e "${GREEN}✅ MONGODB_CONNECTION_STRING set${NC}"
  fi

  if [ -n "$MONGODB_NAME" ]; then
    echo -e "${GREEN}✅ MONGODB_NAME set to: $MONGODB_NAME${NC}"
  fi
else
  echo -e "${YELLOW}⚠️  Could not find MONGODB_CONNECTION_STRING in .env files${NC}"
  echo ""
  echo "Please manually set:"
  echo "  export MONGODB_CONNECTION_STRING=\"mongodb+srv://...\""
  echo "  export MONGODB_NAME=\"aesthetic\""
fi

# Set silo defaults (can be overridden)
if [ -z "$SILO_MONGODB_CONNECTION_STRING" ]; then
  export SILO_MONGODB_CONNECTION_STRING="mongodb://silo.aesthetic.computer:27017/aesthetic"
  echo -e "${BLUE}Using default silo connection: $SILO_MONGODB_CONNECTION_STRING${NC}"
fi

if [ -z "$SILO_MONGODB_NAME" ]; then
  export SILO_MONGODB_NAME="aesthetic"
fi

echo ""
echo -e "${GREEN}Environment ready!${NC}"
echo ""
echo "Run benchmark with:"
echo "  node utilities/benchmark-mongodb.mjs"
echo "  # or"
echo "  ./utilities/benchmark-run.sh [writes] [reads]"
echo ""
