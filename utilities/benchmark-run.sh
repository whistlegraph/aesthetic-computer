#!/bin/bash
# Quick launcher for MongoDB benchmark
# Usage: ./benchmark-run.sh [writes] [reads]

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Default values
WRITES=${1:-100}
READS=${2:-100}

echo -e "${GREEN}MongoDB Performance Benchmark${NC}"
echo "=============================="
echo ""

# Check if we're in the right directory
if [ ! -f "utilities/benchmark-mongodb.mjs" ]; then
  echo -e "${RED}Error: Must run from project root directory${NC}"
  echo "Usage: ./utilities/benchmark-run.sh [writes] [reads]"
  exit 1
fi

# Check for environment variables
if [ -z "$MONGODB_CONNECTION_STRING" ]; then
  echo -e "${YELLOW}⚠️  MONGODB_CONNECTION_STRING not set${NC}"
  echo ""
  echo "Please set your Atlas credentials:"
  echo "  export MONGODB_CONNECTION_STRING=\"mongodb+srv://...\""
  echo "  export MONGODB_NAME=\"aesthetic\""
  echo ""
  echo "Or source from a .env file:"
  echo "  export \$(grep -v '^#' system/.env | xargs)"
  echo ""
  exit 1
fi

# Show configuration
echo "Configuration:"
echo "  Writes: $WRITES documents"
echo "  Reads:  $READS operations"
echo ""
echo "Atlas: ${MONGODB_CONNECTION_STRING:0:30}..."
echo "Silo:  ${SILO_MONGODB_CONNECTION_STRING:-mongodb://silo.aesthetic.computer:27017/aesthetic}"
echo ""
echo -e "${YELLOW}Starting benchmark...${NC}"
echo ""

# Run the benchmark
node utilities/benchmark-mongodb.mjs --writes "$WRITES" --reads "$READS"

# Check exit code
if [ $? -eq 0 ]; then
  echo ""
  echo -e "${GREEN}✅ Benchmark completed successfully${NC}"
else
  echo ""
  echo -e "${RED}❌ Benchmark failed${NC}"
  exit 1
fi
