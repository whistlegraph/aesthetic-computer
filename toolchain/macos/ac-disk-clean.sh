#!/bin/bash
# Compatibility wrapper. The fleet macOS disk janitor is now called Cleaner.
set -euo pipefail
exec "$(cd "$(dirname "$0")" && pwd)/cleaner.sh" "$@"
