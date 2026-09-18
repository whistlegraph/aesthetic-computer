#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."
if [[ "$(uname -s)" == Darwin ]]; then
  nice -n 10 swiftc -O native/credit-label.swift -o native/credit-label
fi
