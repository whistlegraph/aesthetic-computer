#!/usr/bin/env bash
# Build WhistlegraphWizard — plain swiftc, ad-hoc signed dev tool.
set -euo pipefail
cd "$(dirname "$0")"
mkdir -p build
swiftc -O -o build/WhistlegraphWizard Sources/main.swift
codesign --force --sign - build/WhistlegraphWizard
echo "build/WhistlegraphWizard"
