# 🔧 KidLisp Development Tools

This directory contains utilities for KidLisp development and analysis.

## Available Tools

### `api-summary.mjs`
**Dynamic API analysis tool** - Generates a complete function inventory
```bash
node tools/api-summary.mjs
```
- Lists all 118 KidLisp functions across 12 categories
- Analyzes function distribution and prominence  
- Provides API balance reporting
- Useful for documentation and feature planning

### `get-source.mjs`
**Source code extraction utility** - Retrieves KidLisp piece source code
```bash
node tools/get-source.mjs
```

### `source-tree.mjs`  
**Codebase analysis tool** - Analyzes project structure
```bash
node tools/source-tree.mjs
```

## Usage

All tools are designed to be run from the main `aesthetic-computer` directory:
```bash
cd /workspaces/aesthetic-computer
node kidlisp/tools/api-summary.mjs
```

## Development Notes

These tools help maintain KidLisp by:
- Tracking API completeness
- Documenting function relationships
- Analyzing implementation patterns
- Supporting documentation generation
