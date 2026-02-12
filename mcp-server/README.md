# @aesthetic.computer/mcp

[![npm version](https://badge.fury.io/js/@aesthetic.computer%2Fmcp.svg)](https://www.npmjs.com/package/@aesthetic.computer/mcp)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

MCP (Model Context Protocol) server for [aesthetic.computer](https://aesthetic.computer) - enabling AI assistants to create and publish creative coding pieces.

## What is this?

This MCP server allows AI assistants like **Claude**, **ChatGPT**, and other MCP-compatible tools to:
- ✨ **Publish JavaScript pieces** to aesthetic.computer
- 🎨 **Create and share KidLisp art** (a Lisp-based creative coding language)
- 🎵 **Compose clock melodies** with pronounceable short codes
- 📚 **Access API documentation** programmatically
- 🚀 **Get starter templates** and references
- 🔄 **Content deduplication** - same code returns same URL
- 🌐 **Anonymous publishing** - no account required

## Installation

### NPM Global Install

```bash
npm install -g @aesthetic.computer/mcp
```

### NPX (No Install - Recommended)

```bash
npx @aesthetic.computer/mcp
```

No installation required! The `-y` flag in MCP configs will automatically download and run the latest version.

## Configuration

### Claude Desktop

Add to your `~/Library/Application Support/Claude/claude_desktop_config.json` (macOS) or `%APPDATA%\Claude\claude_desktop_config.json` (Windows):

```json
{
  "mcpServers": {
    "aesthetic-computer": {
      "command": "npx",
      "args": ["-y", "@aesthetic-computer/mcp"],
      "env": {
        "AC_TOKEN": "your-auth0-bearer-token-here"
      }
    }
  }
}
```

### Claude Code (VS Code Extension)

Add to your `.vscode/mcp.json` or `~/.claude/mcp.json`:

```json
{
  "mcpServers": {
    "aesthetic-computer": {
      "command": "npx",
      "args": ["-y", "@aesthetic-computer/mcp"],
      "env": {
        "AC_TOKEN": "your-auth0-bearer-token-here"
      }
    }
  }
}
```

### Cursor

Add to your Cursor MCP settings:

```json
{
  "mcpServers": {
    "aesthetic-computer": {
      "command": "npx",
      "args": ["-y", "@aesthetic-computer/mcp"]
    }
  }
}
```

### ChatGPT Developer Mode

ChatGPT now supports MCP servers! Add to your ChatGPT configuration:

```json
{
  "mcpServers": {
    "aesthetic-computer": {
      "command": "npx",
      "args": ["-y", "@aesthetic.computer/mcp"]
    }
  }
}
```

See [ChatGPT Developer Mode docs](https://platform.openai.com/docs/guides/developer-mode) for setup instructions.

### Authentication (Optional)

The `AC_TOKEN` environment variable is **optional**. If not provided:
- All publishing is **anonymous** (guest mode)
- Pieces are still publicly accessible
- To associate pieces with your account, provide a Bearer token from aesthetic.computer

## Available Tools

### `publish_piece`

Publish a JavaScript piece to aesthetic.computer.

**Input:**
```json
{
  "source": "export function boot($) { ... }\nexport function paint($) { ... }",
  "name": "my-piece"
}
```

**Output:**
```json
{
  "code": "drift",
  "url": "https://aesthetic.computer/drift",
  "cached": false
}
```

### `publish_kidlisp`

Publish KidLisp code.

**Input:**
```json
{
  "source": "(wipe blue)\n(ink yellow)\n(circle (/ w 2) (/ h 2) 100)"
}
```

**Output:**
```json
{
  "code": "xyz789",
  "url": "https://aesthetic.computer/xyz789",
  "cached": false
}
```

### `publish_clock`

Publish a clock melody.

**Input:**
```json
{
  "source": "c4 e4 g4 c5 g4 e4 c4"
}
```

**Output:**
```json
{
  "code": "bako",
  "url": "https://aesthetic.computer/clock~bako",
  "cached": false
}
```

### `get_api_info`

Fetch the full API documentation.

## Available Resources

### `aesthetic-computer://piece-template`

Returns a starter template for a new aesthetic.computer piece with all lifecycle functions (boot, paint, sim, act).

### `aesthetic-computer://kidlisp-reference`

Quick reference guide for KidLisp syntax and common functions.

## Available Prompts

### `create-piece`

A guided prompt for creating an aesthetic.computer piece.

**Arguments:**
- `name` (required): Name of the piece
- `description` (required): What the piece should do

## Why Use This?

- **🎨 Creative Coding Made Easy**: Let AI assistants write aesthetic.computer pieces for you
- **⚡ Instant Publishing**: Go from idea to live URL in seconds
- **🔗 Shareable URLs**: Every piece gets a clean, shareable link
- **🆓 No Account Required**: Anonymous publishing works out of the box
- **♻️ Smart Deduplication**: Identical code returns the same URL
- **🌍 Open Platform**: All pieces are publicly accessible

## Example Conversations

Once configured, you can ask your AI assistant:

### JavaScript Pieces
> "Create a piece that draws a bouncing ball"
>
> "Make an interactive drawing canvas where I can paint with my mouse"
>
> "Build a particle system that responds to sound"

### KidLisp Art
> "Make a KidLisp piece with a yellow circle on a blue background"
>
> "Create generative art using KidLisp with random shapes"

### Clock Melodies
> "Publish a clock melody using the C major scale"
>
> "Compose a pentatonic melody for the clock"

### Learning & Templates
> "Show me the piece template"
>
> "What functions can I use in KidLisp?"

The AI will use the MCP tools to create and publish pieces automatically, returning live URLs you can visit immediately!

## Development

```bash
# Clone the repo
git clone https://github.com/whistlegraph/aesthetic-computer
cd aesthetic-computer/mcp-server

# Install dependencies
npm install

# Build
npm run build

# Test locally
node dist/index.js
```

## Links

- 🌐 [aesthetic.computer](https://aesthetic.computer)
- 📖 [API Documentation](https://aesthetic.computer/api)
- 🎨 [KidLisp Documentation](https://kidlisp.com)
- 📝 [Write a Piece Guide](https://github.com/whistlegraph/aesthetic-computer/blob/main/WRITE-A-PIECE.md)
- 🔧 [MCP Specification](https://modelcontextprotocol.io)

## License

MIT

## Author

Jeffrey Alan Scudder
