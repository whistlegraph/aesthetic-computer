# @aesthetic-computer/mcp

MCP (Model Context Protocol) server for [aesthetic.computer](https://aesthetic.computer) - enabling AI assistants to create and publish creative coding pieces.

## What is this?

This MCP server allows AI assistants like Claude, GPT-4, and others to:
- ✨ Publish JavaScript pieces to aesthetic.computer
- 🎨 Create and share KidLisp art
- 🎵 Compose clock melodies
- 📚 Access API documentation
- 🚀 Get starter templates and references

## Installation

### NPM Global Install

```bash
npm install -g @aesthetic-computer/mcp
```

### NPX (No Install)

```bash
npx @aesthetic-computer/mcp
```

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

## Example Usage

Once configured, you can ask your AI assistant:

> "Create a piece that draws a bouncing ball"

> "Make a KidLisp piece with a yellow circle on a blue background"

> "Publish a clock melody using the C major scale"

> "Show me the piece template"

The AI will use the MCP tools to create and publish pieces automatically!

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
