# MCP Registry Submission Guide

This guide covers how to submit the `@aesthetic.computer/mcp` server to various MCP registries and directories.

## Official MCP Registry

The official registry at https://registry.modelcontextprotocol.io uses a command-line publisher tool.

### Prerequisites

1. You've already published to npm: ✅ `@aesthetic.computer/mcp@1.0.0`
2. You need to authenticate using one of these methods:
   - GitHub OAuth (login as `whistlegraph`)
   - Domain verification for `aesthetic.computer`

### Submission Steps

1. **Clone the registry repository**:
   ```bash
   git clone https://github.com/modelcontextprotocol/registry
   cd registry
   ```

2. **Build the publisher tool**:
   ```bash
   make publisher
   ```

3. **Authenticate**:
   ```bash
   # Option 1: GitHub OAuth (as whistlegraph user)
   ./bin/mcp-publisher auth github

   # Option 2: Domain verification (for aesthetic.computer namespace)
   ./bin/mcp-publisher auth dns aesthetic.computer
   ```

4. **Publish the server**:
   ```bash
   ./bin/mcp-publisher publish \
     --package @aesthetic.computer/mcp \
     --name "aesthetic.computer" \
     --description "Publish creative coding pieces to aesthetic.computer" \
     --category "creative-tools" \
     --tags "creative-coding,javascript,kidlisp,art,music"
   ```

## Alternative Registries & Directories

### 1. MCP.so Community Registry

Visit https://mcp.so/ and search for submission instructions.

### 2. Cline MCP Marketplace

Repository: https://github.com/cline/mcp-marketplace

Create a PR adding a JSON file for your server:

```json
{
  "name": "aesthetic.computer",
  "package": "@aesthetic.computer/mcp",
  "description": "Publish creative coding pieces to aesthetic.computer",
  "author": "Jeffrey Alan Scudder",
  "repository": "https://github.com/whistlegraph/aesthetic-computer/tree/main/mcp-server",
  "homepage": "https://aesthetic.computer",
  "categories": ["creative-tools", "publishing"],
  "tags": ["javascript", "kidlisp", "creative-coding", "art", "music"]
}
```

### 3. PulseMCP Directory

Visit https://www.pulsemcp.com/ and look for "Submit Server" option.

## Package Metadata for Submission

Use this information when filling out registry forms:

### Basic Info
- **Name**: aesthetic.computer
- **Package**: @aesthetic.computer/mcp
- **Version**: 1.0.0
- **License**: MIT
- **Author**: Jeffrey Alan Scudder

### Description
```
MCP server for aesthetic.computer - enabling AI assistants to create and publish
JavaScript pieces, KidLisp art, and clock melodies. Supports anonymous publishing
with instant shareable URLs.
```

### Tags/Keywords
```
creative-coding, javascript, kidlisp, art, music, generative, publishing,
web-platform, anonymous, real-time
```

### Categories
- Creative Tools
- Publishing
- Development Tools

### Links
- **Homepage**: https://aesthetic.computer
- **Repository**: https://github.com/whistlegraph/aesthetic-computer/tree/main/mcp-server
- **npm**: https://www.npmjs.com/package/@aesthetic.computer/mcp
- **API Docs**: https://aesthetic.computer/api
- **KidLisp Docs**: https://kidlisp.com

### Features
- ✨ Publish JavaScript pieces to aesthetic.computer
- 🎨 Create and share KidLisp art (Lisp-based creative coding)
- 🎵 Compose clock melodies with pronounceable short codes
- 📚 Access API documentation programmatically
- 🚀 Get starter templates and references
- 🔄 Content deduplication - same code returns same URL
- 🌐 Anonymous publishing - no account required

### Compatible Clients
- Claude Desktop
- Claude Code (VS Code Extension)
- ChatGPT Developer Mode
- Cursor
- Any MCP-compatible client

### Installation Command
```bash
npx @aesthetic.computer/mcp
```

### Configuration Example (Claude Desktop)
```json
{
  "mcpServers": {
    "aesthetic-computer": {
      "command": "npx",
      "args": ["-y", "@aesthetic.computer/mcp"],
      "env": {
        "AC_TOKEN": "optional-bearer-token"
      }
    }
  }
}
```

## Social Media Announcement Template

Once published to registries, you can announce it:

### Twitter/X
```
🎨 Just published @aesthetic.computer MCP server!

Now Claude, ChatGPT & other AI assistants can:
✨ Create JavaScript pieces
🎨 Generate KidLisp art
🎵 Compose clock melodies

Try it: npx @aesthetic.computer/mcp

#MCP #CreativeCoding #AI
```

### Mastodon/Bluesky
```
🎨 The aesthetic.computer MCP server is now live!

AI assistants (Claude, ChatGPT, etc.) can now create and publish creative coding
pieces directly to aesthetic.computer.

✨ JavaScript pieces
🎨 KidLisp art (Lisp-based creative coding)
🎵 Clock melodies with shareable URLs
🌐 Anonymous publishing - no account needed

Install: npx @aesthetic.computer/mcp
Docs: https://aesthetic.computer/api

#ModelContextProtocol #CreativeCoding #AI #OpenSource
```

## Monitoring & Updates

After submission:

1. **Monitor npm stats**: https://www.npmjs.com/package/@aesthetic.computer/mcp
2. **Check registry listings**: Search for "aesthetic.computer" on:
   - https://registry.modelcontextprotocol.io
   - https://mcp.so
   - https://www.pulsemcp.com
3. **Update version**: When releasing new versions, re-publish to registries
4. **Respond to issues**: Monitor GitHub issues for user feedback

## Version Updates

When releasing a new version:

1. Update package.json version
2. Build: `npm run build`
3. Publish to npm: `npm publish`
4. Re-publish to MCP registry: `./bin/mcp-publisher publish ...`
5. Update README with new features
6. Create GitHub release tag

---

**Status**:
- ✅ Published to npm (1.0.0)
- ⏳ Pending MCP registry submission
- ⏳ Pending community registry listings
