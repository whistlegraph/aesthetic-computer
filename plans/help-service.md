# help.aesthetic.computer — AI Agent Service Plan

## Overview

A Claude-powered AI assistant service for the Aesthetic Computer community.
Users with AC handles can bring their own Anthropic API key and get AI help
with pieces, KidLisp, creative computing, and more.

**Domain:** `help.aesthetic.computer`
**IP:** `146.190.150.173`
**Droplet:** `help-aesthetic-computer` (ID: 552385566)
**Specs:** 2 vCPU, 4GB RAM, Ubuntu 24.04, SFO3 (~$24/mo)
**Status:** Live — landing page + Auth0 sign-in deployed

---

## Architecture

### No Container Isolation Needed

Unlike NanoClaw (which runs Claude Code CLI with Bash/file access in containers),
this service uses the Claude API with **server-controlled tools**. Users can't
escape the defined tool set — no sandbox needed.

### Stack

- **Node.js 22 + Express** (same pattern as silo, oven)
- **Caddy** (HTTPS reverse proxy, auto-certs)
- **MongoDB** (via silo — user keys, conversation history, tool permissions)
- **Systemd** (service management)
- **Claude API** (`@anthropic-ai/sdk` npm package)

### Auth Flow

1. User visits `help.aesthetic.computer`
2. Auth0 login (same as rest of AC)
3. User must have an `@handle` (registered AC user)
4. User provides their Anthropic API key in settings
5. Key stored encrypted in MongoDB
6. All Claude API calls use the user's own key

### Landing Page

The landing page explains:
- What help.aesthetic.computer does
- How to get an AC handle (if you don't have one)
- How to get an Anthropic API key
- How to connect your key
- What tools/capabilities are available

---

## Tool Allow/Deny Lists (RBAC)

Tools are defined server-side and passed to the Claude API per-request
based on user role. Users never see tools they don't have access to.

### @jeffrey (admin)

```
allow: [
  create-piece,        # Create new .mjs or .lisp pieces
  edit-piece,          # Modify existing pieces
  search-codebase,     # Search AC source code
  explain-piece,       # Explain how a piece works
  run-kidlisp,         # Execute KidLisp code
  system-status,       # Check oven/silo/session health
  manage-users,        # View/manage user permissions
  deploy,              # Trigger deployments
  raw-chat,            # Unconstrained Claude conversation
  search-docs,         # Search AC documentation
]
deny: []
```

### Registered User (@handle)

```
allow: [
  create-kidlisp,      # Create KidLisp pieces in own namespace
  fork-piece,          # Fork existing pieces
  search-docs,         # Search AC documentation
  explain-piece,       # Explain how a piece works
  raw-chat,            # Claude conversation (with AC context)
  my-pieces,           # List/manage own published pieces
]
deny: [
  deploy,
  system-status,
  manage-users,
  edit-piece (others'),
  search-codebase,
]
```

### Anonymous (no handle, no key)

```
allow: [
  search-docs,         # Read-only doc search
  explain-piece,       # Explain public pieces
]
deny: [everything else]
```

### Storage

Tool permissions stored in MongoDB `help-permissions` collection.
Editable from silo dashboard. Start with hardcoded defaults,
make configurable later.

---

## API Endpoints

### Public

| Endpoint | Method | Purpose |
|----------|--------|---------|
| `/` | GET | Landing page (HTML) |
| `/health` | GET | Health check |
| `/auth/config` | GET | Auth0 config for frontend |

### Authenticated

| Endpoint | Method | Purpose |
|----------|--------|---------|
| `/auth/me` | GET | Current user info + role |
| `/api/settings` | GET/PUT | API key management |
| `/api/chat` | POST | Send message to Claude (SSE streaming) |
| `/api/conversations` | GET | List conversation history |
| `/api/conversations/:id` | GET/DELETE | Get/delete conversation |
| `/ws` | WebSocket | Real-time streaming (alternative to SSE) |

### Admin Only (@jeffrey)

| Endpoint | Method | Purpose |
|----------|--------|---------|
| `/api/admin/users` | GET | List users + their roles |
| `/api/admin/permissions` | GET/PUT | Edit tool permissions |
| `/api/admin/stats` | GET | Usage stats |

---

## Claude API Integration

### Per-Request Flow

```javascript
import Anthropic from '@anthropic-ai/sdk';

async function handleChat(user, message, conversationHistory) {
  // Get user's API key from MongoDB (decrypted)
  const apiKey = await getUserApiKey(user.sub);

  // Get user's allowed tools based on role
  const tools = getToolsForRole(user.role);

  // Create client with user's key
  const client = new Anthropic({ apiKey });

  // System prompt with AC context
  const systemPrompt = buildSystemPrompt(user);

  // Stream response
  const stream = client.messages.stream({
    model: 'claude-sonnet-4-5-20250929',
    max_tokens: 4096,
    system: systemPrompt,
    messages: [...conversationHistory, { role: 'user', content: message }],
    tools,
  });

  return stream;
}
```

### System Prompt

The system prompt includes:
- AC documentation context (from CLAUDE.md, piece API docs)
- KidLisp reference (118 built-in functions)
- User's role and permissions
- Conversation instructions (be helpful, AC-aware)

### Tool Definitions

Each tool is a Claude API tool definition + a server-side handler:

```javascript
const TOOLS = {
  'search-docs': {
    definition: {
      name: 'search_docs',
      description: 'Search AC documentation and piece source code',
      input_schema: {
        type: 'object',
        properties: {
          query: { type: 'string', description: 'Search query' }
        },
        required: ['query']
      }
    },
    handler: async (input) => {
      // Server-side: search docs, return results
      // User can't escape this — it's just a search
    }
  },
  // ... more tools
};
```

---

## Deployment

### Provisioning Script

Create `help/deploy.fish` following the judge/oven pattern:

1. SSH into droplet (need to add SSH key first)
2. Wipe judge files: `rm -rf /opt/judge`, stop ollama
3. Install/update Node.js 22
4. Create `/opt/help` directory
5. Upload `server.mjs`, `dashboard.html`, `package.json`, `.env`
6. Install npm dependencies
7. Configure Caddy for `help.aesthetic.computer`
8. Create systemd service `help`
9. Start services

### SSH Key

The judge SSH key was generated on a different machine. Need to either:
- Add the vault's `id_rsa.pub` to the droplet via DO API (console access)
- Or destroy and recreate the droplet (fresh start, same IP not guaranteed)
- Or use DO console to add a new key

### Environment Variables

```bash
# MongoDB (via silo)
MONGODB_CONNECTION_STRING=mongodb://aesthetic_app:...@silo.aesthetic.computer:27017/...
MONGODB_NAME=aesthetic

# Auth0
AUTH0_DOMAIN=aesthetic.us.auth0.com
AUTH0_CLIENT_ID=...
AUTH0_CUSTOM_DOMAIN=hi.aesthetic.computer
ADMIN_SUB=auth0|63effeeb2a7d55f8098d62f9

# Encryption key for stored API keys
ENCRYPTION_KEY=<generate new key>

# Optional: fallback community API key
ANTHROPIC_API_KEY=<for anonymous/shared use>
```

---

## File Structure

```
help/
├── server.mjs          # Express API server
├── dashboard.html      # Landing page + chat UI
├── tools/              # Tool definitions + handlers
│   ├── search-docs.mjs
│   ├── explain-piece.mjs
│   ├── create-kidlisp.mjs
│   ├── fork-piece.mjs
│   └── system-status.mjs
├── deploy.fish         # Deployment script
├── redeploy.fish       # Quick update script
├── package.json
└── README.md
```

Vault:
```
aesthetic-computer-vault/help/
├── deploy.env          # DO + Cloudflare credentials
└── .env                # MongoDB, Auth0, encryption keys
```

---

## Relationship to NanoClaw

The `/nanoclaw` submodule remains as **reference code** for:
- Claude Agent SDK patterns (session management, tool execution)
- How to build async message streams
- IPC patterns between host and agent
- Memory/context management via CLAUDE.md files

We don't run NanoClaw — we build our own simpler service.

---

## Infrastructure Summary

| Service | Domain | IP | Purpose |
|---------|--------|----|---------|
| oven | oven.aesthetic.computer | 137.184.237.166 | Screenshot/video |
| silo | silo.aesthetic.computer | 64.23.151.169 | DB dashboard |
| session | session-server | 157.245.134.225 | Real-time multiplayer |
| **help** | **help.aesthetic.computer** | **146.190.150.173** | **AI agent** |
| system | aesthetic.computer | Netlify | Main site |

---

## Phase Plan

### Phase 1: Foundation (DONE)
- [x] Recreated droplet with SSH key (destroyed old judge, new ID 552385566)
- [x] Installed Node.js 22 + Caddy 2.10
- [x] Built minimal Express server with Caddy reverse proxy
- [x] Auth0 integration (same client as silo)
- [x] Landing page with sign-in + API key input

### Phase 2: Core Chat
- [ ] API key storage (encrypted in MongoDB)
- [ ] Claude API integration with streaming
- [ ] Basic chat UI
- [ ] Conversation history

### Phase 3: Tools
- [ ] search-docs tool
- [ ] explain-piece tool
- [ ] create-kidlisp tool
- [ ] Role-based tool filtering

### Phase 4: Community
- [ ] Public launch
- [ ] Usage analytics
- [ ] Rate limiting
- [ ] Admin dashboard in silo
