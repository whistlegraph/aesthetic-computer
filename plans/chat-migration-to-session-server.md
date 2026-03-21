# Chat Migration to Session Server

> **Goal**: Merge `chat-system`, `chat-clock`, and `chat-sotce` from the `/nanos` directory (currently deployed on Google Cloud) into the existing `/session-server` (running on DigitalOcean at `session-server.aesthetic.computer`).

## 📊 Current Architecture

### Google Cloud (Going Away)
- **chat-system** → `chat-system.aesthetic.computer`
- **chat-clock** → `chat-clock.aesthetic.computer`
- **chat-sotce** → `chat.sotce.net`

All three are separate VM instances running the same `nanos/chat.mjs` with different `CHAT_INSTANCE` environment variables.

### DigitalOcean (Keeping)
- **session-server** → `session-server.aesthetic.computer` (157.245.134.225)
- Single Node.js process managed by pm2
- Already handles: WebSocket connections, UDP/WebRTC, Redis pub/sub, Firebase notifications, world state management

## 🎯 Migration Strategy

### Phase 1: Import Chat Module as Plugin
Keep the `nanos/chat.mjs` code intact (for potential future re-deployment) and import it as a module into `session-server`.

```
/session-server
├── session.mjs          # Main server (add chat routing)
├── chat.mjs             # Import from nanos (symlink or copy)
├── chat-instance.mjs    # NEW: Multi-instance chat manager
├── filter.mjs           # Already exists (profanity filter)
├── redact.mjs           # NEW: Copy from nanos
└── package.json         # Add mongodb dependency
```

### Phase 2: DNS Updates (Cloudflare)
Using credentials from `aesthetic-computer-vault/nanos/conductor.env`:
```
CLOUDFLARE_EMAIL=<from-vault>
CLOUDFLARE_API_TOKEN=<from-vault>
```

Update A records to point all chat subdomains to session-server IP:
- `chat-system.aesthetic.computer` → `157.245.134.225`
- `chat-clock.aesthetic.computer` → `157.245.134.225`
- `chat.sotce.net` → `157.245.134.225`

## 📝 Implementation Plan

### Task 1: Create Chat Module Wrapper
**File**: `/session-server/chat-manager.mjs`

```javascript
// Chat Manager - Multi-instance chat support for session-server
// Wraps the original nanos/chat.mjs logic for integration

import { WebSocketServer, WebSocket } from "ws";
import { MongoClient } from "mongodb";
import { initializeApp, cert } from "firebase-admin/app";
import { getMessaging } from "firebase-admin/messaging";
import { filter } from "./filter.mjs";
import { redact, unredact } from "./redact.mjs";

const MAX_MESSAGES = 500;

export const chatInstances = {
  "chat-system": {
    name: "chat-system",
    allowedHost: "chat-system.aesthetic.computer",
    userInfoEndpoint: "https://aesthetic.us.auth0.com/userinfo",
    topic: "mood", // FCM topic
  },
  "chat-sotce": {
    name: "chat-sotce",
    allowedHost: "chat.sotce.net",
    userInfoEndpoint: "https://sotce.us.auth0.com/userinfo",
    topic: "mood",
  },
  "chat-clock": {
    name: "chat-clock",
    allowedHost: "chat-clock.aesthetic.computer",
    userInfoEndpoint: "https://aesthetic.us.auth0.com/userinfo",
    topic: "mood",
  },
};

export class ChatInstance {
  constructor(config, db) {
    this.config = config;
    this.db = db;
    this.messages = [];
    this.connections = {};
    this.connectionId = 0;
    this.authorizedConnections = {};
    this.subsToHandles = {};
    this.subsToSubscribers = {};
  }
  // ... methods from nanos/chat.mjs
}
```

### Task 2: Modify Session Server Entry Point
**File**: `/session-server/session.mjs`

Add host-based routing for chat connections:

```javascript
// In the wss.on("connection") handler:
wss.on("connection", (ws, req) => {
  const host = req.headers.host;
  
  // Route to chat instances based on host
  if (chatInstances[host] || isChatHost(host)) {
    return chatManager.handleConnection(ws, req, host);
  }
  
  // ... existing session-server logic
});
```

### Task 3: Add Required Dependencies
**Update**: `/session-server/package.json`

```json
{
  "dependencies": {
    "mongodb": "^6.20.0",
    "node-fetch": "^3.3.2"
    // ... existing deps
  }
}
```

### Task 4: Environment Variables
**Update**: `/session-server/.env`

Add from `aesthetic-computer-vault/nanos/chat.env`:
```dotenv
# Existing (from vault)
REDIS_CONNECTION_STRING=<from-vault>
GCM_FIREBASE_CONFIG_URL=<from-vault>

# New for chat (from vault)
MONGODB_NAME=aesthetic
MONGODB_CONNECTION_STRING=<from-vault>
LOGGER_KEY=<from-vault>
```

### Task 5: Update Cloudflare DNS
**Script**: `/session-server/update-chat-dns.mjs`

```javascript
// Updates all chat subdomains to point to session-server IP
const CLOUDFLARE_EMAIL = process.env.CLOUDFLARE_EMAIL;
const CLOUDFLARE_API_TOKEN = process.env.CLOUDFLARE_API_TOKEN;
const SESSION_SERVER_IP = "157.245.134.225";

const domains = [
  { subdomain: "chat-system.aesthetic.computer", zone: "aesthetic.computer" },
  { subdomain: "chat-clock.aesthetic.computer", zone: "aesthetic.computer" },
  { subdomain: "chat.sotce.net", zone: "sotce.net" },
];

// Use Cloudflare API to update A records
```

## 🧪 Testing Plan

### Unit Tests (`/session-server/tests/`)

```
tests/
├── chat.test.mjs         # Chat functionality tests
├── filter.test.mjs       # Profanity filter tests
├── auth.test.mjs         # Auth0 authorization tests
└── integration.test.mjs  # Full integration tests
```

#### Test 1: Chat Message Flow
```javascript
// tests/chat.test.mjs
import { describe, it, expect } from 'vitest'; // or mocha/chai

describe('Chat Message Flow', () => {
  it('should accept authorized chat messages', async () => {
    // 1. Connect WebSocket to chat-system endpoint
    // 2. Send chat:message with valid token
    // 3. Verify message is broadcast to all clients
    // 4. Verify message is stored in MongoDB
  });

  it('should reject unauthorized chat messages', async () => {
    // 1. Connect WebSocket
    // 2. Send chat:message with invalid/no token
    // 3. Verify 'unauthorized' response
  });

  it('should filter profanity', async () => {
    // 1. Send message with profanity
    // 2. Verify message text is filtered
  });

  it('should enforce message length limits', async () => {
    // 1. Send message > 128 chars
    // 2. Verify 'too-long' response
  });
});
```

#### Test 2: Multi-Instance Isolation
```javascript
describe('Chat Instance Isolation', () => {
  it('chat-system messages should not appear in chat-clock', async () => {
    // 1. Connect to chat-system, send message
    // 2. Connect to chat-clock
    // 3. Verify message not in chat-clock history
  });

  it('sotce.net requires subscription check', async () => {
    // 1. Connect to chat.sotce.net
    // 2. Send message without subscription
    // 3. Verify rejection with subscription required
  });
});
```

#### Test 3: Push Notifications
```javascript
describe('Push Notifications', () => {
  it('should send FCM notification on chat-system message', async () => {
    // Mock FCM and verify notification payload
  });

  it('should use clock emoji for chat-clock notifications', async () => {
    // Verify clock emoji based on time
  });
});
```

### CLI Test Script
**File**: `/session-server/test-chat.fish`

```fish
#!/usr/bin/env fish

echo "🧪 Testing Chat Integration..."

# Test 1: WebSocket connection
echo "Test 1: WebSocket Connection"
echo '{"type":"ping"}' | websocat -1 wss://chat-system.aesthetic.computer

# Test 2: HTTP health check
echo "Test 2: HTTP Health Check"
curl -s https://chat-system.aesthetic.computer | head -1

# Test 3: Log endpoint (requires auth)
echo "Test 3: Log Endpoint"
curl -s -X POST https://chat-system.aesthetic.computer/log \
  -H "Authorization: Bearer $LOGGER_KEY" \
  -H "Content-Type: application/json" \
  -d '{"text":"Test log message","when":"2024-01-01T00:00:00Z"}'

echo "✅ All tests complete"
```

## 📋 Implementation Checklist

### Pre-Migration
- [ ] Create backup of current MongoDB chat collections
- [ ] Document current GCP instance IPs for rollback
- [ ] Test session-server can handle additional WebSocket load

### Code Changes
- [ ] Copy `nanos/redact.mjs` to `/session-server/redact.mjs`
- [ ] Create `/session-server/chat-manager.mjs` (wrapper module)
- [ ] Update `/session-server/session.mjs` (add chat routing)
- [ ] Update `/session-server/package.json` (add mongodb)
- [ ] Update `/session-server/.env` (add MongoDB connection)
- [ ] Create `/session-server/update-chat-dns.mjs` (DNS script)

### Testing
- [ ] Run local tests with `npm run dev`
- [ ] Test each chat instance locally (8083, 8084, 8085 ports)
- [ ] Create test suite in `/session-server/tests/`

### Deployment
- [ ] Deploy updated session-server to DigitalOcean
- [ ] Run DNS update script to point subdomains to session-server
- [ ] Verify all three chat instances are working
- [ ] Monitor logs for errors

### Post-Migration
- [ ] Shutdown GCP instances (don't delete yet - keep for 1 week)
- [ ] Monitor for any issues
- [ ] Delete GCP instances after 1 week of stable operation

## 🔄 Rollback Plan

If issues occur:
1. Re-run `nanos/conductor.mjs` to deploy back to GCP
2. DNS will automatically update back to GCP IPs
3. Session-server can continue running without chat functionality

## 💰 Cost Savings

- **Before**: 3x GCP VM instances for chat
- **After**: 0 GCP instances, chat runs on existing DigitalOcean droplet
- **Savings**: Full GCP compute bill elimination

## 🗺️ Architecture Diagram

```
                    ┌─────────────────────────────────┐
                    │         Cloudflare DNS          │
                    │  (Proxied A Records)            │
                    └─────────────┬───────────────────┘
                                  │
          ┌───────────────────────┼───────────────────────┐
          │                       │                       │
          ▼                       ▼                       ▼
   chat-system.ac          chat-clock.ac           chat.sotce.net
          │                       │                       │
          └───────────────────────┼───────────────────────┘
                                  │
                                  ▼
                    ┌─────────────────────────────────┐
                    │   DigitalOcean Droplet          │
                    │   157.245.134.225               │
                    │                                 │
                    │  ┌──────────────────────────┐  │
                    │  │    session.mjs           │  │
                    │  │                          │  │
                    │  │  ┌─────────────────────┐ │  │
                    │  │  │   Chat Manager      │ │  │
                    │  │  │                     │ │  │
                    │  │  │  • chat-system      │ │  │
                    │  │  │  • chat-clock       │ │  │
                    │  │  │  • chat-sotce       │ │  │
                    │  │  └─────────────────────┘ │  │
                    │  │                          │  │
                    │  │  ┌─────────────────────┐ │  │
                    │  │  │  Existing Session   │ │  │
                    │  │  │  (worlds, UDP, etc) │ │  │
                    │  │  └─────────────────────┘ │  │
                    │  └──────────────────────────┘  │
                    └─────────────────────────────────┘
                                  │
                    ┌─────────────┴─────────────┐
                    │                           │
                    ▼                           ▼
           ┌──────────────┐            ┌──────────────┐
           │   MongoDB    │            │    Redis     │
           │   Atlas      │            │  DigitalOcean│
           └──────────────┘            └──────────────┘
```

## 📂 File Structure After Migration

```
/session-server
├── session.mjs           # Main entry (updated with chat routing)
├── chat-manager.mjs      # NEW: Multi-instance chat manager
├── filter.mjs            # Profanity filter (existing)
├── redact.mjs            # NEW: Message redaction (from nanos)
├── package.json          # Updated with mongodb dep
├── .env                  # Updated with MongoDB connection
├── update-chat-dns.mjs   # NEW: DNS update script
├── test-chat.fish        # NEW: CLI test script
├── tests/
│   ├── chat.test.mjs
│   ├── filter.test.mjs
│   └── integration.test.mjs
├── DEPLOY.md             # Updated deployment docs
└── README.md             # Updated with chat info
```

## 🚀 Quick Start Commands

```fish
# Local development (all services)
cd /workspaces/aesthetic-computer/session-server
npm run dev

# Test chat locally
curl -s http://localhost:8889/ # Session server
# Chat instances will be on different ports or host-routed

# Deploy to production
./deploy.fish

# Update DNS after deploy
node update-chat-dns.mjs

# Run tests
npm test
```

---

**Author**: Claude (AI Assistant)
**Created**: 2024-11-28
**Status**: Planning Phase
