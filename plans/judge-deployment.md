# Judge Deployment Plan - AI Chat Moderation Service

## 📋 Executive Summary

Deploy the `/judge` Ollama-based chat moderation system as a subdomain service (`judge.aesthetic.computer`) following the established `/oven` deployment pattern, then integrate it into the existing chat infrastructure to replace the current `nanos/filter.mjs` regex-based profanity filter.

**Current State:**
- ✅ Working local prototype in `/judge`
- ✅ gemma2:2b model (100% explicit content blocking, ~1.3s/msg, 250MB RAM)
- ✅ Web dashboard with WebSocket streaming
- ✅ MongoDB integration for testing
- ❌ Not deployed to production
- ❌ Not integrated with chat system

**Goal State:**
- 🎯 Production deployment at `judge.aesthetic.computer`
- 🎯 Replace `nanos/filter.mjs` with API calls to judge service
- 🎯 Real-time moderation for all chat instances (chat-system, chat-sotce, chat-clock)
- 🎯 Dashboard monitoring at `judge.aesthetic.computer`

---

## 🏗️ Architecture Analysis

### Current Stack Overview

#### Chat Infrastructure
```
┌─────────────────────────────────────────────────────────────┐
│ Chat Servers (Google Cloud Compute - Unikernels)            │
├─────────────────────────────────────────────────────────────┤
│ • chat-system.aesthetic.computer    (port 8083)             │
│ • chat.sotce.net                    (port 8084)             │
│ • chat-clock.aesthetic.computer     (port 8085)             │
│                                                              │
│ Current Filter: nanos/filter.mjs (regex-based)              │
│   - Uses 'obscenity' package                                │
│   - Replaces profanity with underscores                     │
│   - Limited to English wordlist                             │
│   - No context awareness                                    │
└─────────────────────────────────────────────────────────────┘
```

#### Proven Deployment Pattern: /oven
```
┌─────────────────────────────────────────────────────────────┐
│ oven.aesthetic.computer (DigitalOcean Droplet)              │
├─────────────────────────────────────────────────────────────┤
│ • Dedicated service for video processing                    │
│ • Node.js + ffmpeg + Caddy                                  │
│ • HTTPS with auto-certificates                              │
│ • systemd service for auto-restart                          │
│ • POST /bake endpoint                                       │
│ • WebSocket dashboard at /                                  │
│ • Webhook callback to Netlify on completion                 │
│                                                              │
│ Deployed via: oven/deploy.fish (automated)                  │
└─────────────────────────────────────────────────────────────┘
```

---

## 🎯 Proposed Architecture

### Option 1: Dedicated Droplet (Recommended)
**Following /oven pattern - separate infrastructure**

```
┌────────────────────────────────────────────────────────────────┐
│ judge.aesthetic.computer (DigitalOcean Droplet)                │
├────────────────────────────────────────────────────────────────┤
│ Hardware:                                                       │
│  • 1GB RAM / 1 vCPU ($6/month) - sufficient for gemma2:2b      │
│  • Ubuntu 22.04 LTS                                            │
│                                                                 │
│ Software Stack:                                                │
│  • Node.js 24                                                  │
│  • Ollama (with gemma2:2b model)                               │
│  • Caddy (HTTPS + auto-certs)                                  │
│  • systemd service (judge.service)                             │
│                                                                 │
│ Endpoints:                                                     │
│  • GET  /              → Dashboard (WebSocket streaming)       │
│  • POST /api/filter    → Filter single message                 │
│  • GET  /api/health    → Health check                          │
│  • WS   /ws            → WebSocket for real-time updates       │
│                                                                 │
│ Features:                                                      │
│  • Auto-restart on crash                                       │
│  • Log rotation                                                │
│  • MongoDB connection for analytics                            │
│  • Rate limiting (100 req/min per IP)                          │
└────────────────────────────────────────────────────────────────┘
```

**Pros:**
- ✅ Isolated resource usage (Ollama won't affect chat)
- ✅ Easy to scale independently
- ✅ Matches proven /oven deployment pattern
- ✅ Can upgrade RAM if needed without affecting other services
- ✅ Complete control over Ollama configuration

**Cons:**
- ❌ Additional $6/month cost
- ❌ Network latency (API call to different server)
- ❌ Complexity of maintaining another droplet

### Option 2: Co-locate with Chat Servers
**Install on existing Google Cloud chat unikernels**

```
┌────────────────────────────────────────────────────────────────┐
│ chat-system.aesthetic.computer (Google Compute)                │
├────────────────────────────────────────────────────────────────┤
│ Existing:                                                      │
│  • WebSocket chat server (port 8083)                           │
│  • MongoDB connection                                          │
│  • Firebase notifications                                      │
│                                                                 │
│ Add:                                                           │
│  • Ollama service (local)                                      │
│  • Judge API server (port 3000, internal only)                 │
│  • Caddy proxy to expose dashboard                             │
│                                                                 │
│ Filter Integration:                                            │
│  • Replace filter.mjs import with HTTP call to localhost:3000  │
│  • No network latency (same machine)                           │
└────────────────────────────────────────────────────────────────┘
```

**Pros:**
- ✅ No additional infrastructure cost
- ✅ Lowest latency (localhost)
- ✅ Simpler deployment (one server)

**Cons:**
- ❌ Resource competition with chat (Ollama uses 250MB RAM)
- ❌ Harder to debug/monitor separately
- ❌ Less flexible scaling
- ❌ Chat server downtime affects moderation

### **Recommendation: Option 1 (Dedicated Droplet)**
Following the established /oven pattern for consistency and isolation.

---

## 📦 Deployment Steps

### Phase 1: Prepare Judge Service for Production

#### 1.1 Create Deployment Script
**File:** `judge/deploy.fish`

```fish
#!/usr/bin/env fish
# Deploy judge service to DigitalOcean droplet

set DROPLET_NAME "judge-aesthetic-computer"
set DROPLET_REGION "nyc3"
set DROPLET_SIZE "s-1vcpu-1gb"  # $6/month
set DROPLET_IMAGE "ubuntu-22-04-x64"

echo "🚀 Deploying Judge Moderation Service"
echo "═══════════════════════════════════════"

# Step 1: Create droplet if it doesn't exist
# (Similar to oven/deploy.fish implementation)

# Step 2: Install dependencies
#  - Node.js 24
#  - Ollama
#  - Caddy
#  - MongoDB client libraries

# Step 3: Pull gemma2:2b model
#  - ollama pull gemma2:2b

# Step 4: Deploy service files
#  - api-server.mjs
#  - package.json
#  - systemd service file

# Step 5: Configure Caddy
#  - HTTPS with auto-certs
#  - Proxy to Node.js on port 3000

# Step 6: Set environment variables
#  - MONGODB_CONNECTION_STRING (from vault)
#  - MONGODB_NAME
#  - COLLECTION_NAME

# Step 7: Start services
#  - systemctl enable ollama
#  - systemctl enable judge
#  - systemctl start judge
```

#### 1.2 Create systemd Service
**File:** `judge/judge.service`

```ini
[Unit]
Description=Judge - AI Chat Moderation Service
After=network.target ollama.service
Requires=ollama.service

[Service]
Type=simple
User=judge
WorkingDirectory=/opt/judge
Environment="NODE_ENV=production"
EnvironmentFile=/opt/judge/.env
ExecStart=/usr/bin/node /opt/judge/api-server.mjs
Restart=always
RestartSec=10
StandardOutput=append:/var/log/judge/judge.log
StandardError=append:/var/log/judge/judge.log

[Install]
WantedBy=multi-user.target
```

#### 1.3 Create Caddyfile
**File:** `judge/Caddyfile.production`

```caddyfile
judge.aesthetic.computer {
    # Serve dashboard
    reverse_proxy localhost:3000
    
    # WebSocket support
    @websockets {
        header Connection *Upgrade*
        header Upgrade websocket
    }
    reverse_proxy @websockets localhost:3000
    
    # Enable logging
    log {
        output file /var/log/caddy/judge.log
    }
}
```

#### 1.4 Add Health Check Endpoint
**Update:** `judge/api-server.mjs`

```javascript
// Add health check endpoint
if (req.method === 'GET' && req.url === '/api/health') {
  res.writeHead(200, { 'Content-Type': 'application/json' });
  res.end(JSON.stringify({
    status: 'healthy',
    model: MODEL,
    uptime: process.uptime(),
    memory: process.memoryUsage(),
    timestamp: Date.now()
  }));
  return;
}
```

---

### Phase 2: Integration with Chat System

#### 2.1 Create Judge Client Library
**File:** `nanos/judge-client.mjs`

```javascript
// Judge API client for chat servers
import fetch from 'node-fetch';

const JUDGE_URL = process.env.JUDGE_URL || 'https://judge.aesthetic.computer';
const JUDGE_TIMEOUT = 2000; // 2 second timeout

export async function filterMessage(text, debug = false) {
  try {
    const controller = new AbortController();
    const timeout = setTimeout(() => controller.abort(), JUDGE_TIMEOUT);
    
    const response = await fetch(`${JUDGE_URL}/api/filter`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ message: text }),
      signal: controller.signal
    });
    
    clearTimeout(timeout);
    
    if (!response.ok) {
      throw new Error(`Judge API error: ${response.status}`);
    }
    
    const result = await response.json();
    
    if (debug) {
      console.log('⚖️ Judge result:', {
        decision: result.decision,
        reason: result.reason,
        responseTime: result.responseTime
      });
    }
    
    // Return decision: 't' = allowed, 'f' = blocked
    return {
      allowed: result.decision === 't',
      blocked: result.decision === 'f',
      reason: result.reason,
      sentiment: result.sentiment,
      responseTime: result.responseTime
    };
    
  } catch (error) {
    console.error('🔴 Judge API error:', error.message);
    
    // FALLBACK: On timeout/error, fall back to regex filter
    const { filter } = await import('./filter.mjs');
    const filtered = filter(text, debug);
    const blocked = filtered !== text;
    
    if (debug) {
      console.log('⚠️  Using fallback regex filter');
    }
    
    return {
      allowed: !blocked,
      blocked: blocked,
      reason: blocked ? 'profanity detected (fallback)' : 'allowed',
      fallback: true
    };
  }
}
```

#### 2.2 Update Chat Server Integration
**File:** `nanos/chat.mjs`

Find the message handling code and replace filter usage:

```javascript
// BEFORE:
import { filter } from "./filter.mjs";
// ...
const filtered = filter(messageText, filterDebug);

// AFTER:
import { filterMessage } from "./judge-client.mjs";
// ...
const judgeResult = await filterMessage(messageText, filterDebug);

if (judgeResult.blocked) {
  // Message blocked by AI filter
  console.log(`⚖️ Message blocked: "${messageText}"`);
  console.log(`   Reason: ${judgeResult.reason}`);
  
  ws.send(JSON.stringify({
    type: "too-long",  // Reuse existing client-side handler
    content: `Message blocked: ${judgeResult.reason}`
  }));
  
  return;
}

// Continue with allowed message
const finalText = messageText;  // Already validated by AI
```

#### 2.3 Add Monitoring Endpoint
**File:** `judge/api-server.mjs`

```javascript
// Track statistics
let stats = {
  totalRequests: 0,
  blocked: 0,
  allowed: 0,
  errors: 0,
  startTime: Date.now()
};

// Add stats endpoint
if (req.method === 'GET' && req.url === '/api/stats') {
  res.writeHead(200, { 'Content-Type': 'application/json' });
  res.end(JSON.stringify({
    ...stats,
    uptime: Date.now() - stats.startTime,
    avgResponseTime: stats.totalRequests > 0 
      ? stats.totalResponseTime / stats.totalRequests 
      : 0
  }));
  return;
}

// Update stats in filterMessage
async function filterMessage(message, onChunk = null) {
  stats.totalRequests++;
  
  try {
    const result = await /* existing filter logic */;
    
    if (result.decision === 't') stats.allowed++;
    if (result.decision === 'f') stats.blocked++;
    
    return result;
  } catch (error) {
    stats.errors++;
    throw error;
  }
}
```

---

### Phase 3: Testing & Rollout

#### 3.1 Testing Checklist

**Local Testing:**
- [ ] Start judge service locally (`node api-server.mjs`)
- [ ] Test single message via curl
- [ ] Test 100 messages via auto-test
- [ ] Verify WebSocket streaming
- [ ] Test MongoDB connection
- [ ] Verify fallback on timeout

**Production Testing:**
- [ ] Deploy to judge.aesthetic.computer
- [ ] Health check: `curl https://judge.aesthetic.computer/api/health`
- [ ] Dashboard access: `https://judge.aesthetic.computer`
- [ ] Filter API: `curl -X POST https://judge.aesthetic.computer/api/filter -d '{"message":"test"}'`
- [ ] Load test: 100 concurrent requests
- [ ] Monitor Ollama memory usage

**Integration Testing:**
- [ ] Deploy updated chat server (one instance first)
- [ ] Test chat message filtering
- [ ] Verify allowed messages pass through
- [ ] Verify blocked messages are caught
- [ ] Test fallback on judge service downtime
- [ ] Monitor latency impact (~1-2s per message)

#### 3.2 Rollout Strategy

**Phase A: Canary Deployment (chat-system + moods)**
- Deploy to `chat-system.aesthetic.computer` first (main chat)
- Deploy to moods chat integration simultaneously
- Monitor for 24 hours
- Metrics: response time, accuracy, error rate

**Phase B: Sotce Chat**
- Deploy to `chat.sotce.net`
- Monitor for 48 hours
- Compare with chat-system metrics

**Phase C: Clock Chat**
- Deploy to `chat-clock.aesthetic.computer`
- Full production monitoring across all instances
- Keep fallback filter active for 1 week

**Phase D: Cleanup**
- Remove old `filter.mjs` imports after 2 weeks
- Update documentation

---

## 🔧 Configuration

### Environment Variables

**Judge Service** (`/opt/judge/.env`):
```bash
# MongoDB
MONGODB_CONNECTION_STRING=mongodb+srv://admin:***@aesthetic.qencn.mongodb.net/
MONGODB_NAME=aesthetic
COLLECTION_NAME=chat-system

# Service
NODE_ENV=production
MODEL=gemma2:2b
PORT=3000
```

**Chat Servers** (add to existing `.env`):
```bash
# Judge service
JUDGE_URL=https://judge.aesthetic.computer
JUDGE_TIMEOUT=2000
JUDGE_FALLBACK=true
```

---

## 📊 Monitoring & Observability

### Key Metrics to Track

1. **Performance:**
   - Average response time (target: <2s)
   - 95th percentile response time
   - Requests per minute
   - Timeout rate

2. **Accuracy:**
   - Messages blocked (count)
   - Messages allowed (count)
   - False positive rate (manual review)
   - Fallback usage rate

3. **Resource Usage:**
   - Ollama RAM usage (should stay ~250MB)
   - CPU usage
   - Disk usage
   - Network bandwidth

4. **Availability:**
   - Uptime percentage
   - Error rate
   - Health check status

### Dashboard Updates

Update `judge/index.html` to show production stats:
- Total messages filtered (all-time)
- Messages filtered today
- Current accuracy rate
- Active connections (WebSocket)

---

## 🚨 Failure Modes & Mitigation

### Scenario 1: Judge Service Down
**Mitigation:** Automatic fallback to regex filter
```javascript
if (judgeResult.fallback) {
  console.warn('⚠️  Using fallback filter due to judge service unavailable');
}
```

### Scenario 2: Slow Response (>2s)
**Mitigation:** Timeout + fallback
```javascript
const JUDGE_TIMEOUT = 2000; // Abort after 2s
```

### Scenario 3: Ollama Out of Memory
**Mitigation:** 
- systemd auto-restart
- Monitor RAM usage
- Alert if >80% memory

### Scenario 4: Too Many False Positives
**Mitigation:**
- Keep fallback filter active for 2 weeks
- Manual review dashboard
- Adjust gemma2:2b prompt if needed

---

## 💰 Cost Analysis

### Infrastructure Costs
- **Judge Droplet:** $6/month (1GB RAM, 1 vCPU)
- **Existing:** No change to chat servers

### Performance Impact
- **Latency:** +1-2s per message (vs instant regex)
- **Accuracy:** 100% explicit blocking (vs ~85% regex)
- **Tradeoff:** Worth it for context-aware filtering

---

## 📚 Documentation Updates

### Files to Update
1. `/judge/README.md` - Add production deployment section
2. `/nanos/README.md` - Document judge integration
3. `/plans/chat-moderation.md` - Archive old filter.mjs approach
4. `/.devcontainer/config.fish` - Add `ac-judge` function

### New Files to Create
1. `/judge/deploy.fish` - Automated deployment script
2. `/judge/judge.service` - systemd service config
3. `/judge/Caddyfile.production` - Production Caddy config
4. `/nanos/judge-client.mjs` - API client library

---

## ✅ Success Criteria

- [ ] Judge service deployed at `judge.aesthetic.computer`
- [ ] Dashboard accessible and showing live stats
- [ ] 100% explicit content blocking (verified)
- [ ] <5% false positive rate (verified via manual review)
- [ ] Average response time <2s
- [ ] 99% uptime over 30 days
- [ ] Fallback filter working on timeouts
- [ ] All 3 chat instances using judge API
- [ ] Old `filter.mjs` deprecated and removed

---

## 🔜 Future Enhancements

1. **Multi-Model Support:**
   - Add qwen2.5:3b for more nuanced filtering
   - A/B test different models
   - Language-specific models

2. **Rate Limiting:**
   - Per-user rate limits
   - IP-based throttling
   - CAPTCHA on suspicious patterns

3. **Analytics:**
   - Blocked message trends
   - Common false positives
   - User-specific patterns
   - Language detection

4. **Admin Tools:**
   - Manual review interface
   - Override blocked messages
   - Whitelist/blacklist management
   - Real-time monitoring dashboard

5. **Edge Deployment:**
   - Deploy judge to Cloudflare Workers
   - Use WebAssembly for Ollama
   - Sub-100ms latency

---

## 📝 Implementation Timeline

**Week 1: Infrastructure**
- Create deployment script
- Set up DigitalOcean droplet
- Deploy judge service
- Configure DNS and HTTPS

**Week 2: Integration**
- Create judge-client.mjs
- Update chat-clock integration
- Test canary deployment

**Week 3: Rollout**
- Deploy to chat-sotce
- Deploy to chat-system
- Monitor metrics

**Week 4: Stabilization**
- Fix issues
- Tune performance
- Update documentation

---

## 🎓 Lessons from /oven Deployment

1. **Use systemd:** Auto-restart is crucial
2. **Caddy simplicity:** Let Caddy handle HTTPS
3. **Health checks:** Always include `/health` endpoint
4. **Environment detection:** Dev vs production URLs
5. **Callback pattern:** Use webhooks for async results
6. **Logging:** Proper log rotation and monitoring
7. **Deploy script:** Automate everything in Fish script

Apply these same patterns to judge deployment for consistency!

---

**End of Plan** 🎯
