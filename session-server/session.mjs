// Session Server, 23.12.04.14.57
// Represents a "room" or user or "client" backend
// which at the moment is run once for every "piece"
// that requests it.

/* #region todo
 + Now
 - [-] Fix live reloading of in-production udp.
 + Done
 - [c] `code.channel` should return a promise, and wait for a
      `code-channel:subbed`.
    event here? This way users get better confirmation if the socket
    doesn't go through or if there is a server issue. 23.07.04.18.01
    (Might not actually be that necessary.)
 - [x] Add `obscenity` filter.
 - [x] Conditional redis sub to dev updates. (Will save bandwidth if extension
       gets lots of use, also would be more secure.)
 - [x] Secure the "code" path to require a special string.
 - [x] Secure the "reload" path (must be in dev mode, sorta okay)
 - [c] Speed up developer reload by using redis pub/sub.
 - [x] Send a signal to everyone once a user leaves.
 - [x] Get "developer" live reloading working again.
 - [x] Add sockets back.
 - [x] Make a "local" option.
 - [x] Read through: https://redis.io/docs/data-types
#endregion */

// Add redis pub/sub here...

import Fastify from "fastify";
import geoip from "geoip-lite";
import { WebSocket, WebSocketServer } from "ws";
import ip from "ip";
import chokidar from "chokidar";
import fs from "fs";
import path from "path";
import crypto from "crypto";
import dotenv from "dotenv";
dotenv.config();

import { exec } from "child_process";

// FCM (Firebase Cloud Messaging)
import { initializeApp, cert } from "firebase-admin/app"; // Firebase notifications.
//import serviceAccount from "./aesthetic-computer-firebase-adminsdk-79w8j-5b5cdfced8.json" assert { type: "json" };
import { getMessaging } from "firebase-admin/messaging";

let serviceAccount;
try {
  const response = await fetch(process.env.GCM_FIREBASE_CONFIG_URL);
  if (!response.ok) {
    throw new Error(`HTTP error! Status: ${response.status}`);
  }
  serviceAccount = await response.json();
} catch (error) {
  console.error("Error fetching service account:", error);
  // Handle the error as needed
}

initializeApp(
  { credential: cert(serviceAccount) }, //,
  //"aesthetic" + ~~performance.now(),
);

import { filter } from "./filter.mjs"; // Profanity filtering.

import { createClient } from "redis";
const redisConnectionString = process.env.REDIS_CONNECTION_STRING;
const dev = process.env.NODE_ENV === "development";

// Dev log file for remote debugging
const DEV_LOG_FILE = path.join(process.cwd(), "../system/public/aesthetic.computer/dev-logs.txt");

const { keys } = Object;
let fastify; //, termkit, term;

if (dev) {
  // Load local ssl certs in development mode.
  fastify = Fastify({
    https: {
      // allowHTTP1: true,
      key: fs.readFileSync("../ssl-dev/localhost-key.pem"),
      cert: fs.readFileSync("../ssl-dev/localhost.pem"),
    },
    logger: true,
  });

  // Import the `terminal-kit` library if dev is true.
  // try {
  //   termkit = (await import("terminal-kit")).default;
  // } catch (err) {
  //   error("Failed to load terminal-kit", error);
  // }
} else {
  fastify = Fastify({ logger: true }); // Still log in production. No reason not to?
}

// Insert `cors` headers as needed. 23.12.19.16.31
// TODO: Is this even necessary?
fastify.options("*", async (req, reply) => {
  const allowedOrigins = [
    "https://aesthetic.local:8888",
    "https://aesthetic.computer",
  ];

  const origin = req.headers.origin;
  log("✈️ Preflight origin:", origin);
  // Check if the incoming origin is allowed
  if (allowedOrigins.includes(origin)) {
    reply.header("Access-Control-Allow-Origin", origin);
  }
  reply.header("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE");
  reply.send();
});

const server = fastify.server;

const DEV_LOG_DIR = "/tmp/dev-logs/";
const deviceLogFiles = new Map(); // Track which devices have log files

// Ensure log directory exists
if (dev) {
  try {
    fs.mkdirSync(DEV_LOG_DIR, { recursive: true });
  } catch (error) {
    console.error("Failed to create dev log directory:", error);
  }
}

const info = {
  port: process.env.PORT, // 8889 in development via `package.json`
  name: process.env.SESSION_BACKEND_ID,
  service: process.env.JAMSOCKET_SERVICE,
};

const codeChannels = {}; // Used to filter `code` updates from redis to
//                          clients who explicitly have the channel set.

// Unified client tracking: each client has handle, user, location, and connection types
const clients = {}; // Map of connection ID to { handle, user, location, websocket: true/false, udp: true/false }

// *** Start up two `redis` clients. (One for subscribing, and for publishing)
const sub = !dev
  ? createClient({ url: redisConnectionString })
  : createClient();
sub.on("error", (err) => log("🔴 Redis subscriber client error!", err));

const pub = !dev
  ? createClient({ url: redisConnectionString })
  : createClient();
pub.on("error", (err) => log("🔴 Redis publisher client error!", err));

try {
  await sub.connect();
  await pub.connect();

  // TODO: This needs to be sent only for a specific user or needs
  //       some kind of special ID.
  await sub.subscribe("code", (message) => {
    const parsed = JSON.parse(message);
    if (codeChannels[parsed.codeChannel]) {
      const msg = pack("code", message, "development");
      subscribers(codeChannels[parsed.codeChannel], msg);
    }
  });

  await sub.subscribe("scream", (message) => {
    everyone(pack("scream", message, "screamer")); // Socket back to everyone.
  });
} catch (err) {
  error("🔴 Could not connect to `redis` instance.");
}

const secret = process.env.GITHUB_WEBHOOK_SECRET;

fastify.post("/update", (request, reply) => {
  const signature = request.headers["x-hub-signature"];
  const hash =
    "sha1=" +
    crypto
      .createHmac("sha1", secret)
      .update(JSON.stringify(request.body))
      .digest("hex");

  if (hash !== signature) {
    reply.status(401).send({ error: "Invalid signature" });
    return;
  }

  // log("Path:", process.env.PATH);

  // Restart service in production.
  // exec(
  //   "cd /home/aesthetic-computer/session-server; pm2 stop all; git pull; npm install; pm2 start all",
  //   (err, stdout, stderr) => {
  //     if (err) {
  //       error(`exec error: ${error}`);
  //       return;
  //     }
  //     log(`stdout: ${stdout}`);
  //     error(`stderr: ${stderr}`);
  //   },
  // );

  reply.send({ status: "ok" });
});

// *** Build Stream - pipe terminal output to WebSocket clients ***
// Available in both dev and production for build progress streaming
fastify.post("/build-stream", async (req) => {
  const line = typeof req.body === 'string' ? req.body : req.body.line || '';
  everyone(pack("build:log", { line, timestamp: Date.now() }));
  return { status: "ok" };
});

fastify.post("/build-status", async (req) => {
  everyone(pack("build:status", { ...req.body, timestamp: Date.now() }));
  return { status: "ok" };
});

// *** Live Reload of Pieces in Development ***
if (dev) {
  fastify.post("/reload", async (req) => {
    everyone(pack("reload", req.body, "pieces"));
    return { msg: "Reload request sent!", body: req.body };
  });
  
  // Jump to a specific piece (navigate)
  fastify.post("/jump", async (req) => {
    const { piece } = req.body;
    
    // Broadcast to all browser clients
    everyone(pack("jump", { piece }, "pieces"));
    
    // Send direct message to VSCode extension clients
    vscodeClients.forEach(client => {
      if (client?.readyState === WebSocket.OPEN) {
        client.send(pack("vscode:jump", { piece }, "vscode"));
      }
    });
    
    return { 
      msg: "Jump request sent!", 
      piece,
      vscodeConnected: vscodeClients.size > 0 
    };
  });
}

// *** HTTP Server Initialization ***
const start = async () => {
  try {
    if (dev) {
      fastify.listen({
        host: "0.0.0.0", // ip.address(),
        port: info.port,
      });
    } else {
      fastify.listen({ host: "0.0.0.0", port: info.port });
    }
  } catch (err) {
    fastify.log.error(err);
    process.exit(1);
  }
};

await start();

// *** Status Page Data Collection ***
// Track UDP channels manually (geckos.io doesn't expose this)
const udpChannels = {};

// Get unified client status - user-centric view
function getClientStatus() {
  const identityMap = new Map(); // Map by identity (handle or user or IP)
  
  // Helper to get identity key for a client
  const getIdentityKey = (client) => {
    // Priority: handle > user > IP (for grouping same person)
    if (client.handle) return `handle:${client.handle}`;
    if (client.user) return `user:${client.user}`;
    if (client.ip) return `ip:${client.ip}`;
    return null;
  };
  
  // Process all WebSocket connections
  Object.keys(connections).forEach((id) => {
    const client = clients[id] || {};
    const ws = connections[id];
    const identityKey = getIdentityKey(client);
    
    if (!identityKey) return; // Skip if no identity info
    
    if (!identityMap.has(identityKey)) {
      identityMap.set(identityKey, {
        handle: client.handle || null,
        location: client.location || null,
        ip: client.ip || null,
        geo: client.geo || null,
        connectionIds: { websocket: [], udp: [] },
        protocols: { websocket: false, udp: false },
        connections: { websocket: [], udp: [] }
      });
    }
    
    const identity = identityMap.get(identityKey);
    
    // Update with latest info
    if (client.handle && !identity.handle) identity.handle = client.handle;
    if (client.location) identity.location = client.location;
    if (client.ip && !identity.ip) identity.ip = client.ip;
    if (client.geo && !identity.geo) identity.geo = client.geo;
    
    identity.connectionIds.websocket.push(parseInt(id));
    identity.protocols.websocket = true;
    identity.connections.websocket.push({
      id: parseInt(id),
      alive: ws.isAlive || false,
      readyState: ws.readyState,
      ping: ws.lastPing || null,
      codeChannel: findCodeChannel(parseInt(id)),
      worlds: getWorldMemberships(parseInt(id))
    });
  });
  
  // Process all UDP connections
  Object.keys(udpChannels).forEach((id) => {
    const client = clients[id] || {};
    const udp = udpChannels[id];
    const identityKey = getIdentityKey(client);
    
    if (!identityKey) return; // Skip if no identity info
    
    if (!identityMap.has(identityKey)) {
      identityMap.set(identityKey, {
        handle: client.handle || null,
        location: client.location || null,
        ip: client.ip || null,
        geo: client.geo || null,
        connectionIds: { websocket: [], udp: [] },
        protocols: { websocket: false, udp: false },
        connections: { websocket: [], udp: [] }
      });
    }
    
    const identity = identityMap.get(identityKey);
    
    // Update with latest info
    if (client.handle && !identity.handle) identity.handle = client.handle;
    if (client.location) identity.location = client.location;
    if (client.ip && !identity.ip) identity.ip = client.ip;
    if (client.geo && !identity.geo) identity.geo = client.geo;
    
    identity.connectionIds.udp.push(id);
    identity.protocols.udp = true;
    identity.connections.udp.push({
      id: id,
      connectedAt: udp.connectedAt,
      state: udp.state || 'unknown'
    });
  });
  
  // Convert to array and add summary info
  return Array.from(identityMap.values()).map(identity => {
    const wsCount = identity.connectionIds.websocket.length;
    const udpCount = identity.connectionIds.udp.length;
    const totalConnections = wsCount + udpCount;
    
    return {
      handle: identity.handle,
      location: identity.location,
      ip: identity.ip,
      geo: identity.geo,
      protocols: identity.protocols,
      connectionCount: {
        websocket: wsCount,
        udp: udpCount,
        total: totalConnections
      },
      // Simplified connection info - just take first of each type for display
      websocket: identity.connections.websocket.length > 0 ? identity.connections.websocket[0] : null,
      udp: identity.connections.udp.length > 0 ? identity.connections.udp[0] : null,
      multipleTabs: totalConnections > 1
    };
  });
}

function getWorldMemberships(connectionId) {
  const worlds = [];
  Object.keys(worldClients).forEach(piece => {
    if (worldClients[piece][connectionId]) {
      worlds.push({
        piece,
        handle: worldClients[piece][connectionId].handle,
        showing: worldClients[piece][connectionId].showing,
        ghost: worldClients[piece][connectionId].ghost || false,
      });
    }
  });
  return worlds;
}

function findCodeChannel(connectionId) {
  for (const [channel, subscribers] of Object.entries(codeChannels)) {
    if (subscribers.has(connectionId)) return channel;
  }
  return null;
}

function getFullStatus() {
  const clients = getClientStatus();
  return {
    timestamp: Date.now(),
    server: {
      uptime: process.uptime(),
      environment: dev ? "development" : "production",
      port: info.port,
    },
    totals: {
      websocket: wss.clients.size,
      udp: Object.keys(udpChannels).length,
      unique_clients: clients.length
    },
    clients: clients,
  };
}


// *** Socket Server Initialization ***
// #region socket
let wss;
let connections = {}; // All active WebSocket connections.
const worldClients = {}; // All connected 🧒 to a space like `field`.

let connectionId = 0; // TODO: Eventually replace with a username arrived at through
//                             a client <-> server authentication function.

wss = new WebSocketServer({ server });
log(
  `🤖 session.aesthetic.computer (${
    dev ? "Development" : "Production"
  }) socket: wss://${ip.address()}:${info.port}`,
);

// *** Status Page Routes (defined after wss initialization) ***
// Status JSON endpoint
fastify.get("/status", async (request, reply) => {
  return getFullStatus();
});

// Status dashboard HTML at root
fastify.get("/", async (request, reply) => {
  reply.type("text/html");
  return `<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8">
  <title>session-server</title>
  <style>
    * { margin: 0; padding: 0; box-sizing: border-box; }
    body {
      font-family: monospace;
      background: #000;
      color: #0f0;
      padding: 2rem;
      line-height: 1.6;
    }
    .header {
      border-bottom: 1px solid #333;
      padding-bottom: 1rem;
      margin-bottom: 2rem;
    }
    .header h1 {
      color: #0ff;
      font-size: 1.2rem;
    }
    .header .status {
      color: #888;
      font-size: 0.9rem;
      margin-top: 0.5rem;
    }
    .client {
      background: #111;
      border-left: 3px solid #0f0;
      padding: 1rem;
      margin-bottom: 1rem;
    }
    .name {
      color: #0ff;
      font-weight: bold;
    }
    .ping { color: yellow; }
    .detail {
      color: #888;
      margin-top: 0.3rem;
      font-size: 0.9rem;
    }
    .empty { color: #555; font-style: italic; }
  </style>
</head>
<body>
  <div class="header">
    <h1>🧩 session-server</h1>
    <div class="status">
      <span id="ws-status">🔴</span> | 
      Uptime: <span id="uptime">--</span> | 
      Online: <span id="client-count">0</span>
    </div>
  </div>
  
  <div id="clients"></div>

  <script>
    const ws = new WebSocket(\`\${location.protocol === 'https:' ? 'wss:' : 'ws:'}//\${location.host}/status-stream\`);
    
    ws.onopen = () => {
      document.getElementById('ws-status').innerHTML = '🟢';
    };
    
    ws.onclose = () => {
      document.getElementById('ws-status').innerHTML = '🔴';
      setTimeout(() => location.reload(), 2000);
    };
    
    ws.onmessage = (event) => {
      const data = JSON.parse(event.data);
      if (data.type === 'status') update(data.data);
    };
    
    function update(s) {
      const hrs = Math.floor(s.server.uptime / 3600);
      const min = Math.floor((s.server.uptime % 3600) / 60);
      document.getElementById('uptime').textContent = \`\${hrs}h \${min}m\`;
      document.getElementById('client-count').textContent = s.totals.unique_clients;
      
      const html = s.clients.length === 0 
        ? '<div class="empty">Nobody online</div>'
        : s.clients.map(c => {
            let out = '<div class="client">';
            out += '<div class="name">';
            out += c.handle || '(anonymous)';
            if (c.multipleTabs && c.connectionCount.total > 1) out += \` (×\${c.connectionCount.total})\`;
            if (c.websocket?.ping) out += \` <span class="ping">(\${c.websocket.ping}ms)</span>\`;
            out += '</div>';
            if (c.location && c.location !== '*keep-alive*') out += \`<div class="detail">📍 \${c.location}</div>\`;
            
            // Show geolocation if available
            if (c.geo) {
              let geo = '🗺️ ';
              if (c.geo.city) geo += c.geo.city + ', ';
              if (c.geo.region) geo += c.geo.region + ', ';
              geo += c.geo.country;
              if (c.geo.timezone) geo += \` (\${c.geo.timezone})\`;
              out += \`<div class="detail">\${geo}</div>\`;
            } else if (c.ip) {
              out += \`<div class="detail">🌐 \${c.ip}</div>\`;
            }
            
            if (c.websocket?.worlds?.length > 0) {
              const w = c.websocket.worlds[0];
              out += \`<div class="detail">🌍 \${w.piece}\`;
              if (w.showing) out += \` (viewing \${w.showing})\`;
              if (w.ghost) out += ' 👻';
              out += '</div>';
            }
            const p = [];
            if (c.protocols.websocket) p.push(c.connectionCount.websocket > 1 ? \`ws×\${c.connectionCount.websocket}\` : 'ws');
            if (c.protocols.udp) p.push(c.connectionCount.udp > 1 ? \`udp×\${c.connectionCount.udp}\` : 'udp');
            if (p.length) out += \`<div class="detail" style="opacity:0.5">\${p.join(' + ')}</div>\`;
            out += '</div>';
            return out;
          }).join('');
      
      document.getElementById('clients').innerHTML = html;
    }
  </script>
</body>
</html>`;
});

// Pack messages into a simple object protocol of `{type, content}`.
function pack(type, content, id) {
  return JSON.stringify({ type, content, id });
}

// Enable ping-pong behavior to keep connections persistently tracked.
// (In the future could just tie connections to logged in users or
// persistent tokens to keep persistence.)
const interval = setInterval(function ping() {
  wss.clients.forEach((client) => {
    if (client.isAlive === false) {
      return client.terminate();
    }
    client.isAlive = false;
    client.pingStart = Date.now(); // Start ping timer
    client.ping();
  });
}, 15000); // 15 second pings from server before termination.

wss.on("close", function close() {
  clearInterval(interval);
  connections = {};
});

// Construct the server.
wss.on("connection", (ws, req) => {
  const connectionInfo = {
    url: req.url,
    host: req.headers.host,
    origin: req.headers.origin,
    userAgent: req.headers['user-agent'],
    remoteAddress: req.socket.remoteAddress,
  };
  log('🔌 WebSocket connection received:', JSON.stringify(connectionInfo, null, 2));
  log('🔌 Total wss.clients.size:', wss.clients.size);
  log('🔌 Current connections count:', Object.keys(connections).length);
  
  // Route status dashboard WebSocket connections separately
  if (req.url === '/status-stream') {
    log('📊 Status dashboard viewer connected from:', req.socket.remoteAddress);
    statusClients.add(ws);
    
    // Mark as dashboard viewer (don't add to game clients)
    ws.isDashboardViewer = true;
    
    // Send initial state
    ws.send(JSON.stringify({
      type: 'status',
      data: getFullStatus(),
    }));
    
    ws.on('close', () => {
      log('📊 Status dashboard viewer disconnected');
      statusClients.delete(ws);
    });
    
    ws.on('error', (err) => {
      error('📊 Status dashboard error:', err);
      statusClients.delete(ws);
    });
    
    return; // Don't process as a game client
  }
  
  log('🎮 Game client connection detected, adding to connections');
  
  // Regular game client connection handling below
  const ip = req.socket.remoteAddress || "localhost"; // beautify ip
  ws.isAlive = true; // For checking persistence between ping-pong messages.
  ws.pingStart = null; // Track ping timing
  ws.lastPing = null; // Store last measured ping

  ws.on("pong", () => {
    ws.isAlive = true;
    if (ws.pingStart) {
      ws.lastPing = Date.now() - ws.pingStart;
      ws.pingStart = null;
    }
  }); // Receive a pong and stay alive!

  // Assign the conection a unique id.
  connections[connectionId] = ws;
  const id = connectionId;
  let codeChannel; // Used to subscribe to incoming piece code.
  
  // Initialize client record with IP and geolocation
  if (!clients[id]) clients[id] = {};
  clients[id].websocket = true;
  
  // Clean IP and get geolocation
  const cleanIp = ip.replace('::ffff:', '');
  clients[id].ip = cleanIp;
  
  const geo = geoip.lookup(cleanIp);
  if (geo) {
    clients[id].geo = {
      country: geo.country,
      region: geo.region,
      city: geo.city,
      timezone: geo.timezone,
      ll: geo.ll // [latitude, longitude]
    };
    log(`🌍 Geolocation for ${cleanIp}:`, geo.country, geo.region, geo.city);
  } else {
    log(`🌍 No geolocation data for ${cleanIp}`);
  }

  log("🧏 Someone joined:", `${id}:${ip}`, "Online:", wss.clients.size, "🫂");
  log("🎮 Added to connections. Total game clients:", Object.keys(connections).length);

  const content = { id, playerCount: wss.clients.size };

  // Send a message to all other clients except this one.
  function others(string) {
    wss.clients.forEach((c) => {
      if (c !== ws && c?.readyState === WebSocket.OPEN) c.send(string);
    });
  }

  // Send a self-connection message back to the client.
  ws.send(
    pack(
      "connected",
      JSON.stringify({ ip, playerCount: content.playerCount }),
      id,
    ),
  );

  // Send a join message to everyone else.
  others(
    pack(
      "joined",
      JSON.stringify({
        text: `${connectionId} has joined. Connections open: ${content.playerCount}`,
      }),
      id,
    ),
  );

  connectionId += 1;

  // Relay all incoming messages from this client to everyone else.
  ws.on("message", (data) => {
    // Parse incoming message and attach client identifier.
    let msg;
    try {
      msg = JSON.parse(data.toString());
    } catch (error) {
      console.error("📚 Failed to parse JSON:", error);
      return;
    }

    msg.id = id; // TODO: When sending a server generated message, use a special id.

    // Extract user identity and handle from ANY message that contains it
    if (msg.content?.user?.sub) {
      if (!clients[id]) clients[id] = { websocket: true };
      
      const userSub = msg.content.user.sub;
      const userChanged = !clients[id].user || clients[id].user !== userSub;
      
      if (userChanged) {
        clients[id].user = userSub;
        log("🔑 User identity from", msg.type + ":", userSub.substring(0, 20) + "...", "conn:", id);
      }
      
      // Extract handle from message if present (e.g., location:broadcast includes it)
      if (msg.content.handle && (!clients[id].handle || clients[id].handle !== msg.content.handle)) {
        clients[id].handle = msg.content.handle;
        log("✅ Handle from message:", msg.content.handle, "conn:", id);
      }
    }

    if (msg.type === "scream") {
      // Alert all connected users via redis pub/sub to the scream.
      log("😱 About to scream...");
      const out = filter(msg.content);
      pub
        .publish("scream", out)
        .then((result) => {
          log("😱 Scream succesfully published:", result);

          let piece = "";
          if (out.indexOf("pond") > -1) piece = "pond";
          else if (out.indexOf("field") > -1) piece = "field";

          //if (!dev) {
          getMessaging()
            .send({
              notification: {
                title: "😱 Scream",
                body: out, //,
              },
              // android: {
              //   notification: {
              //     imageUrl: "https://aesthetic.computer/api/logo.png",
              //   },
              apns: {
                payload: {
                  aps: {
                    "mutable-content": 1,
                    "interruption-level": "time-sensitive", // Marks as time-sensitive
                    priority: 10, // Highest priority
                    "content-available": 1, // Tells iOS to wake the app
                  },
                },
                headers: {
                  "apns-priority": "10", // Immediate delivery priority
                  "apns-push-type": "alert", // Explicit push type
                  "apns-expiration": "0", // Message won't be stored by APNs
                },
                fcm_options: {
                  image: "https://aesthetic.computer/api/logo.png",
                },
              },
              webpush: {
                headers: {
                  image: "https://aesthetic.computer/api/logo.png",
                },
              },
              topic: "scream",
              data: { piece },
            })
            .then((response) => {
              log("☎️  Successfully sent notification:", response);
            })
            .catch((error) => {
              log("📵  Error sending notification:", error);
            });
          //}
        })
        .catch((error) => {
          log("🙅‍♀️ Error publishing scream:", error);
        });
      // Send a notification to all devices subscribed to the `scream` topic.
    } else if (msg.type === "code-channel:sub") {
      // Filter code-channel updates based on this user.
      codeChannel = msg.content;
      if (!codeChannels[codeChannel]) codeChannels[codeChannel] = new Set();
      codeChannels[codeChannel].add(id);
    } else if (msg.type === "login") {
      if (msg.content?.user?.sub) {
        if (!clients[id]) clients[id] = { websocket: true };
        clients[id].user = msg.content.user.sub;
        
        // Fetch the user's handle from the API
        const userSub = msg.content.user.sub;
        log("🔑 Login attempt for user:", userSub.substring(0, 20) + "...", "connection:", id);
        
        fetch(`https://aesthetic.computer/handle/${encodeURIComponent(userSub)}`)
          .then(response => {
            log("📡 Handle API response status:", response.status, "for", userSub.substring(0, 20) + "...");
            return response.json();
          })
          .then(data => {
            log("📦 Handle API data:", JSON.stringify(data), "for connection:", id);
            if (data.handle) {
              clients[id].handle = data.handle;
              log("✅ User logged in:", data.handle, `(${userSub.substring(0, 12)}...)`, "connection:", id);
            } else {
              log("⚠️  User logged in (no handle in response):", userSub.substring(0, 12), "..., connection:", id);
            }
          })
          .catch(err => {
            log("❌ Failed to fetch handle for:", userSub.substring(0, 20) + "...", "Error:", err.message);
          });
      }
    } else if (msg.type === "identify") {
      // VSCode extension identifying itself
      if (msg.content?.type === "vscode") {
        vscodeClients.add(ws);
        log("✅ VSCode extension connected, conn:", id);
        
        // Send confirmation
        ws.send(pack("identified", { type: "vscode", id }, id));
      }
    } else if (msg.type === "location:broadcast") {      /*
      sub
        .subscribe(`logout:broadcast:${msg.content.user.sub}`, () => {
          ws.send(pack(`logout:broadcast:${msg.content.user.sub}`, true, id));
        })
        .then(() => {
          log("🏃 Subscribed to logout updates from:", msg.content.user.sub);
        })
        .catch((err) =>
          error(
            "🏃 Could not unsubscribe from logout:broadcast for:",
            msg.content.user.sub,
            err,
          ),
        );
        */
    } else if (msg.type === "logout:broadcast:subscribe") {
      /*
      console.log("Logout broadcast:", msg.type, msg.content);
      pub
        .publish(`logout:broadcast:${msg.content.user.sub}`, "true")
        .then((result) => {
          console.log("🏃 Logout broadcast successful for:", msg.content);
        })
        .catch((error) => {
          log("🙅‍♀️ Error publishing logout:", error);
        });
        */
    } else if (msg.type === "location:broadcast") {
      // Receive a slug location for this handle.
      if (msg.content.slug !== "*keep-alive*") {
        log("🗼 Location:", msg.content.slug, "Handle:", msg.content.handle, "ID:", id);
      }
      
      // Store handle and location for this client
      if (!clients[id]) clients[id] = { websocket: true };
      
      // Extract user identity from message
      if (msg.content?.user?.sub) {
        clients[id].user = msg.content.user.sub;
      }
      
      // Extract handle directly from message
      if (msg.content.handle) {
        clients[id].handle = msg.content.handle;
      }
      
      // Extract and store location
      if (msg.content.slug) {
        // Don't overwrite location with keep-alive
        if (msg.content.slug !== "*keep-alive*") {
          clients[id].location = msg.content.slug;
          log(`📍 Location updated for ${clients[id].handle || id}: "${msg.content.slug}"`);
        } else {
          log(`💓 Keep-alive from ${clients[id].handle || id}, location unchanged`);
        }
      }

      // Publish to redis...
      pub
        .publish("slug:" + msg.content.handle, msg.content.slug)
        .then((result) => {
          if (msg.content.slug !== "*keep-alive*") {
            log(
              "🐛 Slug succesfully published for:",
              msg.content.handle,
              msg.content.slug,
            );
          }
        })
        .catch((error) => {
          log("🙅‍♀️ Error publishing slug:", error);
        });

      // TODO: - [] When a user is ghosted, then subscribe to their location
      //            updates.
      //       - [] And stop subscribing when they are unghosted.
    } else if (msg.type === "dev-log" && dev) {
      // Create device-specific log files and only notify in terminal
      const timestamp = new Date().toISOString();
      const deviceId = `client-${id}`;
      const logFileName = `${DEV_LOG_DIR}${deviceId}.log`;
      
      // Check if this is a new device
      if (!deviceLogFiles.has(deviceId)) {
        deviceLogFiles.set(deviceId, logFileName);
        console.log(`📱 New device logging: ${deviceId} -> ${logFileName}`);
        console.log(`   tail -f ${logFileName}`);
      }
      
      // Write to device-specific log file
      const logEntry = `[${timestamp}] ${msg.content.level || 'LOG'}: ${msg.content.message}\n`;
      
      try {
        fs.appendFileSync(logFileName, logEntry);
      } catch (error) {
        console.error(`Failed to write to ${logFileName}:`, error);
      }
    } else {
      // 🗺️ World Messages
      // TODO: Should all messages be prefixed with their piece?

      // Filter for `world:${piece}:${label}` type messages.
      if (msg.type.startsWith("world:")) {
        const parsed = msg.type.split(":");
        const piece = parsed[1];
        const label = parsed.pop();

        // TODO: Store client position on disconnect, based on their handle.

        if (label === "show") {
          // Store any existing show picture in clients list.
          worldClients[piece][id].showing = msg.content;
        }

        if (label === "hide") {
          // Store any existing show picture in clients list.
          worldClients[piece][id].showing = null;
        }

        // Intercept chats and filter them.
        if (label === "write") {
          msg.content = filter(msg.content);
        }

        if (label === "join") {
          if (!worldClients[piece]) worldClients[piece] = {};

          // Check to see if the client handle matches and a connection can
          // be reassociated.

          let pickedUpConnection = false;
          keys(worldClients[piece]).forEach((clientID) => {
            // TODO: Break out of this loop early.
            const client = worldClients[piece][clientID];
            if (
              client["handle"].startsWith("@") &&
              client["handle"] === msg.content.handle &&
              client.ghosted
            ) {
              // log("👻 Ghosted?", client);

              log(
                "👻 Unghosting:",
                msg.content.handle,
                "old id:",
                clientID,
                "new id:",
                id,
              );
              pickedUpConnection = true;

              client.ghosted = false;

              sub
                .unsubscribe("slug:" + msg.content.handle)
                .then(() => {
                  log("🐛 Unsubscribed from slug for:", msg.content.handle);
                })
                .catch((err) => {
                  error(
                    "🐛 Could not unsubscribe from slug for:",
                    msg.content.handle,
                    err,
                  );
                });

              delete worldClients[piece][clientID];

              ws.send(pack(`world:${piece}:list`, worldClients[piece], id));

              // Replace the old client with the new data.
              worldClients[piece][id] = { ...msg.content };
            }
          });

          if (!pickedUpConnection)
            ws.send(pack(`world:${piece}:list`, worldClients[piece], id));

          // ❤️‍🔥 TODO: No need to send the current user back via `list` here.
          if (!pickedUpConnection) worldClients[piece][id] = { ...msg.content };

          // ^ Send existing list to just this user.

          others(JSON.stringify(msg)); // Alert everyone else about the join.

          log("🧩 Clients in piece:", piece, worldClients[piece]);
          return;
        } else if (label === "move") {
          // log("🚶‍♂️", piece, msg.content);
          if (typeof worldClients?.[piece]?.[id] === "object")
            worldClients[piece][id].pos = msg.content.pos;
        } else {
          log(`${label}:`, msg.content);
        }

        if (label === "persist") {
          log("🧮 Persisting this client...", msg.content);
        }

        // All world: messages are only broadcast to "others", with the
        // exception of "write" with relays the filtered message back:
        if (label === "write") {
          everyone(JSON.stringify(msg));
        } else {
          others(JSON.stringify(msg));
        }
        return;
      }

      everyone(JSON.stringify(msg)); // Relay any other message to every user.
    }
  });

  // More info: https://stackoverflow.com/a/49791634/8146077
  ws.on("close", () => {
    log("🚪 Someone left:", id, "Online:", wss.clients.size, "🫂");

    // Remove from VSCode clients if present
    vscodeClients.delete(ws);

    // Delete the user from the worldClients pieces index.
    // keys(worldClients).forEach((piece) => {
    //   delete worldClients[piece][id];
    //   if (keys(worldClients[piece]).length === 0)
    //     delete worldClients[piece];
    // });

    if (clients[id]?.user) {
      const userSub = clients[id].user;
      sub
        .unsubscribe("logout:broadcast:" + userSub)
        .then(() => {
          log("🏃 Unsubscribed from logout:broadcast for:", userSub);
        })
        .catch((err) => {
          error(
            "🏃 Could not unsubscribe from logout:broadcast for:",
            userSub,
            err,
          );
        });
    }

    // Send a message to everyone else on the server that this client left.

    let ghosted = false;

    keys(worldClients).forEach((piece) => {
      if (worldClients[piece][id]) {
        // Turn this client into a ghost, unless it's the last one in the
        // world region.
        if (
          worldClients[piece][id].handle.startsWith("@") &&
          keys(worldClients[piece]).length > 1
        ) {
          const handle = worldClients[piece][id].handle;
          log("👻 Ghosted:", handle);
          log("World clients after ghosting:", worldClients[piece]);
          worldClients[piece][id].ghost = true;
          ghosted = true;

          function kick() {
            log("👢 Kicked:", handle, id);
            clearTimeout(kickTimer);
            sub
              .unsubscribe("slug:" + handle)
              .then(() => {
                log("🐛 Unsubscribed from slug for:", handle);
              })
              .catch((err) => {
                error("🐛 Could not unsubscribe from slug for:", handle, err);
              });
            // Delete the user from the worldClients pieces index.
            delete worldClients[piece][id];
            if (keys(worldClients[piece]).length === 0)
              delete worldClients[piece];
            everyone(pack(`world:${piece}:kick`, {}, id)); // Kick this ghost.
          }

          let kickTimer = setTimeout(kick, 5000);

          const worlds = ["field", "horizon"]; // Whitelist for worlds...
          // This could eventually be communicated based on a parameter in
          // the subscription? 24.03.09.15.05

          // Subscribe to slug updates from redis.
          sub
            .subscribe("slug:" + handle, (slug) => {
              if (slug !== "*keep-alive*") {
                log(`🐛 ${handle} is now in:`, slug);
                if (!worlds.includes(slug))
                  everyone(pack(`world:${piece}:slug`, { handle, slug }, id));
              }

              if (worlds.includes(slug)) {
                kick();
              } else {
                clearTimeout(kickTimer);
                kickTimer = setTimeout(kick, 5000);
              }
              // Whitelist slugs here
            })
            .then(() => {
              log("🐛 Subscribed to slug updates from:", handle);
            })
            .catch((err) =>
              error("🐛 Could not subscribe to slug for:", handle, err),
            );

          // Send a message to everyone on the server that this client is a ghost.
          everyone(pack(`world:${piece}:ghost`, {}, id));
        } else {
          // Delete the user from the worldClients pieces index.
          delete worldClients[piece][id];
          if (keys(worldClients[piece]).length === 0)
            delete worldClients[piece];
        }
      }
    });

    // Send a message to everyone else on the server that this client left.
    if (!ghosted) everyone(pack("left", { count: wss.clients.size }, id));

    // Delete from the connection index.
    delete connections[id];
    
    // Clean up client record if no longer connected via any protocol
    if (clients[id]) {
      clients[id].websocket = false;
      // If also not connected via UDP, delete the client record entirely
      if (!udpChannels[id]) {
        delete clients[id];
      }
    }

    // Clear out the codeChannel if the last user disconnects from it.
    if (codeChannel !== undefined) {
      codeChannels[codeChannel]?.delete(id);
      if (codeChannels[codeChannel]?.values().length === 0) {
        delete codeChannels[codeChannel];
      }
    }
  });
});

// Sends a message to all connected clients.
function everyone(string) {
  wss.clients.forEach((c) => {
    if (c?.readyState === WebSocket.OPEN) c.send(string);
  });
}

// Sends a message to a particular set of client ids on
// this instance that are part of the `subs` Set.
function subscribers(subs, msg) {
  subs.forEach((connectionId) => {
    connections[connectionId]?.send(msg);
  });
}
// #endregion

// *** Status WebSocket Stream ***
// Track status dashboard clients (separate from game clients)
const statusClients = new Set();

// *** VSCode Extension Clients ***
// Track VSCode extension clients for direct jump message routing
const vscodeClients = new Set();

// Broadcast status updates every 2 seconds
setInterval(() => {
  if (statusClients.size > 0) {
    const status = getFullStatus();
    statusClients.forEach(client => {
      if (client.readyState === WebSocket.OPEN) {
        try {
          client.send(JSON.stringify({ type: 'status', data: status }));
        } catch (err) {
          error('📊 Failed to send status update:', err);
        }
      }
    });
  }
}, 2000);

// 🧚 UDP Server (using Twilio ICE servers)
// #endregion udp

// Note: This currently works off of a monolith via `udp.aesthetic.computer`
//       as the ports are blocked on jamsocket.

import geckos from "@geckos.io/server";

const io = geckos();

io.addServer(server); // Hook up to the HTTP Server.
// io.listen(9208); // default port is 9208

io.onConnection((channel) => {
  // Track this UDP channel
  udpChannels[channel.id] = {
    connectedAt: Date.now(),
    state: channel.webrtcConnection.state,
    user: null,
    handle: null
  };
  
  // Get IP address from channel
  const udpIp = channel.userData?.address || channel.remoteAddress || null;
  
  log(`🩰 UDP ${channel.id} connected from:`, udpIp || 'unknown');
  
  // Initialize client record with IP
  if (!clients[channel.id]) clients[channel.id] = { udp: true };
  if (udpIp) {
    const cleanIp = udpIp.replace('::ffff:', '');
    clients[channel.id].ip = cleanIp;
    
    // Get geolocation for UDP client
    const geo = geoip.lookup(cleanIp);
    if (geo) {
      clients[channel.id].geo = {
        country: geo.country,
        region: geo.region,
        city: geo.city,
        timezone: geo.timezone,
        ll: geo.ll
      };
      log(`🌍 UDP ${channel.id} geolocation:`, geo.city || geo.country);
    }
  }
  
  // Set a timeout to warn about missing identity
  setTimeout(() => {
    if (!clients[channel.id]?.user && !clients[channel.id]?.handle) {
      log(`⚠️  UDP ${channel.id} has been connected for 10s but hasn't sent identity message`);
    }
  }, 10000);
  
  // Handle identity message
  channel.on("udp:identity", (data) => {
    try {
      const identity = JSON.parse(data);
      log(`🩰 UDP ${channel.id} sent identity:`, JSON.stringify(identity).substring(0, 100));
      
      // Initialize client record if needed
      if (!clients[channel.id]) clients[channel.id] = { udp: true };
      
      // Extract user identity
      if (identity.user?.sub) {
        clients[channel.id].user = identity.user.sub;
        log(`🩰 UDP ${channel.id} user:`, identity.user.sub.substring(0, 20) + "...");
      }
      
      // Extract handle directly from identity message
      if (identity.handle) {
        clients[channel.id].handle = identity.handle;
        log(`✅ UDP ${channel.id} handle: "${identity.handle}"`);
      }
    } catch (e) {
      error(`🩰 Failed to parse identity for ${channel.id}:`, e);
    }
  });
  
  channel.onDisconnect(() => {
    log(`🩰 ${channel.id} got disconnected`);
    delete udpChannels[channel.id];
    
    // Clean up client record if no longer connected via any protocol
    if (clients[channel.id]) {
      clients[channel.id].udp = false;
      // If also not connected via WebSocket, delete the client record entirely
      if (!connections[channel.id]) {
        delete clients[channel.id];
      }
    }
    
    channel.close();
  });

  // 💎 TODO: Make these channel names programmable somehow? 24.12.08.04.12

  channel.on("tv", (data) => {
    if (channel.webrtcConnection.state === "open") {
      try {
        channel.room.emit("tv", data);
      } catch (err) {
        console.warn("Broadcast error:", err);
      }
    } else {
      console.log(channel.webrtcConnection.state);
    }
  });

  // Just for testing via the aesthetic `udp` piece for now.
  channel.on("fairy:point", (data) => {
    // See docs here: https://github.com/geckosio/geckos.io#reliable-messages
    // TODO: - [] Learn about the differences between channels and rooms.

    // log(`🩰 fairy:point - ${data}`);
    if (channel.webrtcConnection.state === "open") {
      try {
        channel.broadcast.emit("fairy:point", data);
        // ^ emit the to all channels in the same room except the sender
      } catch (err) {
        console.warn("Broadcast error:", err);
      }
    } else {
      console.log(channel.webrtcConnection.state);
    }
  });
});

// #endregion

// 🚧 File Watching in Local Development Mode
// File watching uses: https://github.com/paulmillr/chokidar
if (dev) {
  // 1. Watch for local file changes in pieces.
  chokidar
    .watch("../system/public/aesthetic.computer/disks")
    .on("all", (event, path) => {
      if (event === "change") {
        const piece = path
          .split("/")
          .pop()
          .replace(/\.mjs|\.lisp$/, "");
        everyone(pack("reload", { piece: piece || "*" }, "local"));
      }
    });  // 2. Watch base system files.
  chokidar
    .watch([
      "../system/netlify/functions",
      "../system/public/privacy-policy.html",
      "../system/public/aesthetic-direct.html",
      "../system/public/aesthetic.computer/lib",
      "../system/public/aesthetic.computer/systems", // This doesn't need a full reload / could just reload the disk module?
      "../system/public/aesthetic.computer/boot.mjs",
      "../system/public/aesthetic.computer/bios.mjs",
      "../system/public/aesthetic.computer/style.css",
    ])
    .on("all", (event, path) => {
      if (event === "change")
        everyone(pack("reload", { piece: "*refresh*" }, "local"));
    });

  // 2b. Watch prompt files separately (piece reload instead of full refresh)
  chokidar
    .watch("../system/public/aesthetic.computer/prompts")
    .on("all", (event, path) => {
      if (event === "change") {
        const filename = path.split("/").pop();
        console.log(`🎨 Prompt file changed: ${filename}`);
        everyone(pack("reload", { piece: "*piece-reload*" }, "local"));
      }
    });

  // 3. Watch vscode extension
  chokidar.watch("../vscode-extension/out").on("all", (event, path) => {
    if (event === "change")
      everyone(pack("vscode-extension:reload", { reload: true }, "local"));
  });
}

/*
if (termkit) {
  term = termkit.terminal;

  const doc = term.createDocument({
    palette: new termkit.Palette(),
  });

  // Create left (log) and right (client list) columns
  const leftColumn = new termkit.Container({
    parent: doc,
    x: 0,
    width: "70%",
    height: "100%",
  });

  const rightColumn = new termkit.Container({
    parent: doc,
    x: "70%",
    width: "30%",
    height: "100%",
  });

  term.grabInput();

  console.log("grabbed input");

  term.on("key", function (name, matches, data) {
    console.log("'key' event:", name);

    // Detect CTRL-C and exit 'manually'
    if (name === "CTRL_C") {
      process.exit();
    }
  });

  term.on("mouse", function (name, data) {
    console.log("'mouse' event:", name, data);
  });

  // Log box in the left column
  const logBox = new termkit.TextBox({
    parent: leftColumn,
    content: "Your logs will appear here...\n",
    scrollable: true,
    vScrollBar: true,
    x: 0,
    y: 0,
    width: "100%",
    height: "100%",
    mouse: true, // to allow mouse interactions if needed
  });

  // Static list box in the right column
  const clientList = new termkit.TextBox({
    parent: rightColumn,
    content: "Client List:\n",
    x: 0,
    y: 0,
    width: "100%",
    height: "100%",
  });

  // Example functions to update contents
  function addLog(message) {
    logBox.setContent(logBox.getContent() + message + "\n");
    // logBox.scrollBottom();
    doc.draw();
  }

  function updateClientList(clients) {
    clientList.setContent("Client List:\n" + clients.join("\n"));
    doc.draw();
  }

  // Example usage
  addLog("Server started...");
  updateClientList(["Client1", "Client2"]);

  // Handle input for graceful exit
  // term.grabInput();
  // term.on("key", (key) => {
  // if (key === "CTRL_C") {
  // process.exit();
  // }
  // });

  // doc.draw();
}
*/

function log() {
  console.log(...arguments);
}

function error() {
  console.error(...arguments);
}
