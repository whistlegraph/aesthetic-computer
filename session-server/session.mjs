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
import geckos from "@geckos.io/server";
import geoip from "geoip-lite";
import { WebSocket, WebSocketServer } from "ws";
import ip from "ip";
import chokidar from "chokidar";
import fs from "fs";
import path from "path";
import crypto from "crypto";
import dotenv from "dotenv";
dotenv.config();

// Module streaming - path to public directory
const PUBLIC_DIR = path.resolve(process.cwd(), "../system/public/aesthetic.computer");

// Module hash cache (invalidated on file change)
const moduleHashes = new Map(); // path -> { hash, content, mtime }

// Compute hash for a module file
function getModuleHash(modulePath) {
  const fullPath = path.join(PUBLIC_DIR, modulePath);
  try {
    const stats = fs.statSync(fullPath);
    const cached = moduleHashes.get(modulePath);
    
    // Return cached if mtime matches
    if (cached && cached.mtime === stats.mtimeMs) {
      return cached;
    }
    
    // Read and hash
    const content = fs.readFileSync(fullPath, "utf8");
    const hash = crypto.createHash("sha256").update(content).digest("hex").slice(0, 16);
    const entry = { hash, content, mtime: stats.mtimeMs };
    moduleHashes.set(modulePath, entry);
    return entry;
  } catch (err) {
    return null;
  }
}

// Error logging ring buffer (for dashboard display)
const errorLog = [];
const MAX_ERRORS = 50;
const ERROR_RETENTION_MS = 60 * 60 * 1000; // 1 hour

function logError(level, message) {
  const entry = {
    level,
    message: typeof message === 'string' ? message : JSON.stringify(message),
    timestamp: new Date().toISOString()
  };
  errorLog.push(entry);
  if (errorLog.length > MAX_ERRORS) errorLog.shift();
}

// Capture uncaught errors
process.on('uncaughtException', (err) => {
  logError('error', `Uncaught: ${err.message}`);
  console.error('Uncaught Exception:', err);
});

process.on('unhandledRejection', (reason, promise) => {
  logError('error', `Unhandled Rejection: ${reason}`);
  console.error('Unhandled Rejection:', reason);
});

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

// Initialize ChatManager for multi-instance chat support
const chatManager = new ChatManager({ dev: process.env.NODE_ENV === "development" });
await chatManager.init();

// Helper function to get handles of users currently on a specific piece
// Used by chatManager to determine who's actually viewing the chat piece
function getHandlesOnPiece(pieceName) {
  const handles = [];
  for (const [id, client] of Object.entries(clients)) {
    if (client.location === pieceName && client.handle) {
      handles.push(client.handle);
    }
  }
  return [...new Set(handles)]; // Remove duplicates
}

// Expose the function to chatManager
chatManager.setPresenceResolver(getHandlesOnPiece);

import { filter } from "./filter.mjs"; // Profanity filtering.
import { ChatManager } from "./chat-manager.mjs"; // Multi-instance chat support.

// *** SockLogs - Remote console log forwarding from devices ***
// Devices with ?socklogs param send logs via WebSocket
// Viewers (CLI or web) can subscribe to see device logs in real-time
const socklogsDevices = new Map(); // deviceId -> { ws, lastLog, logCount }
const socklogsViewers = new Set(); // Set of viewer WebSockets

function socklogsBroadcast(deviceId, logEntry) {
  const message = JSON.stringify({
    type: 'log',
    deviceId,
    ...logEntry,
    serverTime: Date.now()
  });
  for (const viewer of socklogsViewers) {
    if (viewer.readyState === WebSocket.OPEN) {
      viewer.send(message);
    }
  }
}

function socklogsStatus() {
  return {
    devices: Array.from(socklogsDevices.entries()).map(([id, info]) => ({
      deviceId: id,
      logCount: info.logCount,
      lastLog: info.lastLog,
      connectedAt: info.connectedAt
    })),
    viewerCount: socklogsViewers.size
  };
}

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
const codeChannelState = {}; // Store last code sent to each channel for late joiners

// DAW channel for M4L device ↔ IDE communication
const dawDevices = new Set(); // Connection IDs of /device instances
const dawIDEs = new Set(); // Connection IDs of IDE instances in Ableton mode

// Unified client tracking: each client has handle, user, location, and connection types
const clients = {}; // Map of connection ID to { handle, user, location, websocket: true/false, udp: true/false }

// Device naming for local dev (persisted to file)
const DEVICE_NAMES_FILE = path.join(process.cwd(), "../.device-names.json");
let deviceNames = {}; // Map of IP -> { name, group }
function loadDeviceNames() {
  try {
    if (fs.existsSync(DEVICE_NAMES_FILE)) {
      deviceNames = JSON.parse(fs.readFileSync(DEVICE_NAMES_FILE, 'utf8'));
      log("📱 Loaded device names:", Object.keys(deviceNames).length);
    }
  } catch (e) {
    log("📱 Could not load device names:", e.message);
  }
}
function saveDeviceNames() {
  try {
    fs.writeFileSync(DEVICE_NAMES_FILE, JSON.stringify(deviceNames, null, 2));
  } catch (e) {
    log("📱 Could not save device names:", e.message);
  }
}
if (dev) loadDeviceNames();

// Get the dev host machine name
import os from "os";
const DEV_HOST_NAME = os.hostname();
const DEV_LAN_IP = (() => {
  // First, try to read from /tmp/host-lan-ip (written by entry.fish in devcontainer)
  try {
    const hostIpFile = '/tmp/host-lan-ip';
    if (fs.existsSync(hostIpFile)) {
      const ip = fs.readFileSync(hostIpFile, 'utf-8').trim();
      if (ip && ip.match(/^\d+\.\d+\.\d+\.\d+$/)) {
        console.log(`🖥️ Using host LAN IP from ${hostIpFile}: ${ip}`);
        return ip;
      }
    }
  } catch (e) { /* ignore */ }
  
  // Fallback: try to detect from network interfaces
  const interfaces = os.networkInterfaces();
  for (const name of Object.keys(interfaces)) {
    for (const iface of interfaces[name]) {
      if (iface.family === 'IPv4' && !iface.internal && iface.address.startsWith('192.168.')) {
        return iface.address;
      }
    }
  }
  return null;
})();
console.log(`🖥️ Dev host: ${DEV_HOST_NAME}, LAN IP: ${DEV_LAN_IP || 'N/A'}`);

// Helper: Assign device letters (A, B, C...) based on connection order
function getDeviceLetter(connectionId) {
  // Get sorted list of connection IDs
  const sortedIds = Object.keys(connections)
    .map(id => parseInt(id))
    .sort((a, b) => a - b);
  const index = sortedIds.indexOf(parseInt(connectionId));
  if (index === -1) return '?';
  // A=65, B=66, etc. Wrap around after Z
  return String.fromCharCode(65 + (index % 26));
}

// Helper: Find connections by ID, IP, handle, or device letter
function targetClients(target) {
  if (target === 'all') {
    return Object.entries(connections)
      .filter(([id, ws]) => ws?.readyState === WebSocket.OPEN)
      .map(([id, ws]) => ({ id: parseInt(id), ws }));
  }
  
  const results = [];
  for (const [id, ws] of Object.entries(connections)) {
    const client = clients[id];
    const cleanTarget = target.replace('@', '');
    const cleanIp = client?.ip?.replace('::ffff:', '');
    const deviceLetter = getDeviceLetter(id);
    
    if (
      String(id) === String(target) ||
      cleanIp === target ||
      client?.handle === `@${cleanTarget}` ||
      client?.handle === cleanTarget ||
      deviceNames[cleanIp]?.name?.toLowerCase() === target.toLowerCase() ||
      deviceLetter.toLowerCase() === target.toLowerCase() // Match by letter (A, B, C...)
    ) {
      if (ws?.readyState === WebSocket.OPEN) {
        results.push({ id: parseInt(id), ws });
      }
    }
  }
  return results;
}

// *** Start up two `redis` clients. (One for subscribing, and for publishing)
const sub = !dev
  ? createClient({ url: redisConnectionString })
  : createClient();
sub.on("error", (err) => {
  log("🔴 Redis subscriber client error!", err);
  logError('error', `Redis sub: ${err.message}`);
});

const pub = !dev
  ? createClient({ url: redisConnectionString })
  : createClient();
pub.on("error", (err) => {
  log("🔴 Redis publisher client error!", err);
  logError('error', `Redis pub: ${err.message}`);
});

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

// *** Robots.txt - prevent crawling ***
fastify.get("/robots.txt", async (req, reply) => {
  reply.type("text/plain");
  return "User-agent: *\nDisallow: /";
});

// *** Module HTTP endpoint - serve modules directly (bypasses Netlify proxy) ***
// Used by boot.mjs on localhost when the main proxy is flaky
fastify.get("/module/*", async (req, reply) => {
  const modulePath = req.params["*"];
  const moduleData = getModuleHash(modulePath);
  
  if (moduleData) {
    reply
      .header("Content-Type", "application/javascript; charset=utf-8")
      .header("Access-Control-Allow-Origin", "*")
      .header("Cache-Control", "no-cache")
      .send(moduleData.content);
  } else {
    reply.status(404).send({ error: "Module not found", path: modulePath });
  }
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

// *** FF1 Art Computer Proxy ***
// Proxies displayPlaylist commands to FF1 via direct connection or cloud relay
const FF1_RELAY_URL = "https://artwork-info.feral-file.workers.dev/api/cast";

// Load FF1 config from machines.json
function getFF1Config() {
  try {
    const machinesPath = path.resolve(process.cwd(), "../aesthetic-computer-vault/machines.json");
    const machines = JSON.parse(fs.readFileSync(machinesPath, "utf8"));
    return machines.machines?.["ff1-dvveklza"] || null;
  } catch (e) {
    log("⚠️ Could not load FF1 config from machines.json:", e.message);
    return null;
  }
}

// Execute FF1 cast via SSH through MacBook (for devcontainer)
async function castViaSSH(ff1Config, payload) {
  const { exec } = await import("child_process");
  const { promisify } = await import("util");
  const execAsync = promisify(exec);
  
  const ip = ff1Config.ip;
  const port = ff1Config.port || 1111;
  const payloadJson = JSON.stringify(payload).replace(/'/g, "'\\''"); // Escape for shell
  
  // SSH through MacBook to reach FF1 on local network
  const sshCmd = `ssh -o ConnectTimeout=5 jas@host.docker.internal "curl -s --connect-timeout 5 -X POST -H 'Content-Type: application/json' http://${ip}:${port}/api/cast -d '${payloadJson}'"`;
  
  log(`📡 FF1 cast via SSH: http://${ip}:${port}/api/cast`);
  const { stdout, stderr } = await execAsync(sshCmd, { timeout: 15000 });
  
  if (stderr && !stdout) {
    throw new Error(stderr);
  }
  
  try {
    return JSON.parse(stdout);
  } catch {
    return { raw: stdout };
  }
}

fastify.post("/ff1/cast", async (req, reply) => {
  reply.header("Access-Control-Allow-Origin", "*");
  reply.header("Access-Control-Allow-Methods", "POST, OPTIONS");
  reply.header("Access-Control-Allow-Headers", "Content-Type");
  
  const { topicID, apiKey, command, request, useDirect } = req.body || {};
  const ff1Config = getFF1Config();
  
  // Build the DP-1 payload
  const payload = {
    command: command || "displayPlaylist",
    request: request || {}
  };
  
  // Strategy 1: Try direct connection via SSH tunnel (in dev mode)
  if (dev && ff1Config?.ip) {
    try {
      const result = await castViaSSH(ff1Config, payload);
      return { success: true, method: "direct-ssh", response: result };
    } catch (sshErr) {
      log(`⚠️ FF1 SSH cast failed: ${sshErr.message}`);
      // Fall through to cloud relay
    }
  }
  
  // Strategy 2: Try direct connection (if useDirect or localhost tunnel is running)
  if (useDirect) {
    const deviceUrl = `http://localhost:1111/api/cast`;
    try {
      log(`📡 FF1 direct cast to ${deviceUrl}`);
      const directResponse = await fetch(deviceUrl, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(payload),
        signal: AbortSignal.timeout(5000), // 5s timeout
      });
      
      if (directResponse.ok) {
        const result = await directResponse.json();
        return { success: true, method: "direct", response: result };
      }
      log(`⚠️ FF1 direct cast failed: ${directResponse.status}`);
    } catch (directErr) {
      log(`⚠️ FF1 direct connection failed: ${directErr.message}`);
    }
  }
  
  // Strategy 3: Use cloud relay with topicID
  const relayTopicId = topicID || ff1Config?.topicId;
  if (!relayTopicId) {
    reply.status(400);
    return { 
      success: false, 
      error: "No topicID provided and no FF1 config found. Get topicID from your FF1 app settings." 
    };
  }
  
  const relayUrl = `${FF1_RELAY_URL}?topicID=${encodeURIComponent(relayTopicId)}`;
  
  try {
    log(`☁️ FF1 relay cast to ${relayUrl}`);
    const headers = { "Content-Type": "application/json" };
    if (apiKey || ff1Config?.apiKey) {
      headers["API-KEY"] = apiKey || ff1Config?.apiKey;
    }
    
    const relayResponse = await fetch(relayUrl, {
      method: "POST",
      headers,
      body: JSON.stringify(payload),
      signal: AbortSignal.timeout(10000), // 10s timeout
    });
    
    const responseText = await relayResponse.text();
    let responseData;
    try {
      responseData = JSON.parse(responseText);
    } catch {
      responseData = { raw: responseText };
    }
    
    if (!relayResponse.ok) {
      // Check if relay is down (404 or Cloudflare errors)
      if (relayResponse.status === 404 || responseText.includes("error code:")) {
        reply.status(503);
        return { 
          success: false, 
          error: "FF1 cloud relay is unavailable",
          hint: "The Feral File relay service appears to be down. Use ac-ff1 tunnel for local development.",
          details: responseData 
        };
      }
      reply.status(relayResponse.status);
      return { success: false, error: `FF1 relay error: ${relayResponse.status}`, details: responseData };
    }
    
    return { success: true, method: "relay", response: responseData };
  } catch (relayErr) {
    reply.status(500);
    return { success: false, error: relayErr.message };
  }
});

// FF1 CORS preflight
fastify.options("/ff1/cast", async (req, reply) => {
  reply.header("Access-Control-Allow-Origin", "*");
  reply.header("Access-Control-Allow-Methods", "POST, OPTIONS");
  reply.header("Access-Control-Allow-Headers", "Content-Type");
  return "";
});

// *** Chat Log Endpoint (for system logs from other services) ***
fastify.post("/chat/log", async (req, reply) => {
  const host = req.headers.host;
  // Determine which chat instance based on a header or default to chat-system
  const chatHost = req.headers["x-chat-instance"] || "chat-system.aesthetic.computer";
  const instance = chatManager.getInstance(chatHost);
  
  if (!instance) {
    reply.status(404);
    return { status: "error", message: "Unknown chat instance" };
  }
  
  const result = await chatManager.handleLog(instance, req.body, req.headers.authorization);
  reply.status(result.status);
  return result.body;
});

// *** Chat Status Endpoint ***
fastify.get("/chat/status", async (req) => {
  return chatManager.getStatus();
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
  
  // GET /devices - List all connected clients with metadata and names
  fastify.get("/devices", async () => {
    const clientList = getClientStatus();
    // Enhance with device names and letters
    const enhanced = clientList.map((c, index) => ({
      ...c,
      letter: getDeviceLetter(c.id),
      deviceName: deviceNames[c.ip]?.name || null,
      deviceGroup: deviceNames[c.ip]?.group || null,
    }));
    return {
      devices: enhanced,
      host: { name: DEV_HOST_NAME, ip: DEV_LAN_IP },
      timestamp: Date.now()
    };
  });
  
  // GET /dev-info - Get dev host info for client overlay
  fastify.get("/dev-info", async (req, reply) => {
    // Add CORS headers for cross-origin requests from main site
    reply.header("Access-Control-Allow-Origin", "*");
    reply.header("Access-Control-Allow-Methods", "GET");
    return {
      host: DEV_HOST_NAME,
      ip: DEV_LAN_IP,
      mode: "LAN Dev",
      timestamp: Date.now()
    };
  });
  
  // POST /jump/:target - Targeted jump (by ID, IP, handle, or device name)
  fastify.post("/jump/:target", async (req) => {
    const { target } = req.params;
    const { piece, ahistorical, alias } = req.body;
    
    const targeted = targetClients(target);
    if (targeted.length === 0) {
      return { error: "No matching device", target };
    }
    
    targeted.forEach(({ ws }) => {
      ws.send(pack("jump", { piece, ahistorical, alias }, "pieces"));
    });
    
    return { 
      msg: "Targeted jump sent", 
      piece, 
      count: targeted.length,
      targets: targeted.map(t => t.id)
    };
  });
  
  // POST /reload/:target - Targeted reload
  fastify.post("/reload/:target", async (req) => {
    const { target } = req.params;
    const targeted = targetClients(target);
    
    targeted.forEach(({ ws }) => {
      ws.send(pack("reload", req.body, "pieces"));
    });
    
    return { msg: "Targeted reload sent", count: targeted.length };
  });
  
  // POST /piece-reload/:target - Targeted KidLisp reload
  fastify.post("/piece-reload/:target", async (req) => {
    const { target } = req.params;
    const { source, createCode, authToken } = req.body;
    const targeted = targetClients(target);
    
    targeted.forEach(({ ws }) => {
      ws.send(pack("piece-reload", { source, createCode, authToken }, "kidlisp"));
    });
    
    return { msg: "Targeted piece-reload sent", count: targeted.length };
  });
  
  // POST /device/name - Set a friendly name for a device by IP
  fastify.post("/device/name", async (req) => {
    const { ip, name, group } = req.body;
    if (!ip) return { error: "IP required" };
    
    const cleanIp = ip.replace('::ffff:', '');
    if (name) {
      deviceNames[cleanIp] = { name, group: group || null, updatedAt: Date.now() };
    } else {
      delete deviceNames[cleanIp];
    }
    saveDeviceNames();
    
    // Notify the device of its new name
    const targeted = targetClients(cleanIp);
    targeted.forEach(({ ws }) => {
      ws.send(pack("dev:identity", { 
        name, 
        host: DEV_HOST_NAME, 
        hostIp: DEV_LAN_IP,
        mode: "LAN Dev"
      }, "dev"));
    });
    
    return { 
      msg: name ? "Device named" : "Device name cleared", 
      ip: cleanIp, 
      name,
      notified: targeted.length
    };
  });
  
  // GET /device/names - List all device names
  fastify.get("/device/names", async () => {
    return { names: deviceNames };
  });
}

// *** HTTP Server Initialization ***

// Track UDP channels manually (geckos.io doesn't expose this)
const udpChannels = {};

// 🩰 Initialize geckos.io BEFORE server starts listening
// Configure for devcontainer/Docker environment:
// - iceServers: Use local TURN server for relay (required in Docker/devcontainer)
// - portRange: constrain UDP to small range that can be exposed from container
// - cors: allow from any origin in dev mode

// Detect external IP for TURN server (browsers need to reach TURN from outside container)
// In devcontainer, we expose ports to the host, so use the host's LAN IP
// Priority: TURN_HOST env var > DEV_LAN_IP > localhost
const getExternalTurnHost = () => {
  // Check for explicitly set TURN host
  if (process.env.TURN_HOST) return process.env.TURN_HOST;
  // Use the DEV_LAN_IP if available (detected earlier)
  if (DEV_LAN_IP) return DEV_LAN_IP;
  // Fallback to localhost (won't work for external clients but ok for local testing)
  return 'localhost';
};

const turnHost = getExternalTurnHost();
console.log("🩰 TURN server host for ICE:", turnHost);

const devIceServers = [
  { urls: `stun:${turnHost}:3478` },
  { 
    urls: `turn:${turnHost}:3478`,
    username: 'aesthetic',
    credential: 'computer123'
  },
];
const prodIceServers = [
  { urls: 'stun:stun.l.google.com:19302' },
  // TODO: Add production TURN server
];

const io = geckos({
  iceServers: dev ? devIceServers : prodIceServers,
  // Force relay-only mode in dev to work through container networking
  // Direct UDP won't work from host browser to container internal IP
  iceTransportPolicy: dev ? 'relay' : 'all',
  portRange: {
    min: 10000,
    max: 10007,
  },
  cors: {
    allowAuthorization: true,
    origin: dev ? "*" : "https://aesthetic.computer",
  },
});
io.addServer(server); // Hook up to the HTTP Server - must be before listen()
console.log("🩰 Geckos.io server attached to fastify server (UDP ports 10000-10007)");

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
  const clientList = getClientStatus();
  
  // Get chat status with recent messages
  const chatStatus = chatManager.getStatus();
  const chatWithMessages = chatStatus.map(instance => {
    const recentMessages = instance.messages > 0 
      ? chatManager.getRecentMessages(instance.host, 5)
      : [];
    return {
      ...instance,
      recentMessages
    };
  });
  
  // Filter old errors
  const cutoff = Date.now() - ERROR_RETENTION_MS;
  const recentErrors = errorLog.filter(e => new Date(e.timestamp).getTime() > cutoff);
  
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
      unique_clients: clientList.length
    },
    clients: clientList,
    chat: chatWithMessages,
    errors: recentErrors.slice(-20).reverse() // Most recent first
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
  <meta name="robots" content="noindex, nofollow">
  <title>session-server</title>
  <style>
    * { margin: 0; padding: 0; box-sizing: border-box; }
    body {
      font-family: monospace;
      background: #000;
      color: #0f0;
      padding: 1.5rem;
      line-height: 1.5;
    }
    .header {
      border-bottom: 1px solid #333;
      padding-bottom: 1rem;
      margin-bottom: 1.5rem;
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
    .grid {
      display: grid;
      grid-template-columns: 1fr 1fr;
      gap: 1.5rem;
    }
    @media (max-width: 900px) {
      .grid { grid-template-columns: 1fr; }
    }
    .section {
      background: #0a0a0a;
      border: 1px solid #222;
      border-radius: 4px;
      padding: 1rem;
    }
    .section h2 {
      color: #0ff;
      font-size: 0.95rem;
      margin-bottom: 0.75rem;
      border-bottom: 1px solid #222;
      padding-bottom: 0.5rem;
    }
    .client {
      background: #111;
      border-left: 3px solid #0f0;
      padding: 0.75rem;
      margin-bottom: 0.75rem;
    }
    .name {
      color: #0ff;
      font-weight: bold;
    }
    .ping { color: yellow; }
    .detail {
      color: #888;
      margin-top: 0.2rem;
      font-size: 0.85rem;
    }
    .empty { color: #555; font-style: italic; }
    .chat-instance {
      background: #111;
      border-left: 3px solid #f0f;
      padding: 0.75rem;
      margin-bottom: 0.75rem;
    }
    .chat-instance.offline { border-left-color: #f00; opacity: 0.6; }
    .chat-instance .name { color: #f0f; }
    .chat-msg {
      background: #0a0a0a;
      padding: 0.4rem 0.6rem;
      margin-top: 0.4rem;
      font-size: 0.8rem;
      border-radius: 3px;
    }
    .chat-msg .from { color: #0ff; }
    .chat-msg .text { color: #aaa; }
    .chat-msg .time { color: #555; font-size: 0.75rem; }
    .error-log {
      background: #1a0000;
      border-left: 3px solid #f00;
      padding: 0.5rem;
      margin-bottom: 0.5rem;
      font-size: 0.8rem;
    }
    .error-log .time { color: #555; }
    .error-log .msg { color: #f66; }
    .warn-log {
      background: #1a1a00;
      border-left: 3px solid #ff0;
    }
    .warn-log .msg { color: #ff6; }
    .no-errors { color: #0f0; font-style: italic; }
    .tabs {
      display: flex;
      gap: 0.5rem;
      margin-bottom: 1rem;
    }
    .tab {
      padding: 0.4rem 0.8rem;
      background: #111;
      border: 1px solid #333;
      color: #888;
      cursor: pointer;
      border-radius: 3px;
      font-family: monospace;
      font-size: 0.85rem;
    }
    .tab.active {
      background: #0f0;
      color: #000;
      border-color: #0f0;
    }
    .tab-content { display: none; }
    .tab-content.active { display: block; }
  </style>
</head>
<body>
  <div class="header">
    <h1>🧩 session-server</h1>
    <div class="status">
      <span id="ws-status">🔴</span> | 
      Uptime: <span id="uptime">--</span> | 
      Online: <span id="client-count">0</span> |
      Chat: <span id="chat-count">0</span>
    </div>
  </div>
  
  <div class="tabs">
    <button class="tab active" data-tab="overview">Overview</button>
    <button class="tab" data-tab="chat">💬 Chat</button>
    <button class="tab" data-tab="errors">⚠️ Errors</button>
  </div>
  
  <div id="overview" class="tab-content active">
    <div class="grid">
      <div class="section">
        <h2>🧑‍💻 Connected Clients</h2>
        <div id="clients"></div>
      </div>
      <div class="section">
        <h2>💬 Chat Instances</h2>
        <div id="chat-status"></div>
      </div>
    </div>
  </div>
  
  <div id="chat" class="tab-content">
    <div class="grid">
      <div class="section" id="chat-system-section">
        <h2>💬 chat-system</h2>
        <div id="chat-system-messages"></div>
      </div>
      <div class="section" id="chat-clock-section">
        <h2>🕐 chat-clock</h2>
        <div id="chat-clock-messages"></div>
      </div>
      <div class="section" id="chat-sotce-section">
        <h2>🌸 chat-sotce</h2>
        <div id="chat-sotce-messages"></div>
      </div>
    </div>
  </div>
  
  <div id="errors" class="tab-content">
    <div class="section">
      <h2>⚠️ Recent Errors & Warnings</h2>
      <div id="error-log"></div>
    </div>
  </div>

  <script>
    // Tab switching
    document.querySelectorAll('.tab').forEach(tab => {
      tab.addEventListener('click', () => {
        document.querySelectorAll('.tab').forEach(t => t.classList.remove('active'));
        document.querySelectorAll('.tab-content').forEach(c => c.classList.remove('active'));
        tab.classList.add('active');
        document.getElementById(tab.dataset.tab).classList.add('active');
      });
    });
    
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
    
    function formatTime(dateStr) {
      if (!dateStr) return '';
      const d = new Date(dateStr);
      return d.toLocaleTimeString('en-US', { hour: '2-digit', minute: '2-digit' });
    }
    
    function escapeHtml(str) {
      if (!str) return '';
      return str.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
    }
    
    function update(s) {
      const hrs = Math.floor(s.server.uptime / 3600);
      const min = Math.floor((s.server.uptime % 3600) / 60);
      document.getElementById('uptime').textContent = \`\${hrs}h \${min}m\`;
      document.getElementById('client-count').textContent = s.totals.unique_clients;
      
      // Chat instance count
      const totalChatters = s.chat ? s.chat.reduce((sum, c) => sum + c.connections, 0) : 0;
      document.getElementById('chat-count').textContent = totalChatters;
      
      // Clients section
      const clientsHtml = s.clients.length === 0 
        ? '<div class="empty">Nobody online</div>'
        : s.clients.map(c => {
            let out = '<div class="client">';
            out += '<div class="name">';
            out += escapeHtml(c.handle) || '(anonymous)';
            if (c.multipleTabs && c.connectionCount.total > 1) out += \` (×\${c.connectionCount.total})\`;
            if (c.websocket?.ping) out += \` <span class="ping">(\${c.websocket.ping}ms)</span>\`;
            out += '</div>';
            if (c.location && c.location !== '*keep-alive*') out += \`<div class="detail">📍 \${escapeHtml(c.location)}</div>\`;
            if (c.geo) {
              let geo = '🗺️ ';
              if (c.geo.city) geo += c.geo.city + ', ';
              if (c.geo.region) geo += c.geo.region + ', ';
              geo += c.geo.country;
              out += \`<div class="detail">\${geo}</div>\`;
            } else if (c.ip) {
              out += \`<div class="detail">🌐 \${c.ip}</div>\`;
            }
            if (c.websocket?.worlds?.length > 0) {
              const w = c.websocket.worlds[0];
              out += \`<div class="detail">🌍 \${escapeHtml(w.piece)}\`;
              if (w.showing) out += \` (viewing \${escapeHtml(w.showing)})\`;
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
      document.getElementById('clients').innerHTML = clientsHtml;
      
      // Chat status section (overview)
      if (s.chat) {
        const chatHtml = s.chat.map(c => {
          const isOnline = c.messages >= 0;
          return \`<div class="chat-instance \${isOnline ? '' : 'offline'}">
            <div class="name">\${escapeHtml(c.name)} \${isOnline ? '🟢' : '🔴'}</div>
            <div class="detail">🧑‍🤝‍🧑 \${c.connections} connected</div>
            <div class="detail">💾 \${c.messages} messages loaded</div>
          </div>\`;
        }).join('');
        document.getElementById('chat-status').innerHTML = chatHtml;
      } else {
        document.getElementById('chat-status').innerHTML = '<div class="empty">Chat not initialized</div>';
      }
      
      // Chat messages (detailed view)
      if (s.chat) {
        s.chat.forEach(c => {
          const name = c.name.replace('chat-', '');
          const el = document.getElementById(\`chat-\${name}-messages\`) || document.getElementById(\`chat-\${c.name}-messages\`);
          if (el && c.recentMessages) {
            const msgsHtml = c.recentMessages.length === 0
              ? '<div class="empty">No recent messages</div>'
              : c.recentMessages.map(m => \`<div class="chat-msg">
                  <span class="from">\${escapeHtml(m.from)}</span>
                  <span class="text">\${escapeHtml(m.text)}</span>
                  <span class="time">\${formatTime(m.when)}</span>
                </div>\`).join('');
            el.innerHTML = msgsHtml;
          }
        });
      }
      
      // Error log
      if (s.errors && s.errors.length > 0) {
        const errHtml = s.errors.map(e => \`<div class="\${e.level === 'error' ? 'error-log' : 'warn-log error-log'}">
          <span class="time">[\${formatTime(e.timestamp)}]</span>
          <span class="msg">\${escapeHtml(e.message)}</span>
        </div>\`).join('');
        document.getElementById('error-log').innerHTML = errHtml;
      } else {
        document.getElementById('error-log').innerHTML = '<div class="no-errors">✅ No errors in the last hour</div>';
      }
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
  
  // Route chat connections to ChatManager based on host
  const host = req.headers.host;
  if (chatManager.isChatHost(host)) {
    log('💬 Chat client connection from:', host);
    chatManager.handleConnection(ws, req);
    return; // Don't process as a game client
  }
  
  // Route socklogs connections - devices sending logs and viewers subscribing
  if (req.url.startsWith('/socklogs')) {
    const urlParams = new URL(req.url, 'http://localhost').searchParams;
    const role = urlParams.get('role') || 'device'; // 'device' or 'viewer'
    const deviceId = urlParams.get('deviceId') || `device-${Date.now()}`;
    
    if (role === 'viewer') {
      // Viewer wants to see logs from devices
      log('👁️ SockLogs viewer connected');
      socklogsViewers.add(ws);
      
      // Send current status
      ws.send(JSON.stringify({
        type: 'status',
        ...socklogsStatus()
      }));
      
      ws.on('close', () => {
        log('👁️ SockLogs viewer disconnected');
        socklogsViewers.delete(ws);
      });
      
      ws.on('error', (err) => {
        error('👁️ SockLogs viewer error:', err);
        socklogsViewers.delete(ws);
      });
    } else {
      // Device sending logs
      log(`📱 SockLogs device connected: ${deviceId}`);
      socklogsDevices.set(deviceId, {
        ws,
        logCount: 0,
        lastLog: null,
        connectedAt: Date.now()
      });
      
      // Notify viewers of new device
      for (const viewer of socklogsViewers) {
        if (viewer.readyState === WebSocket.OPEN) {
          viewer.send(JSON.stringify({
            type: 'device-connected',
            deviceId,
            status: socklogsStatus()
          }));
        }
      }
      
      ws.on('message', (data) => {
        try {
          const msg = JSON.parse(data.toString());
          if (msg.type === 'log') {
            const device = socklogsDevices.get(deviceId);
            if (device) {
              device.logCount++;
              device.lastLog = Date.now();
            }
            socklogsBroadcast(deviceId, msg);
          }
        } catch (e) {
          error('📱 SockLogs parse error:', e);
        }
      });
      
      ws.on('close', () => {
        log(`📱 SockLogs device disconnected: ${deviceId}`);
        socklogsDevices.delete(deviceId);
        
        // Notify viewers
        for (const viewer of socklogsViewers) {
          if (viewer.readyState === WebSocket.OPEN) {
            viewer.send(JSON.stringify({
              type: 'device-disconnected',
              deviceId,
              status: socklogsStatus()
            }));
          }
        }
      });
      
      ws.on('error', (err) => {
        error(`📱 SockLogs device error (${deviceId}):`, err);
        socklogsDevices.delete(deviceId);
      });
    }
    
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

  // In dev mode, send device identity info for LAN overlay
  if (dev) {
    const deviceName = deviceNames[cleanIp]?.name || null;
    const deviceLetter = getDeviceLetter(id);
    const identityPayload = {
      name: deviceName,
      letter: deviceLetter,
      host: DEV_HOST_NAME,
      hostIp: DEV_LAN_IP,
      mode: "LAN Dev",
      connectionId: id,
    };
    console.log(`📱 Sending dev:identity to ${cleanIp}:`, identityPayload);
    ws.send(pack("dev:identity", identityPayload, "dev"));
  }

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

    // 📦 Module streaming - handle module requests before other processing
    if (msg.type === "module:request") {
      const modulePath = msg.path;
      const withDeps = msg.withDeps === true; // Request all dependencies too
      const knownHashes = msg.knownHashes || {}; // Client's cached hashes
      
      if (withDeps) {
        // Recursively gather all dependencies
        const modules = {};
        let skippedCount = 0;
        
        const gatherDeps = (p, fromPath = null) => {
          if (modules[p] || modules[p] === null) return; // Already gathered (or marked as cached)
          const data = getModuleHash(p);
          if (!data) {
            // Only warn for top-level not found, not for deps (which might be optional)
            if (!fromPath) log(`📦 Module not found: ${p}`);
            return;
          }
          
          // Check if client already has this hash cached
          if (knownHashes[p] === data.hash) {
            modules[p] = null; // Mark as "client has it" - don't send content
            skippedCount++;
          } else {
            modules[p] = { hash: data.hash, content: data.content };
          }
          
          // Debug: show when gathering specific important modules
          if (p.includes('headers') || p.includes('kidlisp')) {
            log(`📦 Gathering ${p} (from ${fromPath || 'top'})${knownHashes[p] === data.hash ? ' [cached]' : ''}`);
          }
          
          // Parse static imports from content - match ES module import/export from statements
          // This regex only matches valid relative imports ending in .mjs or .js
          // Skip commented lines by checking each line doesn't start with //
          const staticImportRegex = /^(?!\s*\/\/).*?(?:import|export)\s+(?:[^;]*?\s+from\s+)?["'](\.{1,2}\/[^"'\s]+\.m?js)["']/gm;
          let match;
          while ((match = staticImportRegex.exec(data.content)) !== null) {
            const importPath = match[1];
            // Skip invalid paths
            if (importPath.includes('...') || importPath.length > 200) continue;
            
            // Resolve relative path
            const dir = path.dirname(p);
            const resolved = path.normalize(path.join(dir, importPath));
            log(`📦 Found dep: ${p} -> ${importPath} (resolved: ${resolved})`);
            gatherDeps(resolved, p);
          }
          
          // Parse dynamic imports - import("./path") or import('./path') or import(`./path`)
          // Skip commented lines
          const dynamicImportRegex = /^(?!\s*\/\/).*?import\s*\(\s*["'`](\.{1,2}\/[^"'`\s]+\.m?js)["'`]\s*\)/gm;
          while ((match = dynamicImportRegex.exec(data.content)) !== null) {
            const importPath = match[1];
            // Skip invalid paths
            if (importPath.includes('...') || importPath.length > 200) continue;
            
            // Resolve relative path
            const dir = path.dirname(p);
            const resolved = path.normalize(path.join(dir, importPath));
            gatherDeps(resolved, p);
          }
        };
        
        gatherDeps(modulePath);
        
        // Filter out null entries (modules client already has) and count
        const modulesToSend = {};
        const cachedPaths = [];
        for (const [p, data] of Object.entries(modules)) {
          if (data === null) {
            cachedPaths.push(p);
          } else {
            modulesToSend[p] = data;
          }
        }
        
        const totalModules = Object.keys(modules).length;
        const sentModules = Object.keys(modulesToSend).length;
        
        if (totalModules > 0) {
          // Log bundle stats
          if (skippedCount > 0) {
            log(`📦 Bundle for ${modulePath}: ${sentModules}/${totalModules} sent (${skippedCount} cached)`);
          } else {
            log(`📦 Bundle for ${modulePath}: ${sentModules} modules`);
          }
          
          ws.send(JSON.stringify({
            type: "module:bundle",
            entry: modulePath,
            modules: modulesToSend,
            cached: cachedPaths // Tell client which paths to use from cache
          }));
        } else {
          ws.send(JSON.stringify({
            type: "module:error",
            path: modulePath,
            error: "Module not found"
          }));
        }
      } else {
        // Single module request (original behavior)
        const moduleData = getModuleHash(modulePath);
        
        if (moduleData) {
          ws.send(JSON.stringify({
            type: "module:response",
            path: modulePath,
            hash: moduleData.hash,
            content: moduleData.content
          }));
          log(`📦 Module sent: ${modulePath} (${moduleData.content.length} bytes)`);
        } else {
          ws.send(JSON.stringify({
            type: "module:error",
            path: modulePath,
            error: "Module not found"
          }));
          log(`📦 Module not found: ${modulePath}`);
        }
      }
      return;
    }
    
    if (msg.type === "module:check") {
      const modulePath = msg.path;
      const clientHash = msg.hash;
      const moduleData = getModuleHash(modulePath);
      
      if (moduleData) {
        ws.send(JSON.stringify({
          type: "module:status",
          path: modulePath,
          changed: moduleData.hash !== clientHash,
          hash: moduleData.hash
        }));
      } else {
        ws.send(JSON.stringify({
          type: "module:status",
          path: modulePath,
          changed: true,
          hash: null,
          error: "Module not found"
        }));
      }
      return;
    }
    
    if (msg.type === "module:list") {
      // Return list of available modules (for prefetching)
      const modules = [
        "lib/disk.mjs",
        "lib/graph.mjs", 
        "lib/num.mjs",
        "lib/geo.mjs",
        "lib/parse.mjs",
        "lib/help.mjs",
        "lib/text.mjs",
        "bios.mjs"
      ];
      const moduleInfo = modules.map(p => {
        const data = getModuleHash(p);
        return data ? { path: p, hash: data.hash, size: data.content.length } : null;
      }).filter(Boolean);
      
      ws.send(JSON.stringify({
        type: "module:list",
        modules: moduleInfo
      }));
      return;
    }

    // 🎹 DAW Channel - M4L device ↔ IDE communication
    if (msg.type === "daw:join") {
      // Device (kidlisp.com/device) joining to receive code
      dawDevices.add(id);
      log(`🎹 DAW device joined: ${id} (total: ${dawDevices.size})`);
      ws.send(JSON.stringify({ type: "daw:joined", id }));
      return;
    }
    
    if (msg.type === "daw:code") {
      // IDE sending code to all connected devices
      log(`🎹 DAW code broadcast from ${id} to ${dawDevices.size} devices`);
      const codeMsg = JSON.stringify({
        type: "daw:code",
        content: msg.content,
        from: id
      });
      
      // Broadcast to all DAW devices
      for (const deviceId of dawDevices) {
        const deviceWs = connections[deviceId];
        if (deviceWs && deviceWs.readyState === WebSocket.OPEN) {
          deviceWs.send(codeMsg);
          log(`🎹 Sent code to device ${deviceId}`);
        }
      }
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
      
      // Send current channel state to late joiners
      if (codeChannelState[codeChannel]) {
        // Note: codeChannelState stores the original msg.content object, 
        // pack() will JSON.stringify it, so don't double-stringify here
        const stateMsg = pack("code", codeChannelState[codeChannel], id);
        send(stateMsg);
        log(`📥 Sent current state to late joiner on channel ${codeChannel}`);
      }
    } else if (msg.type === "slide" && msg.content?.codeChannel) {
      // Handle slide broadcast (low-latency value updates, no state storage)
      const targetChannel = msg.content.codeChannel;
      
      // Don't store slide updates as state (they're transient)
      // Just broadcast immediately for low latency
      if (codeChannels[targetChannel]) {
        const slideMsg = pack("slide", msg.content, id);
        subscribers(codeChannels[targetChannel], slideMsg);
      }
    } else if (msg.type === "code" && msg.content?.codeChannel) {
      // Handle code broadcast to channel subscribers (for kidlisp.com pop-out sync)
      const targetChannel = msg.content.codeChannel;
      
      // Store the latest state for late joiners
      codeChannelState[targetChannel] = msg.content;
      
      if (codeChannels[targetChannel]) {
        // Note: msg.content is already an object, pack() will JSON.stringify it
        const codeMsg = pack("code", msg.content, id);
        subscribers(codeChannels[targetChannel], codeMsg);
        log(`📢 Broadcast code to channel ${targetChannel} (${codeChannels[targetChannel].size} subscribers)`);
      }
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
    } else if (msg.type === "dev:log") {
      // 📡 Remote log forwarding from connected devices (LAN Dev mode)
      if (dev && msg.content) {
        const { level, args, deviceName, connectionId, time, queued } = msg.content;
        const client = clients[id];
        const deviceLabel = deviceName || client?.ip || `conn:${connectionId}`;
        const levelEmoji = level === 'error' ? '🔴' : level === 'warn' ? '🟡' : '🔵';
        const queuedTag = queued ? ' [Q]' : '';
        
        // Format the log output
        const timestamp = new Date(time).toLocaleTimeString();
        const argsStr = Array.isArray(args) ? args.join(' ') : String(args);
        
        console.log(`${levelEmoji} [${timestamp}] ${deviceLabel}${queuedTag}: ${argsStr}`);
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

      // 🎮 1v1 game position updates should only go to others (not back to sender)
      if (msg.type === "1v1:move") {
        // Log occasionally in production for debugging (1 in 100 messages)
        if (Math.random() < 0.01) {
          log(`🎮 1v1:move relay: ${msg.content?.handle || id} -> ${wss.clients.size - 1} others`);
        }
        others(JSON.stringify(msg));
        return;
      }
      
      // 🎮 1v1 join/state messages - log and relay to everyone
      if (msg.type === "1v1:join" || msg.type === "1v1:state") {
        log(`🎮 ${msg.type}: ${msg.content?.handle || id} -> all ${wss.clients.size} clients`);
      }

      everyone(JSON.stringify(msg)); // Relay any other message to every user.
    }
  });

  // More info: https://stackoverflow.com/a/49791634/8146077
  ws.on("close", () => {
    log("🚪 Someone left:", id, "Online:", wss.clients.size, "🫂");

    // Remove from VSCode clients if present
    vscodeClients.delete(ws);
    
    // Remove from DAW devices if present
    if (dawDevices.has(id)) {
      dawDevices.delete(id);
      log(`🎹 DAW device disconnected: ${id} (remaining: ${dawDevices.size})`);
    }
    if (dawIDEs.has(id)) {
      dawIDEs.delete(id);
      log(`🎹 DAW IDE disconnected: ${id}`);
    }

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
      if (codeChannels[codeChannel]?.size === 0) {
        delete codeChannels[codeChannel];
        delete codeChannelState[codeChannel]; // Clean up stored state too
        log(`🗑️ Cleaned up empty channel: ${codeChannel}`);
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

// geckos.io is imported at top and initialized before server.listen()

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

  // 🎮 1v1 FPS game position updates over UDP (low latency)
  channel.on("1v1:move", (data) => {
    if (channel.webrtcConnection.state === "open") {
      try {
        // Log occasionally for production debugging (1 in 100)
        if (Math.random() < 0.01) {
          const parsed = typeof data === 'string' ? JSON.parse(data) : data;
          log(`🩰 UDP 1v1:move: ${parsed?.handle || channel.id} broadcasting`);
        }
        // Broadcast position to all other players except sender
        channel.broadcast.emit("1v1:move", data);
      } catch (err) {
        console.warn("1v1:move broadcast error:", err);
      }
    }
  });

  // 🎚️ Slide mode: real-time code value updates via UDP (lowest latency)
  channel.on("slide:code", (data) => {
    if (channel.webrtcConnection.state === "open") {
      try {
        // Broadcast to all including sender (room.emit) for sync
        channel.room.emit("slide:code", data);
      } catch (err) {
        console.warn("slide:code broadcast error:", err);
      }
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
      "../system/public/kidlisp.com",
      "../system/public/gift.aesthetic.computer",
      "../system/public/give.aesthetic.computer",
      "../system/public/news.aesthetic.computer",
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
