// Session Server, 22.12.04.14.57
// Represents a "room" or user or "client" backend
// which at the moment is run once for every "piece"
// that requests it.

/* #region todo 📓 
 - [] Speed up developer reload by using redis pub/sub.
 - [] Cache some stuff in redis / connect to redis here.
 - [] Send a signal to everyone once a user leaves.
 + Done
 - [x] Get "developer" live reloading working again. 
 - [x] Add sockets back.
 - [x] Make a "local" option.
 - [x] Read through: https://redis.io/docs/data-types
#endregion */

// Add redis pub/sub here...

import Fastify from "fastify";
import { WebSocket, WebSocketServer } from "ws";
import ip from "ip";
import chokidar from "chokidar";
import fs from "fs";

import { createClient } from "redis";
const redisConnectionString = process.env.REDIS_CONNECTION_STRING;

const dev = process.env.NODE_ENV === "development";

let fastify;

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
} else {
  fastify = Fastify({ logger: true }); // Still log in production. No reason not to?
}

const server = fastify.server;

const info = {
  port: process.env.PORT, // 8889 in development via `package.json`
  name: process.env.SPAWNER_NAME,
  url: process.env.SPAWNER_URL,
  service: process.env.SPAWNER_SERVICE,
};

// *** Start Up a REDIS Client ***
const client = !dev
  ? createClient({ url: redisConnectionString })
  : createClient();
client.on("error", (err) => console.log("🔴 Redis client error!", err));
await client.connect();
// console.log("client", redisConnectionString);

await client.subscribe("code", (message) => {
  // console.log(message); // 'message'
  everyone(pack("code", message, "development"));
});

fastify.get("/", async () => {
  return { msg: "Hello aesthetic.computer!" };
});

// *** Live Reload of Pieces in Development ***
fastify.post("/reload", async (req) => {
  everyone(pack("reload", req.body, "pieces"));
  // console.log("Reload!", req.body);
  return { msg: "Reload request sent!", body: req.body };
});

// *** HTTP Server Initialization ***
const start = async () => {
  try {
    if (dev) {
      fastify.listen({
        host: "0.0.0.0",// ip.address(),
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

// *** Socket Server Initialization ***
// #region socket
let wss;
const connections = {};

let connectionId = 0; // TODO: Eventually replace with a username arrived at through
//                             a client <-> server authentication function.

wss = new WebSocketServer({ server });
console.log(
  `🤖 session.aesthetic.computer (${
    dev ? "Development" : "Production"
  }) socket: wss://${ip.address()}:${info.port}`
);

// Pack messages into a simple object protocol of `{type, content}`.
function pack(type, content, id) {
  return JSON.stringify({ type, content, id });
}

// Construct the server.
wss.on("connection", (ws, req) => {
  const ip = req.socket.remoteAddress || "localhost"; // beautify ip

  // Assign the conection a unique id.
  connections[connectionId] = ws;
  const id = connectionId;

  // Send a single welcome message for every new client connection.
  // TODO: This message should be a JSON encoded object and be displayed on
  //       the client instead.
  const content = { ip, id, playerCount: wss.clients.size };

  ws.send(pack("message", JSON.stringify(content), id));

  // Send a message to all other clients except this one.
  function others(string) {
    wss.clients.forEach((c) => {
      if (c !== ws && c.readyState === WebSocket.OPEN) c.send(string);
    });
  }

  others(
    pack(
      "message",
      JSON.stringify({
        text: `${connectionId} has joined from ${ip}. Connections open: ${content.playerCount}`,
      }),
      id
    )
  );

  connectionId += 1;

  // Relay all incoming messages from this client to everyone else.
  ws.on("message", (data) => {
    // Parse incoming message and attach client identifier.
    const msg = JSON.parse(data.toString());
    msg.id = id; // TODO: When sending a server generated message, use a special id.
    console.log(msg);
    // TODO: Why not always use "others" here?
    everyone(JSON.stringify(msg));
    // others(JSON.stringify(msg));
  });

  /*
  ws.isAlive = true; // For checking persistence between ping-pong messages.

  // Send a ping message to all clients every 30 seconds, and kill
  // the client if it does not respond back with a pong on any given pass.
  const interval = setInterval(function ping() {
    wss.clients.forEach((client) => {
      if (client.isAlive === false) return client.terminate();
      client.isAlive = false;
      client.ping();
    });
  }, 3000);

  ws.on("pong", () => (ws.isAlive = true)); // Receive a pong.

  // Stop pinging once the socket closes.
  // More info: https://stackoverflow.com/a/49791634/8146077
  ws.on("close", () => {
    everyone(pack("left", { id, count: wss.clients.size }));
    clearInterval(interval);
  });
  */
});

// Sends a message to all connected clients.
function everyone(string) {
  wss.clients.forEach((c) => {
    if (c.readyState === WebSocket.OPEN) c.send(string);
  });
}

// 🚧 File Watching in Local Development Mode
// File watching uses: https://github.com/paulmillr/chokidar
if (dev) {
  // 1. Watch for local file changes in pieces.
  chokidar
    .watch("../system/public/aesthetic.computer/disks")
    .on("all", (event, path) => {
      //console.log("Disk:", event, path);
      if (event === "change") everyone(pack("reload", { piece: "*" }, "local"));
    });

  // 2. Watch base system files.
  chokidar
    .watch([
      "../system/public/aesthetic.computer/lib",
      "../system/public/aesthetic.computer/boot.js",
      "../system/public/aesthetic.computer/bios.js",
      "../system/public/aesthetic.computer/style.css",
    ])
    .on("all", (event, path) => {
      if (event === "change")
        everyone(pack("reload", { piece: "*refresh*" }, "local"));
    });
}
// #endregion
