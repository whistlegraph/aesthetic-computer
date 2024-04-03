// chat, 24.03.30.14.53
// This file is a unikernal compatible chat server designed to eventually replace `session-server/session.mjs` by adding both websocket and udp support.

// But its first job is to be the chat server for AC.

// Management:
// https://console.cloud.google.com/compute/instances?project=aesthetic-computer

import { WebSocketServer } from "ws";
import { readFileSync } from "fs";
import http from "http";
import https from "https";

import { initializeApp, cert } from "firebase-admin/app"; // Firebase notifications.
import { getMessaging } from "firebase-admin/messaging";
import { createClient } from "redis";

console.log("\n🌟 Starting the Aesthetic Computer Chat Server 🌟\n");

const dev = process.env.NODE_ENV === "development";

let server;
let connections = {}; // All active WebSocket connections.
let connectionId = 0;

const MONGODB_CONNECTION_STRING = process.env.MONGODB_CONNECTION_STRING;
const MONGODB_NAME = process.env.MONGODB_NAME;
const GCM_FIREBASE_CONFIG_URL = process.env.GCM_FIREBASE_CONFIG_URL;
const redisConnectionString = process.env.REDIS_CONNECTION_STRING

const request = (req, res) => {
  const domain = req.headers.host; // Get the domain from the request
  res.writeHead(200, { "Content-Type": "text/plain; charset=utf-8" });
  res.end(`😱 Aesthetic Computer\nHost: <mark>${domain}</mark>`);
};

if (dev) {
  server = https.createServer(
    {
      key: readFileSync("ssl/localhost-key.pem"),
      cert: readFileSync("ssl/localhost.pem"),
    },
    request,
  );
} else {
  server = http.createServer(request);
}

const port = dev ? 8083 : 80;

server.listen(port, "0.0.0.0", () => {
  console.log(
    `--> Web server running at ${dev ? "https" : "http"}://0.0.0.0:${port} 🕷️`,
  );
  startSocketServer();
});

function startSocketServer() {
  const wss = new WebSocketServer({ server });
  wss.on("connection", (ws, req) => {
    connections[connectionId] = ws;
    connectionId += 1;
    const id = connectionId;
    const ip = req.socket.remoteAddress || "localhost"; // beautify ip
    ws.isAlive = true; // For checking persistence between ping-pong messages.

    ws.on("pong", () => {
      ws.isAlive = true;
    }); // Receive a pong and stay alive!

    console.log(
      "🔌 New connection:",
      `${id}:${ip}`,
      "Online:",
      wss.clients.size,
      "🫂",
    );

    ws.on("message", async (data) => {
      const msg = JSON.parse(data.toString());
      msg.id = id;

      console.log("💬 Received:", msg.type);

      // 💬 Received an incoming chat message.
      if (msg.type === "chat:message") {
        // TODO: ❤️‍🔥 Add rate-limiting / maybe quit here if needed.

        // 🔐 1. Authorization
        const authorized = await authorize(msg.content.token);
        // TODO (future): Maybe this could be cached at some point. 24.04.02.21.30
        if (authorized) {
          console.log("🔐 Authorized:", authorized);

        // 📚 2. Persistence
        // TODO:  Add this chat to MongoDB, using the domain. (Only allow requests from the domain.)

        // Show cancellation if this fails.

        // (Maybe?) 3. PUB via redis to all connected users.

        // 4. Send a push notification with the type and channel with tap through.

        //const serviceAccount = (
        //  await got(GCM_FIREBASE_CONFIG_URL, {
        //    responseType: "json",
        //  })
        //).body;
        // - [] Web push will need an update.
        // - [] iOS App will need an update.

        } else {
          console.error("🔴 Unauthorized:", msg.content);
          ws.send(
            pack(
              "unauthorized",
              { message: "Your message was unauthorized, please login again." },
              id,
            ),
          );
        }
      }

      // TODO: I need to... authorize the incoming message based on the
      //       token... and probably cache this somehow.

      // Message send process:

      // 1. Authorize on new connection. See `authorization.mjs`.
      // 2. Only add to chatter list if authorized!

      // 3. Submit message to database in MongoDB.
      // 4. Send through redis to all connected users.
      // 6. Show a cancellation if that occurs.

      // New connection process:
      // 1. On a new connection, get a paged list of messages from MongoDB,
      //    but also keep a cache here on the server so it just starts up
      //    and always stores the last 50 messages or something.
      // 2. Create an http request on this endpoint for fetching more messages.

      // TODO: Depending on the message type, relay back to everyone
      //       and/or pass it into the chat log.
    });

    const interval = setInterval(function ping() {
      wss.clients.forEach((client) => {
        if (client.isAlive === false) {
          return client.terminate();
        }
        client.isAlive = false;
        client.ping();
      });
    }, 15000); // 15 second pings from server before termination.

    ws.on("close", () => {
      // Delete from the connection index.
      clearInterval(interval);
      delete connections[id];
      console.log(
        "🚪 Closed connection:",
        id,
        "Online:",
        wss.clients.size,
        "🫂",
      );
    });

    ws.send(
      pack(
        "connected",
        {
          message: "Welcome to the Aesthetic Computer System Chat!",
          chatters: wss.clients.size,
        },
        id,
      ),
    );
  });

  console.log(
    `--> Socket server running at ${
      dev ? "wss" : "ws"
    }://0.0.0.0:${port} 🧦 \n`,
  );
}

// ⚙️ Utilities

// Pack messages into a simple object protocol of `{type, content}`.
function pack(type, content, id) {
  if (typeof content === "object") content = JSON.stringify(content);
  return JSON.stringify({ type, content, id });
}

// Authorize a user token against auth0.
async function authorize(authorization) {
  try {
    const response = await fetch("https://aesthetic.us.auth0.com/userinfo", {
      headers: {
        Authorization: "Bearer " + authorization,
        "Content-Type": "application/json",
      },
    });

    if (response.status === 200) {
      return response.json();
    } else {
      console.log(response);
      throw new Error("🔴 Unauthorized");
    }
  } catch {
    return undefined;
  }
}
