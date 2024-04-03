// chat, 24.03.30.14.53
// This file is a unikernal compatible chat server designed to eventually replace `session-server/session.mjs` by adding both websocket and udp support.

// But its first job is to be the chat server for AC.

// Management:
// https://console.cloud.google.com/compute/instances?project=aesthetic-computer

import { WebSocketServer, WebSocket } from "ws";
import { readFileSync } from "fs";
import http from "http";
import https from "https";

import { initializeApp, cert } from "firebase-admin/app"; // Firebase notifications.
import { getMessaging } from "firebase-admin/messaging";
import { createClient } from "redis";
import { MongoClient } from "mongodb";
import "dotenv/config";

console.log("\n🌟 Starting the Aesthetic Computer Chat Server 🌟\n");

const dev = process.env.NODE_ENV === "development";

let server;
let connections = {}; // All active WebSocket connections.
let connectionId = 0;

const MONGODB_CONNECTION_STRING = process.env.MONGODB_CONNECTION_STRING;
const MONGODB_NAME = process.env.MONGODB_NAME;
const GCM_FIREBASE_CONFIG_URL = process.env.GCM_FIREBASE_CONFIG_URL;
const redisConnectionString = process.env.REDIS_CONNECTION_STRING;

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

async function startSocketServer() {
  // #region 🏬 Redis
  // *** Start up two `redis` clients. (One for subscribing, and for publishing)
  // const sub = !dev
  //   ? createClient({ url: redisConnectionString })
  //   : createClient();
  const sub = createClient({ url: redisConnectionString });
  sub.on("error", (err) =>
    console.log("🔴 Redis subscriber client error!", err),
  );

  // const pub = !dev
  //   ? createClient({ url: redisConnectionString })
  //   : createClient();
  const pub = createClient({ url: redisConnectionString });
  pub.on("error", (err) =>
    console.log("🔴 Redis publisher client error!", err),
  );

  try {
    await sub.connect();
    await pub.connect();

    // TODO: This needs to be sent only for a specific user or needs
    //       some kind of special ID.

    await sub.subscribe("chat-system", (message) => {
      const parsed = JSON.parse(message);
      console.log("Received chat from REDIS:", parsed, message);
      // TODO: Pack a message.
      everyone(pack(`message`, parsed));

    });
  } catch (err) {
    console.error("🔴 Could not connect to `redis` instance.", err);
  }
  // #endregion

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

      console.log(
        "💬 Received:",
        msg.type,
        "from:",
        msg.handle,
        "of:",
        msg.text,
      );

      // 💬 Received an incoming chat message.
      if (msg.type === "chat:message") {
        // TODO: ❤️‍🔥 Add rate-limiting / maybe quit here if needed.

        // 🔐 1. Authorization
        // 💡️ Maybe this could be cached at some point. 24.04.02.21.30
        const authorized = await authorize(msg.content.token);
        if (!authorized) {
          console.error("🔴 Unauthorized:", msg.content);
          ws.send(
            pack(
              "unauthorized",
              { message: "Your message was unauthorized, please login again." },
              id,
            ),
          );
          return;
        }

        console.log("🟢 🔐 Handle authorized:", authorized);

        // 📚 2. Persistence
        // TODO:  Add this chat to MongoDB, using the domain. (Only allow requests from the domain.)
        try {
          // const out = filter(msg.content); // TODO: Filter message for content.

          const stored = await storeMessageInMongo(msg.content);
          console.log("🟢 Message stored:", stored);

          // 1. PUB via redis now or send back to others() ?

          pub
            .publish(
              "chat-system",
              JSON.stringify({
                text: msg.content.text,
                handle: msg.content.handle,
              }),
            )
            .then((result) => {
              console.log("💬 Message succesfully published:", result);
              // if (!dev) {
              //   getMessaging()
              //     .send({
              //       notification: { title: "😱 Scream", body: out },
              //       topic: "scream",
              //       data: { piece },
              //     })
              //     .then((response) => {
              //       log("☎️  Successfully sent notification:", response);
              //     })
              //     .catch((error) => {
              //       log("📵  Error sending notification:", error);
              //     });
              // }
            })
            .catch((error) => {
              console.log("🙅‍♀️ Error publishing message:", error);
            });

          // How can I publish this to redis now?

          // 2. Send a push notification.
          //const serviceAccount = (
          //  await got(GCM_FIREBASE_CONFIG_URL, {
          //    responseType: "json",
          //  })
          //).body;
          // - [] Web push will need an update.
          // - [] iOS App will need an update.

          // 3. Send a confirmation back to the user.
        } catch (err) {
          console.error("🔴 Message could not be stored:", err);
          // TODO: Show cancellation of some kind to the user.
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

  // Sends a message to all connected clients.
  function everyone(string) {
    wss.clients.forEach((c) => {
      if (c?.readyState === WebSocket.OPEN) c.send(string);
    });
  }
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

// #region 🗺️ MongoDB

async function makeMongoConnection() {
  const client = new MongoClient(MONGODB_CONNECTION_STRING);
  await client.connect();
  const db = client.db(MONGODB_NAME);
  return { client, db };
}

async function storeMessageInMongo(message) {
  console.log("🟡 Connecting to MongoDB...");
  const { client, db } = await makeMongoConnection();

  let user = null;

  if (message.handle) {
    // Extract handle name (assuming handle is in the format "@handleName")
    const handleName = message.handle.slice(1);

    // Query the `handles` collection to find the corresponding user _id
    console.log("🟡 Looking up user record...");
    const handle = await db
      .collection("@handles")
      .findOne({ handle: handleName });
    if (handle) user = handle._id;
  }

  console.log("🟡 Storing message...");
  const collection = db.collection("chat-system");
  await collection.createIndex({ when: 1 }); // Index for `when`.

  // Store the chat message
  const inserted = await collection.insertOne({
    user,
    text: message.text,
    when: new Date(),
  });

  await client.close();
  return inserted;
}

// #endregion
