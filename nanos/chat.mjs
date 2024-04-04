// chat, 24.03.30.14.53
// This file is a unikernal compatible chat server designed to eventually replace `session-server/session.mjs` by adding both websocket and udp support.
// But its first job is to be the chat server for AC.

/* #region 🏁 TODO 
 - [-] Get Firebase notifications fixed everywhere!
       (Maybe in production?)
 - [] Get text filtering working. 
 + Done
 - [x] Connect to production server.
 - [x] Disallow any req that isn't the chat-system.aesthetic.computer host when not in dev mode.
 - [x] New connection process:
    1. On a new connection, get a paged list of messages from MongoDB,
      but also keep a cache here on the server so it just starts up
      and always stores the last 50 messages or something.
    2. Create an http request on this endpoint for fetching more messages.
#endregion */

// Management:
// https://console.cloud.google.com/compute/instances?project=aesthetic-computer

import { WebSocketServer, WebSocket } from "ws";
import { readFileSync } from "fs";
import http from "http";
import https from "https";

import { createClient } from "redis"; // Redis
import { MongoClient } from "mongodb"; // MongoDB
// FCM (Firebase Cloud Messaging)

import dotenv from "dotenv";
dotenv.config({ path: "./chat.env" });

import { initializeApp, cert } from "firebase-admin/app"; // Firebase notifications.
import { getMessaging } from "firebase-admin/messaging";

console.log("\n🌟 Starting the Aesthetic Computer Chat Server 🌟\n");

const allowedHost = "chat-system.aesthetic.computer";

const dev = process.env.NODE_ENV === "development";

const subsToHandles = {};
const preAuthorized = {};
// const messages = []; // An active buffer of the last 100 messages.

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
  // "aesthetic" + ~~performance.now(),
);

let server,
  connections = {}, // All active socket connections.
  connectionId = 0;

const MONGODB_CONNECTION_STRING = process.env.MONGODB_CONNECTION_STRING;
const MONGODB_NAME = process.env.MONGODB_NAME;
// const GCM_FIREBASE_CONFIG_URL = process.env.GCM_FIREBASE_CONFIG_URL;
const redisConnectionString = process.env.REDIS_CONNECTION_STRING;

let client, db;

await makeMongoConnection();

const messages = [];
await getLast100MessagesfromMongo();
// Retrieve the last 100 messages and then buffer in the new ones.

// The main HTTP route.
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
  startChatServer();
});

async function startChatServer() {
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
      console.log("🗼 Received chat from redis:", parsed, message);
      everyone(pack(`message`, parsed));
    });
  } catch (err) {
    console.error("🔴 Could not connect to `redis` instance.", err);
  }
  // #endregion

  const wss = new WebSocketServer({ server });
  wss.on("connection", (ws, req) => {
    if (!dev && req.headers.host !== allowedHost) {
      ws.close(1008, "Policy violation"); // Close the WebSocket connection
      return;
    }

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
      let msg;
      try {
        msg = JSON.parse(data.toString());
      } catch (err) {
        console.log("🔴 Failed to parse message:", data.toString());
        return;
      }

      msg.id = id;

      console.log(
        "💬 Received:",
        msg.type,
        "from:",
        msg.content.handle,
        "of:",
        msg.content.text,
      );

      // 💬 Received an incoming chat message.
      if (msg.type === "chat:message") {
        // TODO: ❤️‍🔥 Add rate-limiting / maybe quit here if needed.

        // 🔐 1. Authorization
        // 💡️ Maybe this could be cached at some point. 24.04.02.21.30

        let authorized;

        if (preAuthorized[msg.content.sub]?.token === msg.content.token) {
          authorized = preAuthorized[msg.content.sub].user;
          console.log("🟢 Preauthorization found.");
        } else {
          console.log("🟡 Authorizing...");
          authorized = await authorize(msg.content.token);
          if (authorized)
            preAuthorized[authorized.sub] = {
              token: msg.content.token,
              user: authorized,
            };
        }

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
          // TODO: Filter message for content.
          // const out = filter(msg.content);

          const handle = "@" + (await storeMessageInMongo(msg.content));
          console.log("🟢 Message stored:", handle);

          // 🏬 Publish to redis.
          pub
            .publish(
              "chat-system",
              JSON.stringify({
                text: msg.content.text,
                handle,
              }),
            )
            .then((result) => {
              console.log("💬 Message succesfully published:", result);
              // if (!dev) {
              // ☎️ Send a notification
              console.log("🟡 Sending notification...");
              getMessaging()
                .send({
                  notification: {
                    title: "💬 Chat",
                    body: handle + " " + msg.content.text,
                  },
                  topic: "mood", // <- TODO: Eventually replace this.
                  // topic: "chat-system",
                  data: { piece: "chat" }, // This should send a tappable link to the chat piece.
                })
                .then((response) => {
                  console.log("☎️  Successfully sent notification:", response);
                })
                .catch((error) => {
                  console.log("📵  Error sending notification:", error);
                });
              //}
            })
            .catch((error) => {
              console.log("🙅‍♀️ Error publishing message:", error);
            });

          // 3. Send a confirmation back to the user.
          // No need, as this comes through redis...
        } catch (err) {
          console.error("🔴 Message could not be stored:", err);
          // TODO: Show cancellation of some kind to the user.
        }
      }
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
          messages,
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
      // console.log(response.text());
      throw new Error("🔴 Unauthorized;", response.text());
    }
  } catch {
    return undefined;
  }
}

// #region 🗺️ MongoDB

async function makeMongoConnection() {
  console.log("🟡 Connecting to MongoDB...");
  client = new MongoClient(MONGODB_CONNECTION_STRING);
  await client.connect();
  db = client.db(MONGODB_NAME);
  // return { client, db };
  console.log("🟢 Connected!");
}

// Also looks up the handle for the user and returns it.
async function storeMessageInMongo(message) {
  const fromSub = message.sub;

  console.log("🟡 Storing message...");
  const collection = db.collection("chat-system");
  await collection.createIndex({ when: 1 }); // Index for `when`.

  // Retrieve handle either from cache or MongoDB
  const handle = await getHandleFromSub(fromSub);

  const msg = {
    user: message.sub,
    text: message.text,
    when: new Date(),
  };

  // Store the chat message
  await collection.insertOne(msg);

  messages.push({ handle: "@" + handle, text: msg.text, when: msg.when });
  if (messages.length > 100) {
    messages.shift();
  }
  // console.log("Messages:", messages);
  return handle;
}

async function getLast100MessagesfromMongo() {
  console.log("🟡 Retrieving last 100 messages...");
  const chatCollection = db.collection("chat-system");
  const collectedMessages = await chatCollection
    .find({})
    .sort({ when: 1 })
    .limit(100)
    .toArray();

  for (const message of collectedMessages) {
    const fromSub = message.user;
    // Retrieve handle either from cache or MongoDB
    const handle = await getHandleFromSub(fromSub);

    messages.push({
      handle: "@" + handle,
      text: message.text,
      when: message.when,
    });
  }
}

async function getHandleFromSub(fromSub) {
  let handle;

  console.log("🟡 Looking up user record for...", fromSub);
  if (!subsToHandles[fromSub]) {
    handle = (await db.collection("@handles").findOne({ _id: fromSub }))
      ?.handle;
    console.log("🟢 Got handle from MongoDB:", handle);
    subsToHandles[fromSub] = handle;
  } else {
    handle = subsToHandles[fromSub];
    console.log("🟢 Got handle from cache:", handle);
  }

  return handle;
}

// #endregion
