// Chat, 24.03.30.14.53
// This file is a unikernal compatible chat server designed to eventually replace
// `session-server/session.mjs` by adding both websocket and udp support.
// But its first job is to be the chat server for AC.

/* #region 🏁 TODO 
 - [🟠] Update nanos versions / dependencies in this dir.
 - [] Set up logging so I know why this server is crashing.
   - [] Maybe it's a setting in Google Cloud to log the serial console. 
   - [] Also check the nanos logs.
   - [] Or maybe I need to use an external service. 
  - [] Set up another instance of this chat for sotce-net that...
    - [] Will have a different subdomain setup so `conductor` will need updates. 
    - [] How will it know what version it's running both in production and in development?
    - [] Run the sotce-net instance in development in addition to the AC one in emacs.
    - [] Will also have good production logging support. 
    - [] Will use a different table in the database like `chat-sotce-net` instead
         of chat-system.
    - [] Will require subscriber authorization from `sotce-net` users (code can be found in`sotce-net.mjs`)
         if running that instance in order to actually send messages but not to join or
         observe chats.
    - [] Add a new command to the `package.json`.
    - [] Add a basic client to `sotce-net`.
      - [] Write it as a totally separate UI layer that always connects.
      - [] Have it on the loged out page grayed out, the logged in page opaque
           and scrollable, and the subscriber page interactable.
      - [] Messages should make a sound.
        - [] Bring in AC sound engine here.
    - [] Add web notficiations to `sotce-net` chat.
  - [] Add images to AC chat.
  - [] Add textual links to AC chat.
#endregion */

// Management:
// https://console.cloud.google.com/compute/instances?project=aesthetic-computer

import { WebSocketServer, WebSocket } from "ws";
import { promises as fs, readFileSync } from "fs";

import http from "http";
import https from "https";
import { URL } from "url";
import { StringDecoder } from "string_decoder";

import { filter } from "./filter.mjs"; // Profanity filtering.

// import { createClient } from "redis"; // Redis
import { MongoClient } from "mongodb"; // MongoDB
// FCM (Firebase Cloud Messaging)

import dotenv from "dotenv";
dotenv.config({ path: "chat.env" });

import { initializeApp, cert } from "firebase-admin/app"; // Firebase notifications.
import { getMessaging } from "firebase-admin/messaging";

console.log("\n🌟 Starting the Aesthetic Computer Chat Server 🌟\n");

const allowedHost = "chat-system.aesthetic.computer";

const dev = process.env.NODE_ENV === "development";

const subsToHandles = {};
const authorizedConnections = {};
// const messages = []; // An active buffer of the last 100 messages.

// let serviceAccount;
// try {
//   console.log("🔥 Fetching Firebase configuration from:", process.env.GCM_FIREBASE_CONFIG_URL);
//   const response = await fetch(process.env.GCM_FIREBASE_CONFIG_URL);
//   if (!response.ok) {
//     throw new Error(`HTTP error! Status: ${response.status}`);
//   }
//   serviceAccount = await response.json();
// } catch (error) {
//   console.error("Error fetching service account:", error);
//   // Handle the error as needed
// }

let serviceAccount;
try {
  console.log(
    "🔥 Loading Firebase configuration from file: ./gcp-service-key.json",
  );
  const data = await fs.readFile("./gcp-firebase-service-key.json", "utf8");
  serviceAccount = JSON.parse(data);
} catch (error) {
  console.error("Error loading service account:", error);
  // Handle the error as needed
}

console.log("🔥 Initializing Firebase App from:", serviceAccount);

initializeApp(
  { credential: cert(serviceAccount) }, //,
  // "aesthetic" + ~~performance.now(),
);

console.log("🔥 Firebase App initialized...");

let server,
  connections = {}, // All active socket connections.
  connectionId = 0;

const MONGODB_CONNECTION_STRING = process.env.MONGODB_CONNECTION_STRING;
const MONGODB_NAME = process.env.MONGODB_NAME;
// const GCM_FIREBASE_CONFIG_URL = process.env.GCM_FIREBASE_CONFIG_URL;
// const redisConnectionString = process.env.REDIS_CONNECTION_STRING;

let client, db;

await makeMongoConnection();

const messages = [];
await getLast100MessagesfromMongo();
// Retrieve the last 100 messages and then buffer in the new ones.

// 🛑 The main HTTP route.
const request = async (req, res) => {
  const url = new URL(req.url, `http://${req.headers.host}`);
  const pathname = url.pathname;
  const method = req.method.toUpperCase();

  // 🪵 Logs
  if (method === "POST" && pathname === "/log") {
    // Handle POST request to /log 🪵
    let body = "";
    const decoder = new StringDecoder("utf-8");

    req.on("data", (chunk) => {
      body += decoder.write(chunk);
    });

    req.on("end", () => {
      body += decoder.end();
      const authHeader = req.headers["authorization"];
      const token = authHeader && authHeader.split(" ")[1];

      if (token !== process.env.LOGGER_KEY) {
        res.writeHead(403, { "Content-Type": "application/json" });
        res.end(JSON.stringify({ status: "error", message: "😇 Forbidden!" }));
        return;
      }

      try {
        const parsed = JSON.parse(body);
        console.log("🪵 Received log data:", parsed);

        // 🪧🪵 Respond to log.

        messages.push(parsed);

        if (messages.length > 100) messages.shift();

        // ⚠️
        // Look through the message buffer and update any handles
        // that could be changed by this log.
        // In otherr words... parse its "action".

        if (parsed.action) {
          console.log("🪵 Log action:", parsed.action, "value:", parsed.value);
          const [object, behavior] = parsed.action.split(":");

          console.log(
            "⛈️ Action ~ 🤖 Object:",
            object,
            "🏃 Behavior:",
            behavior,
          );

          if (object === "handle") {
            // Update handles to subs cache.
            subsToHandles[parsed.users[0]] = parsed.value;

            if (behavior === "update" || behavior === "strip") {
              const from =
                behavior === "update" ? "@" + parsed.value : parsed.value;
              // Update messages with new handles.
              messages.forEach((message) => {
                if (message.sub === parsed.users[0]) message.from = from;
              });
              everyone(
                pack(parsed.action, { user: parsed.users[0], handle: from }),
              );
            }
          }
        }

        everyone(pack(`message`, parsed)); // Send to everyone.
        notify("log 🪵", parsed.text); // Push notification.

        res.writeHead(200, { "Content-Type": "application/json" });
        res.end(
          JSON.stringify({ status: "success", message: "🪵 Log received" }),
        );
      } catch (error) {
        res.writeHead(400, { "Content-Type": "application/json" });
        res.end(
          JSON.stringify({ status: "error", message: "🪵 Malforned log JSON" }),
        );
      }
    });
  } else if (method === "GET" && pathname === "/") {
    // Handle GET request to /
    const domain = req.headers.host; // Get the domain from the request
    res.writeHead(200, { "Content-Type": "text/html; charset=utf-8" });
    res.end(`😱 Aesthetic Computer\nHost: <mark>${domain}</mark>`);
  } else {
    // Catch-all response for other requests
    res.writeHead(404, { "Content-Type": "application/json" });
    res.end(JSON.stringify({ status: "error", message: "Not Found" }));
  }
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

let everyone;

async function startChatServer() {
  // #region 🏬 Redis
  // *** Start up two `redis` clients. (One for subscribing, and for publishing)
  // const sub = !dev
  //   ? createClient({ url: redisConnectionString })
  //   : createClient();
  // const pub = !dev
  //   ? createClient({ url: redisConnectionString })
  //   : createClient();

  //let presubscribed = false;

  //async function subscribe() {
  // if (presubscribed) return;
  // presubscribed = true;
  // sub
  //   .subscribe("log", (message) => {
  //     console.log("🪵️ Received log from redis:", message);
  //     const parsed = JSON.parse(message);
  //     messages.push(parsed);
  //     if (messages.length > 100) messages.shift();
  //     everyone(pack(`message`, parsed));
  //     notify("system 💬", parsed.text); // Push notification.
  //   })
  //   .then(() => {
  //     console.log("🪵 Subscribed to `log` updates from redis.");
  //   })
  //   .catch((err) => {
  //     console.error("🪵 Could not subscribe to `log` updates.", err);
  //     presubscribed = false;
  //   });
  //}

  // const createRedisClient = (role) => {
  //   const client = createClient({
  //     url: redisConnectionString,
  //     socket: {
  //       reconnectStrategy: (retries) => {
  //         console.log(`🔄 ${role} Redis client reconnect attempt: ${retries}`);
  //         return Math.min(retries * 50, 3000);
  //       },
  //     },
  //   });

  //   client.on("connect", async () => {
  //     console.log(`🟢 \`${role}\` Redis client connected successfully.`);
  //     // if (role === "subscriber") subscribe();
  //   });

  //   client.on("error", (err) => {
  //     console.log(`🔴 \`${role}\` Redis client connection failure!`, err);
  //     // if (role === "subscriber") presubscribed = false;
  //   });

  //   return client;
  // };

  // const sub = createRedisClient("subscriber");
  // const pub = createRedisClient("publisher");

  //try {
  // await sub.connect();
  // subscribe();
  // await pub.connect();
  //} catch (err) {
  //  console.error("🔴 Could not connect to `redis` instance.", err);
  //}
  // #endregion

  const wss = new WebSocketServer({ server });
  wss.on("connection", (ws, req) => {
    if (!dev && req.headers.host !== allowedHost) {
      ws.close(1008, "Policy violation"); // Close the WebSocket connection
      return;
    }

    // Send a message to all other clients except this one.
    function others(string) {
      wss.clients.forEach((c) => {
        if (c !== ws && c?.readyState === WebSocket.OPEN) c.send(string);
      });
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

    ws.on("message", async (data, more) => {
      let msg;
      try {
        msg = JSON.parse(data.toString());
      } catch (err) {
        console.log(
          "🔴 Failed to parse JSON message...",
          data,
          data.length,
          more,
        );
        return;
      }

      msg.id = id;

      if (msg.type === "logout") {
        console.log("🏃‍♀️ User logged out...");
        delete authorizedConnections[id];
      } else if (msg.type === "chat:message") {
        // 💬 Received an incoming chat message.
        console.log(
          "💬 Received:",
          msg.type,
          "from:",
          msg.content.sub,
          "of:",
          msg.content.text,
        );
        // TODO: ❤️‍🔥 Add rate-limiting / maybe quit here if needed.
        // 🧶 Length limiting.
        const len = 96;
        if (msg.content.text.length > len) {
          ws.send(
            pack("too-long", {
              message: `Your message was too long, please limit it to ${len} characters.`,
            }),
          );
          return; // Silently ignore long messages.
        }

        // 🔐 1. Authorization
        // 💡️ These are cached in a "preAuthorized" section.

        let authorized;

        if (
          authorizedConnections[id /*msg.content.sub*/]?.token ===
          msg.content.token
        ) {
          authorized = authorizedConnections[id /*msg.content.sub*/].user;
          console.log("🟢 Preauthorization found.");
        } else {
          console.log("🟡 Authorizing...");
          authorized = await authorize(msg.content.token);
          if (authorized)
            authorizedConnections[id /*authorized.sub*/] = {
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

          // 🫅 LLM Language filtering.
          // Call out to `ask` to filter for content.
          /*
          let filteredText = "";
          let messages = [];
          messages.push({
            by: "system",
            text: "respond with the exact user text, but filter any profanities or inappropriate language with underscores - and pay attention to cases where profanities may be separated by punctuation or obscured",
          });
          messages.push({ by: "user", text: msg.content.text });

          const host = dev
            ? `https://localhost:8888` // Point to the main netlify stack.
            : "https://ai.aesthetic.computer";

          if (dev) process.env.NODE_TLS_REJECT_UNAUTHORIZED = '0';

          try {
            const response = await fetch(`${host}/api/ask`, {
              method: "POST",
              headers: { "Content-Type": "application/json" },
              body: JSON.stringify({ messages, hint: "filter" }),
            });

            if (!response.ok) {
              throw new Error(`Failed to reply: ${response.status}`);
            }

            const readableStream = response.body;
            const decoder = new TextDecoder();
            const reader = readableStream.getReader();

            while (true) {
              const { done, value } = await reader.read();
              if (done) {
                // convo.controller = null;
                // if (!convo.forgetful) {
                //   convo.messages.push({
                //     by: "system",
                //     text: filteredText,
                //   });
                // }
                break;
              }
              const got = decoder.decode(value, { stream: true });
              filteredText += got;
              console.log("🎣 Filtering:", filteredText);
            }
          } catch (error) {
            console.log("Error:", error);
            // reportFailure(error);
          }
          */

          const message = msg.content;
          const fromSub = message.sub;

          const filteredText = filter(message.text);

          // Don't store any actual messages to the MongoDB in development.
          if (!dev) {
            console.log("🟡 Storing message...");
            const msg = {
              user: message.sub,
              text: message.text, // Store unfiltered text in the database.
              when: new Date(),
            };

            const collection = db.collection("chat-system");
            await collection.createIndex({ when: 1 }); // Index for `when`.
            await collection.insertOne(msg); // Store the chat message

            console.log("🟢 Message stored:", msg);
          } else {
            console.log("🟡 Message not stored:", "Development");
          }

          // Retrieve handle either from cache or MongoDB
          const handle = "@" + (await getHandleFromSub(fromSub));
          const out = {
            from: handle,
            text: filteredText,
            when: msg.when,
            sub: fromSub,
          };
          messages.push(out);
          if (messages.length > 100) messages.shift();

          everyone(pack(`message`, out)); // Send to clients.
          notify(handle + " 💬", filteredText); // Push notification.

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
      delete authorizedConnections[id];

      console.log(
        "🚪 Closed connection:",
        id,
        "Online:",
        wss.clients.size,
        "🫂",
      );

      everyone(pack("left", { count: wss.clients.size }, id));
    });

    // Send a connect message to the new client.
    // console.log("🧡 Sending connected message...", id);
    ws.send(
      pack(
        "connected",
        {
          message: `Joined \`chat-system\` • 🧑‍🤝‍🧑 ${wss.clients.size}`,
          chatters: wss.clients.size,
          messages,
          id,
        },
        id,
      ),
    );

    // Send a join message to everyone else.
    others(
      pack(
        "joined",
        JSON.stringify({
          text: `${id} has joined. Connections open: ${wss.clients.size}`,
        }),
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
  everyone = (string) => {
    wss.clients.forEach((c) => {
      if (c?.readyState === WebSocket.OPEN) c.send(string);
    });
  };
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

async function getLast100MessagesfromMongo() {
  console.log("🟡 Retrieving last 100 combined messages...");
  const chatCollection = db.collection("chat-system");
  // const logsCollection = db.collection("logs");

  const combinedMessages = (
    await chatCollection
      .aggregate([
        {
          $unionWith: {
            coll: "logs",
            pipeline: [{ $match: {} }],
          },
        },
        { $sort: { when: -1 } },
        { $limit: 100 },
      ])
      .toArray()
  ).reverse();

  for (const message of combinedMessages) {
    let from;

    if (message.user) {
      console.log("🗨️ User message:", message);
      const fromSub = message.user;
      const handle = await getHandleFromSub(fromSub);
      from = "@" + handle;
    } else {
      console.log("🪵 System log:", message);
      from = message.from || "deleted";
    }

    messages.push({
      from,
      text: filter(message.text) || "message forgotten",
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

function notify(title, body) {
  if (!dev) {
    // ☎️ Send a notification
    console.log("🟡 Sending notification...");
    // TODO: Test notification icons here.
    // const topicName = "industry-tech";

    getMessaging()
      .send({
        notification: { title, body },
        // android: {
        //   notification: {
        //     imageUrl: "https://aesthetic.computer/api/logo.png",
        //   },
        apns: {
          payload: {
            aps: { "mutable-content": 1 },
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
  }
}

// 🪦 Graveyard
// 🏬 Publish to redis.
// pub
//   .publish("chat-system", JSON.stringify(update))
//   .then((result) => {
// console.log("💬 Message succesfully published:", result);
// })
// .catch((error) => {
// console.log("🙅‍♀️ Error publishing message:", error);
// });
