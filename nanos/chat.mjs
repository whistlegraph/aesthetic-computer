// Chat, 24.03.30.14.53
// This file is a unikernel compatible chat server designed to eventually replace
// `session-server/session.mjs` by adding both websocket and udp support.
// But its first job is to be the chat server for AC.

/* #region 🏁 TODO 
  - [🔵] Add full support for and launch a `chat-clock` backend instance. 
    - [] Proofread the code.
    - [] Update any dependencies
    - [] Make a dev deployment script and emacs tab.
*/

// Management:
// https://console.cloud.google.com/compute/instances?project=aesthetic-computer

import { WebSocketServer, WebSocket } from "ws";
import { readFileSync } from "fs";

import fetch from "node-fetch";
import http from "http";
import https from "https";
import { URL } from "url";
import { StringDecoder } from "string_decoder";

import { filter } from "./filter.mjs"; // Profanity filtering.
import { redact, unredact } from "./redact.mjs";

// import { createClient } from "redis"; // Redis
import { MongoClient } from "mongodb"; // MongoDB
// FCM (Firebase Cloud Messaging)

import dotenv from "dotenv";
dotenv.config({ path: "chat.env" });

import { broadcastToTopic } from "./push.mjs"; // Standard push (no Firebase).

const MAX_MESSAGES = 500; // Maximum messages to keep in memory

const instances = {
  "chat-system": {
    name: "chat-system",
    allowedHost: "chat-system.aesthetic.computer",
    userInfoEndpoint: "https://aesthetic.us.auth0.com/userinfo",
    devPort: 8083,
  },
  "chat-sotce": {
    name: "chat-sotce",
    allowedHost: "chat.sotce.net",
    userInfoEndpoint: "https://sotce.us.auth0.com/userinfo",
    devPort: 8084,
  },
  "chat-clock": {
    name: "chat-clock",
    allowedHost: "chat-clock.aesthetic.computer",
    userInfoEndpoint: "https://aesthetic.us.auth0.com/userinfo",
    devPort: 8085,
  },
};

console.log("🔵 Env:", process.env);

process.on("uncaughtException", (error) => {
  console.error("🔴 Uncaught exception:", error);
});

process.on("unhandledRejection", (reason, promise) => {
  console.error("🔴 Unhandled rejection at:", promise, "reason:", reason);
});

const instance = instances[process.argv[2] || process.env.CHAT_INSTANCE];

if (!instance) {
  console.log("🔴 No instance data found from argument:", process.argv[2]);
  process.exit(1);
}

const dev = process.env.NODE_ENV === "development";
const filterDebug = process.env.FILTER_DEBUG === "true";

console.log(
  `\n🌟 Starting the Aesthetic Computer Chat Server for: ${instance.name} 🌟\n`,
);

if (dev) {
  console.log("🟡 Development mode.");
} else {
  console.log("🟢 Production mode.");
}

const allowedHost = instance.allowedHost;

const subsToHandles = {}; // Cached list of handles.
const subsToSubscribers = {}; // Cached list of active subscribers for this
//                               instance if supported.
const authorizedConnections = {};

let server,
  agent,
  connections = {}, // All active socket connections.
  connectionId = 0;

const MONGODB_CONNECTION_STRING = process.env.MONGODB_CONNECTION_STRING;
const MONGODB_NAME = process.env.MONGODB_NAME;

let client, db;

// 🛑 The main HTTP route.
const request = async (req, res) => {
  console.log("🕸️ Web request:", req.url);

  let url;
  try {
    url = new URL(req.url, `http://${req.headers.host}`);
  } catch (err) {
    console.error("🔴 Request URL Error:", err);
    res.writeHead(404, { "Content-Type": "application/json" });
    res.end(JSON.stringify({ status: "error", message: "Not Found" }));
    return;
  }

  const pathname = url.pathname;
  const method = req.method.toUpperCase();

  // 🪵 Logs and Root HTTP Request
  if (method === "POST" && pathname === "/log") {
    // Handle POST request to /log 🪵
    let body = "";
    const decoder = new StringDecoder("utf-8");

    req.on("data", (chunk) => {
      body += decoder.write(chunk);
    });

    req.on("end", async () => {
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

        if (messages.length > MAX_MESSAGES) messages.shift();

        // ⚠️
        // Look through the message buffer and update any handles
        // that could be changed by this log.
        // In otherr words... parse its "action".

        if (parsed.action) {
          console.log("🪵 Log action:", parsed.action, "value:", parsed.value);
          let [object, behavior] = parsed.action.split(":");
          if (!behavior) {
            behavior = object;
            object = null; // If no separator then assume behavior tag only.
          }

          console.log(
            "⛈️ Action ~ 🤖 Object:",
            object,
            "🏃 Behavior:",
            behavior,
          );

          if (
            object === "chat-system" &&
            (behavior === "mute" || behavior === "unmute")
          ) {
            const user = parsed.users[0];
            if (behavior === "mute") {
              // console.log("⚠️ TODO: NEED TO MUTE SERVER MESSAGES FOR:", user);
              messages.forEach((message) => {
                if (message.sub === user) redact(message);
              });
            } else if (behavior === "unmute") {
              // console.log("⚠️ TODO: NEED TO UNMUTE SERVER MESSAGES FOR:", user);
              messages.forEach((message) => {
                if (message.sub === user) unredact(message);
              });
            }
            // Or get last 100 messages and redo them...
            everyone(pack(parsed.action, { user }));
          }

          if (object === "handle") {
            // Update handles to subs cache.
            subsToHandles[parsed.users[0]] = parsed.value;

            if (behavior === "update" || behavior === "strip") {
              // const from =
              //  behavior === "update" ? "@" + parsed.value : parsed.value;
              let from;

              if (behavior === "update") {
                from = "@" + parsed.value;
              } else if (behavior === "strip") {
                from = "nohandle";
              }

              // const muted = await isMuted(msg.user);

              // Update messages with new handles.
              messages.forEach((message) => {
                if (message.sub === parsed.users[0]) {
                  // if (muted) redact(message);
                  message.from = from;
                }
              });

              everyone(
                pack(parsed.action, { user: parsed.users[0], handle: from }),
              );
            }
          }
        }

        everyone(pack(`message`, parsed)); // Send to everyone.

        if (instance.name === "chat-system") notify("log 🪵", parsed.text); // Push notification.

        res.writeHead(200, { "Content-Type": "application/json" });
        res.end(
          JSON.stringify({ status: "success", message: "🪵 Log received" }),
        );
      } catch (error) {
        res.writeHead(400, { "Content-Type": "application/json" });
        res.end(
          JSON.stringify({ status: "error", message: "🪵 Malformed log JSON" }),
        );
      }
    });
  } else if (method === "GET" && pathname === "/") {
    // Handle GET request to /
    const domain = req.headers.host; // Get the domain from the request
    res.writeHead(200, { "Content-Type": "text/html; charset=utf-8" });
    if (instance.name === "chat-sotce") {
      res.end(`🪷 Sotce Net\nHost: <mark>${domain}</mark>`);
    } else {
      res.end(`😱 Aesthetic Computer\nHost: <mark>${domain}</mark>`);
    }
  } else {
    // Catch-all response for other requests
    res.writeHead(404, { "Content-Type": "application/json" });
    res.end(JSON.stringify({ status: "error", message: "Not Found" }));
  }
};

if (dev) {
  const key = readFileSync("ssl/localhost-key.pem");
  const cert = readFileSync("ssl/localhost.pem");
  server = https.createServer({ key, cert }, request);
  agent = new https.Agent({ key, cert, rejectUnauthorized: false });
} else {
  server = http.createServer(request);
}

if (dev) {
  console.log("🟡 Waiting for local AC backend...");
  async function waitForBackend() {
    while (true) {
      try {
        const response = await fetch("https://localhost:8888", { agent });
        if (response.status === 200) {
          console.log("✅ Backend is ready!");
          break;
        }
      } catch (error) {
        console.log("🟠 Backend not yet available..." /*, error*/);
      }
      await new Promise((resolve) => setTimeout(resolve, 1000)); // retry every 1 second
    }
  }
  await waitForBackend();
}

await makeMongoConnection();
const messages = [];
await getLast100MessagesfromMongo();
// Retrieve the last 100 messages and then buffer in the new ones.

const port = dev ? instance.devPort : 80;

server.listen(port, "0.0.0.0", () => {
  // console.log(
  //   `--> Web server running at ${dev ? "https" : "http"}://0.0.0.0:${port} 🕷️`,
  // );
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

        // 🔇 Mute list check.
        if (await isMuted(msg.content.sub)) {
          ws.send(
            pack("muted", {
              message: `Your user has been muted and cannot send chat messages.`,
            }),
          );
          return; // Ignore long messages.
        }

        // TODO: ❤️‍🔥 Add rate-limiting / maybe quit here if needed.
        // 🧶 Length limiting.
        const len = 128;
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

        // Check to see that the user has a registered handle here
        //       otherwise return unauthorized.

        // TODO: Make sure this works across sotce-net.
        let handle, subscribed;

        if (authorized) {
          console.log("🟢 🔐 Handle authorized:", authorized);
          console.log("🚵 Finding handle for:", authorized.sub);

          // Find handle based on email.
          handle = await getHandleFromSub(authorized.sub);
          // if (bareHandle) handle = "@" + bareHandle;
          // console.log("️🐻 Bare handle is:", bareHandle);
          console.log("🚦 Checking subscription status for:", instance.name);
          if (instance.name === "chat-sotce") {
            // Also ensure that they are subscribed if the instance.name is "chat-sotce".
            // Run through the '/subscribed' endpoint from `sotce-net` and cached in `subsToSubscribers`.
            if (!subsToSubscribers[authorized.sub]) {
              const host = dev ? "https://localhost:8888" : "https://sotce.net";

              const options = {
                method: "POST",
                body: JSON.stringify({ retrieve: "subscription" }),
                headers: {
                  Authorization: "Bearer " + msg.content.token,
                  "Content-Type": "application/json",
                },
              };

              if (dev) options.agent = agent;

              const response = await fetch(
                `${host}/sotce-net/subscribed`,
                options,
              );
              if (response.status === 200) {
                const responseBody = await response.json();
                if (responseBody.subscribed) {
                  console.log("️📰 Subscribed:", responseBody);
                  subscribed = true;
                } else {
                  console.error("🗞️ Unsubscribed:", responseBody);
                  subscribed = false;
                }
              } else {
                console.error("🗞️ Unsubscribed:", response);
                subscribed = false;
              }
              subsToSubscribers[authorized.sub] = subscribed; // Cache the subscription call.
            } else {
              subscribed = subsToSubscribers[authorized.sub];
            }
          } else {
            subscribed = true;
          }
        }

        if (!authorized || !handle || !subscribed) {
          console.error(
            "🔴 Unauthorized:",
            msg.content,
            "Authorized:",
            authorized,
            "Handle:",
            handle,
            "Subscribed:",
            subscribed,
          );

          ws.send(
            pack(
              "unauthorized",
              {
                message:
                  "Your message was unauthorized, please login again and/or subscribe.",
              },
              id,
            ),
          );
          return;
        }

        // 📚 2. Persistence
        // TODO:  Add this chat to MongoDB, using the domain. (Only allow requests from the domain.)
        try {
          // 🫅 LLM Language filtering. (Could fit this in one day if it's fast enough. 24.10.31.04.32)
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
          let filteredText;
          const userIsMuted = await isMuted(fromSub);

          if (userIsMuted) {
            redact(message);
            filteredText = message.text;
          } else {
            filteredText = filter(message.text, filterDebug);
          }

          // Determine 'when' by fetching from server clock or falling back to local time
          let when;
          const clockHost = dev
            ? "https://localhost:8888"
            : "https://aesthetic.computer";
          const clockUrl = `${clockHost}/api/clock`;

          try {
            const clockResponse = await fetch(clockUrl);
            if (clockResponse.ok) {
              const serverTimeISO = await clockResponse.text();
              when = new Date(serverTimeISO);
              console.log("⏲️ Using server time...");
            } else {
              console.error(
                `🔴 Chat: Failed to fetch server time from ${clockUrl}: ${clockResponse.status} ${await clockResponse.text()}. Using local time.`,
              );
              when = new Date(); // Fallback
            }
          } catch (error) {
            console.error(
              `🔴 Chat: Error fetching server time from ${clockUrl}: ${error}. Using local time.`,
            );
            when = new Date(); // Fallback
          }

          // Don't store any actual messages to the MongoDB in development.
          // Also don't store messages from muted users.
          if (!dev && !userIsMuted) {
            console.log("🟡 Storing message...");
            const dbmsg = {
              user: message.sub,
              text: message.text, // Store unfiltered text in the database.
              when,
              font: message.font || "font_1", // 🔤 Store user's font preference
            };

            const collection = db.collection(instance.name); // Use the chat instance name for storing messages.
            await collection.createIndex({ when: 1 }); // Index for `when`.
            await collection.insertOne(dbmsg); // Store the chat message

            console.log("🟢 Message stored:", dbmsg);
          } else {
            console.log("🟡 Message not stored:", userIsMuted ? "User muted" : "Development");
          }

          // Retrieve handle either from cache or MongoDB
          const handle = await getHandleFromSub(fromSub);

          const out = {
            from: handle,
            text: filteredText,
            redactedText: message.redactedText,
            when,
            sub: fromSub, // If the chat is of a specific tenant like
            //               `chat-sotce` then the subs will be local
            //               to that tenant and not prefixed. 24.10.31.21.35
            font: message.font || "font_1", // 🔤 Include font in broadcast
          };
          
          // Check if this is a duplicate of the last message from the same user
          const lastMsg = messages[messages.length - 1];
          if (lastMsg && 
              lastMsg.sub === out.sub && 
              lastMsg.text === out.text &&
              !lastMsg.count) {
            // Increment or initialize the count
            lastMsg.count = (lastMsg.count || 1) + 1;
            // Don't add a new message, just update the existing one
            everyone(pack(`message:update`, { index: messages.length - 1, count: lastMsg.count }));
          } else {
            messages.push(out);
            if (messages.length > MAX_MESSAGES) messages.shift();
            everyone(pack(`message`, out)); // Send to clients.
          }

          // Don't send push notifications for muted users
          if (!userIsMuted) {
            if (instance.name === "chat-system")
              notify(handle + " 💬", filteredText); // Push notification.

            if (instance.name === "chat-clock") {
              const getClockEmoji = (date) => {
                let hour = date.getHours(); // 0-23
                const minutes = date.getMinutes(); // 0-59

                // Convert to 12-hour format for emoji indexing (1-12)
                let hour12 = hour % 12;
                if (hour12 === 0) {
                  hour12 = 12; // Midnight or noon is 12
                }

                let emojiCode;
                if (minutes < 30) {
                  // Use o'clock emoji for minutes 0-29
                  // 🕐 (0x1F550) to 🕛 (0x1F55B)
                  emojiCode = 0x1f550 + hour12 - 1;
                } else {
                  // Use half-past emoji for minutes 30-59
                  // 🕜 (0x1F55C) to 🕧 (0x1F567)
                  emojiCode = 0x1f55c + hour12 - 1;
                }
                return String.fromCodePoint(emojiCode);
              };

              // 'when' is the timestamp of the current message, defined a few lines above
              const clockEmoji = getClockEmoji(when);
              // console.log("The clock emoji is...!", clockEmoji);
              notify(handle + " " + clockEmoji, filteredText); // Push notification.
            }
          }

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

      everyone(pack("left", { chatters: wss.clients.size }, id));
    });

    // Send a connect message to the new client.
    // console.log("🧡 Sending connected message...", id);
    ws.send(
      pack(
        "connected",
        {
          message: `Joined \`${instance.name}\` • 🧑‍🤝‍🧑 ${wss.clients.size}`,
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
        {
          text: `${id} has joined. Connections open: ${wss.clients.size}`,
          chatters: wss.clients.size,
        },
        id,
      ),
    );
  });

  // console.log(
  //   `--> Socket server running at ${
  //     dev ? "wss" : "ws"
  //   }://0.0.0.0:${port} 🧦 \n`,
  // );

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
    const response = await fetch(instance.userInfoEndpoint, {
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
  } catch (error) {
    console.error("🔴 Authorization error:", error);
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
  console.log(`🟡 Retrieving last ${MAX_MESSAGES} combined messages...`);
  const chatCollection = db.collection(instance.name);
  let combinedMessages;

  if (instance.name === "chat-sotce") {
    // 🪷 Don't include AC logs but do reverse for chronological order.
    combinedMessages = (await chatCollection
      .find({})
      .sort({ when: -1 })
      .limit(MAX_MESSAGES)
      .toArray()).reverse();
  } else if (instance.name !== "chat-system") {
    // 🕰️ Don't include logs.
    combinedMessages = (await chatCollection
      .find({})
      .sort({ when: -1 })
      .limit(MAX_MESSAGES)
      .toArray()).reverse();
  } else {
    // chat-system
    // todo; take into account chat-clock
    // 🟪 Assume an AC chat instance with logs rolled in.
    combinedMessages = (
      await chatCollection
        .aggregate([
          {
            $unionWith: {
              coll: "logs",
              pipeline: [{ $match: {} }],
            },
          },
          { $sort: { when: -1 } },
          { $limit: MAX_MESSAGES },
        ])
        .toArray()
    ).reverse();
  }

  // Basic mutes check (`chat-system` only).
  if (instance.name === "chat-system") {
    combinedMessages.forEach(async (msg) => {
      if (await isMuted(msg.user)) redact(msg);
    });
  }

  messages.length = 0; // Clear out all messages.

  for (const message of combinedMessages) {
    let from;

    if (message.user) {
      // console.log("🗨️ User message:", message);
      // console.log(message);

      const fromSub = message.user;
      from = await getHandleFromSub(fromSub);
    } else {
      // 'logs' has a 'users' array but never a 'user' field.
      // console.log("🪵 System log:", message);
      from = message.from || "deleted";
    }

    console.log(`🔵 ${from}: "${message.text}" at ${message.when}`);

    messages.push({
      from,
      text: filter(message.text, filterDebug) || "message forgotten",
      redactedText: message.redactedText,
      when: message.when,
      font: message.font || "font_1", // 🔤 Include font from DB (default for old messages)
    });
  }
}

async function getHandleFromSub(fromSub) {
  let handle;
  // if (await isMuted(fromSub)) return "nohandle"; // Catch this on rendering.

  // console.log("🟡 Looking up user record for...", fromSub);
  if (!subsToHandles[fromSub]) {
    try {
      let prefix = "";
      if (instance.name === "chat-sotce") prefix = "sotce-";

      let host;
      if (dev) {
        host = "https://localhost:8888";
      } else {
        if (instance.name === "chat-sotce") {
          host = "https://sotce.net";
        } else {
          host = "https://aesthetic.computer";
        }
      }

      // console.log("Host:", host);
      const url = `${host}/handle?for=${prefix}${fromSub}`;
      // console.log("Fetching from url:", url);

      const options = {};
      if (dev) options.agent = agent;
      const response = await fetch(url, options);
      if (response.status === 200) {
        const data = await response.json();
        handle = data.handle;
        // console.log("🫅 Handle found:", handle);
      } else {
        // console.warn("❌ 🫅 Handle not found:", await response.json());
      }
    } catch (error) {
      // console.error("❌ 🫅 Handle retrieval error:", error);
    }

    // console.log("🟢 Got handle from network:", handle);
    subsToHandles[fromSub] = handle;
  } else {
    handle = subsToHandles[fromSub];
    // console.log("🟢 Got handle from cache:", handle);
  }

  return "@" + handle;
}

// #endregion

function notify(title, body) {
  if (!dev && db) {
    // ☎️ Send a notification
    console.log("🟡 Sending push notification...", performance.now());

    broadcastToTopic(db, "mood", {
      // <- TODO: Eventually use a wider-range topic.
      title,
      body,
      urgent: true, // time-sensitive on iOS, Urgency: high on web
      ttl: 0, // don't store undelivered chat pings
      data: { piece: "chat" }, // Tappable link to the chat piece.
    })
      .then((summary) => {
        console.log(
          "☎️  Successfully sent notification:",
          summary,
          performance.now(),
        );
      })
      .catch((error) => {
        console.log(
          "📵  Error sending notification:",
          error,
          performance.now(),
        );
      });
  }
}

// Check the database to see if a given user sub has been muted for this instance.
async function isMuted(sub) {
  if (!sub) return false;
  try {
    const mutesCollection = db.collection(instance.name + "-mutes");
    const mute = await mutesCollection.findOne({ user: sub });
    // console.log("Sub:", sub, "Is muted?", !!mute);
    return !!mute;
  } catch (error) {
    return false; // Collection doesn't exist or another error occurred
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
