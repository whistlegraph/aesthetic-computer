// Session Server, 23.12.04.14.57
// Represents a "room" or user or "client" backend
// which at the moment is run once for every "piece"
// that requests it.

/* #region todo
 + Now
 + Possible Concerns
 - [?] `code.channel` should return a promise, and wait for a
      `code-channel:subbed`.
    event here? This way users get better confirmation if the socket
    doesn't go through or if there is a server issue. 23.07.04.18.01
    (Might not actually be that necessary.)
 + Done
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
import { WebSocket, WebSocketServer } from "ws";
import ip from "ip";
import chokidar from "chokidar";
import fs from "fs";
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

const { keys } = Object;
let fastify, termkit, term;

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
  try {
    termkit = (await import("terminal-kit")).default;
  } catch (err) {
    error("Failed to load terminal-kit", error);
  }
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

const info = {
  port: process.env.PORT, // 8889 in development via `package.json`
  name: process.env.SESSION_BACKEND_ID,
  service: process.env.JAMSOCKET_SERVICE,
};

const codeChannels = {}; // Used to filter `code` updates from redis to
//                          clients who explicitly have the channel set.

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
  exec(
    "cd /home/aesthetic.computer/session-server; pm2 stop all; git pull; npm install; pm2 start all",
    (err, stdout, stderr) => {
      if (err) {
        error(`exec error: ${error}`);
        return;
      }
      log(`stdout: ${stdout}`);
      error(`stderr: ${stderr}`);
    },
  );

  reply.send({ status: "ok" });
});

fastify.get("/", async () => {
  return {
    msg: "Hello, and welcome to an aesthetic.computer session... server instance!?",
  };
});

// *** Live Reload of Pieces in Development ***
if (dev) {
  fastify.post("/reload", async (req) => {
    everyone(pack("reload", req.body, "pieces"));
    return { msg: "Reload request sent!", body: req.body };
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
    client.ping();
  });
}, 15000); // 15 second pings from server before termination.

wss.on("close", function close() {
  clearInterval(interval);
  connections = {};
});

// Construct the server.
wss.on("connection", (ws, req) => {
  const ip = req.socket.remoteAddress || "localhost"; // beautify ip
  ws.isAlive = true; // For checking persistence between ping-pong messages.

  ws.on("pong", () => {
    ws.isAlive = true;
  }); // Receive a pong and stay alive!

  // Assign the conection a unique id.
  connections[connectionId] = ws;
  const id = connectionId;
  let codeChannel; // Used to subscribe to incoming piece code.

  log("🧏 Someone joined:", `${id}:${ip}`, "Online:", wss.clients.size, "🫂");

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
      JSON.stringify({ id, ip, playerCount: content.playerCount }),
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
    const msg = JSON.parse(data.toString());
    msg.id = id; // TODO: When sending a server generated message, use a special id.
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
              notification: { title: "😱 Scream", body: out },
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
    } else if (msg.type === "location:broadcast") {
      // Receive a slug location for this handle.
      if (msg.content.slug !== "*keep-alive*") {
        log("🗼 Slug:", msg.content.slug, "from:", msg.content.handle);
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
        if (label === "write") msg.content = filter(msg.content);

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

          // ❤️‍🔥 TODO: No need to send the current user back via `list` here.

          if (!pickedUpConnection)
            ws.send(pack(`world:${piece}:list`, worldClients[piece], id));
          // ^ Send existing list to just this user.

          if (!pickedUpConnection) worldClients[piece][id] = { ...msg.content };
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

        // All world: messages are only broadcast to "others".
        others(JSON.stringify(msg));
        return;
      }

      everyone(JSON.stringify(msg)); // Relay any other message to every user.
    }
  });

  // More info: https://stackoverflow.com/a/49791634/8146077
  ws.on("close", () => {
    log("🚪 Someone left:", id, "Online:", wss.clients.size, "🫂");

    // Delete the user from the worldClients pieces index.
    // keys(worldClients).forEach((piece) => {
    //   delete worldClients[piece][id];
    //   if (keys(worldClients[piece]).length === 0)
    //     delete worldClients[piece];
    // });

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
            everyone(pack(`world:${piece}:kick`, { id })); // Kick this ghost.
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
                  everyone(pack(`world:${piece}:slug`, { id, handle, slug }));
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
          everyone(pack(`world:${piece}:ghost`, { id }));
        } else {
          // Delete the user from the worldClients pieces index.
          delete worldClients[piece][id];
          if (keys(worldClients[piece]).length === 0)
            delete worldClients[piece];
        }
      }
    });

    // Send a message to everyone else on the server that this client left.
    if (!ghosted) everyone(pack("left", { id, count: wss.clients.size }));

    // Delete from the connection index.
    delete connections[id];

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

// 🧚 UDP Server (using Twilio ICE servers)
// #region udp

// Note: This currently works off of a monolith via `udp.aesthetic.computer`
//       as the ports are blocked on jamstack.

import geckos from "@geckos.io/server";

const io = geckos();

io.addServer(server); // Hook up to the HTTP Server.
// io.listen(9208); // default port is 9208

io.onConnection((channel) => {
  channel.onDisconnect(() => {
    log(`🩰 ${channel.id} got disconnected`);
  });

  // Just for testing via the aesthetic `udp` piece for now.
  channel.on("fairy:point", (data) => {
    // See docs here: https://github.com/geckosio/geckos.io#reliable-messages
    // TODO: - [] Learn about the differences between channels and rooms.

    // emit the to all channels in the same room except the sender
    // log(`🩰 fairy:point - ${data}`);
    channel.broadcast.emit("fairy:point", data);
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
        const piece = path.split("/").pop().replace(".mjs", "");
        everyone(pack("reload", { piece: piece || "*" }, "local"));
      }
    });

  // 2. Watch base system files.
  chokidar
    .watch([
      "../system/netlify/functions",
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
  if (!term) {
    console.log(...arguments);
    return;
  }

  // tkit
}

function error() {
  if (!term) {
    console.error(...arguments);
    return;
  }

  // tkit
}
