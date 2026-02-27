// Chat Manager, 25.11.28
// Multi-instance chat support for session-server
// Adapted from nanos/chat.mjs to run multiple chat instances in one process

import { WebSocket } from "ws";
import { promises as fs } from "fs";
import fetch from "node-fetch";
import https from "https";

import { filter } from "./filter.mjs";
import { redact, unredact } from "./redact.mjs";
import { ensureIndexes as ensureHeartsIndexes, toggleHeart, countHearts } from "./hearts.mjs";

import { MongoClient, ObjectId } from "mongodb";
import { getMessaging } from "firebase-admin/messaging";

const MAX_MESSAGES = 500;

// Chat instance configurations
export const chatInstances = {
  "chat-system.aesthetic.computer": {
    name: "chat-system",
    allowedHost: "chat-system.aesthetic.computer",
    userInfoEndpoint: "https://aesthetic.us.auth0.com/userinfo",
    topic: "mood",
  },
  "chat.sotce.net": {
    name: "chat-sotce",
    allowedHost: "chat.sotce.net",
    userInfoEndpoint: "https://sotce.us.auth0.com/userinfo",
    topic: "mood",
  },
  "chat-clock.aesthetic.computer": {
    name: "chat-clock",
    allowedHost: "chat-clock.aesthetic.computer",
    userInfoEndpoint: "https://aesthetic.us.auth0.com/userinfo",
    topic: "mood",
  },
};

// Development host mappings (localhost ports)
const devHostMappings = {
  "localhost:8083": "chat-system.aesthetic.computer",
  "localhost:8084": "chat.sotce.net",
  "localhost:8085": "chat-clock.aesthetic.computer",
};

export class ChatManager {
  constructor(options = {}) {
    this.dev = options.dev || false;
    this.filterDebug = options.filterDebug || false;
    this.loggerKey = options.loggerKey || process.env.LOGGER_KEY;
    this.activityEmitter = options.activityEmitter || null;
    
    // MongoDB
    this.mongoClient = null;
    this.db = null;
    this.mongoConnectionString = options.mongoConnectionString || process.env.MONGODB_CONNECTION_STRING;
    this.mongoDbName = options.mongoDbName || process.env.MONGODB_NAME || "aesthetic";
    
    // HTTPS agent for dev mode
    this.agent = this.dev ? new https.Agent({ rejectUnauthorized: false }) : null;
    
    // Per-instance state
    this.instances = {};
    for (const [host, config] of Object.entries(chatInstances)) {
      this.instances[host] = {
        config,
        messages: [],
        connections: {},
        connectionId: 0,
        authorizedConnections: {},
        subsToHandles: {},
        subsToSubscribers: {},
      };
    }
    
    console.log("💬 ChatManager initialized with instances:", Object.keys(chatInstances).join(", "));
  }

  setActivityEmitter(emitter) {
    this.activityEmitter = typeof emitter === "function" ? emitter : null;
  }

  emitActivity(payload) {
    if (!this.activityEmitter) return;
    try {
      this.activityEmitter(payload);
    } catch (err) {
      console.error("💬 Activity emitter failure:", err);
    }
  }

  async init() {
    // Connect to MongoDB
    if (this.mongoConnectionString) {
      try {
        console.log("💬 Connecting to MongoDB...");
        this.mongoClient = new MongoClient(this.mongoConnectionString);
        await this.mongoClient.connect();
        this.db = this.mongoClient.db(this.mongoDbName);
        console.log("💬 MongoDB connected!");
        
        // Ensure hearts indexes exist
        await ensureHeartsIndexes(this.db);

        // Load messages for each instance
        for (const [host, instance] of Object.entries(this.instances)) {
          await this.loadMessages(instance);
        }
      } catch (err) {
        console.error("💬 MongoDB connection failed:", err);
      }
    } else {
      console.log("💬 No MongoDB connection string, running without persistence");
    }
  }

  async loadMessages(instance) {
    if (!this.db) return;
    
    const collectionName = instance.config.name;
    console.log(`💬 Loading messages for ${collectionName}...`);
    
    try {
      const chatCollection = this.db.collection(collectionName);
      let combinedMessages;

      if (collectionName === "chat-sotce") {
        combinedMessages = (await chatCollection
          .find({})
          .sort({ when: -1 })
          .limit(MAX_MESSAGES)
          .toArray()).reverse();
      } else if (collectionName !== "chat-system") {
        combinedMessages = (await chatCollection
          .find({})
          .sort({ when: -1 })
          .limit(MAX_MESSAGES)
          .toArray()).reverse();
      } else {
        // chat-system includes logs
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

      // Deduplicate messages (removes duplicates caused by persist-on-shutdown bug)
      {
        const seen = new Set();
        combinedMessages = combinedMessages.filter((msg) => {
          const key = `${msg.user || msg.from}:${msg.text}:${msg.when?.getTime?.() ?? msg.when}`;
          if (seen.has(key)) return false;
          seen.add(key);
          return true;
        });
      }

      // Check mutes for chat-system
      if (collectionName === "chat-system") {
        for (const msg of combinedMessages) {
          if (await this.isMuted(instance, msg.user)) {
            redact(msg);
          }
        }
      }

      instance.messages = [];
      for (const message of combinedMessages) {
        let from;
        if (message.user) {
          from = await this.getHandleFromSub(instance, message.user);
        } else {
          from = message.from || "deleted";
        }

        const msg = {
          from,
          text: message.deleted
            ? "[deleted]"
            : filter(message.text, this.filterDebug) || "message forgotten",
          redactedText: message.redactedText,
          when: message.when,
          sub: message.user || undefined,
          font: message.font || "font_1", // 🔤 Include font from DB (default for old messages)
        };
        if (message._id) msg.id = message._id.toString();
        if (message.deleted) msg.deleted = true;
        instance.messages.push(msg);
      }
      
      console.log(`💬 Loaded ${instance.messages.length} messages for ${collectionName}`);
    } catch (err) {
      console.error(`💬 Failed to load messages for ${collectionName}:`, err);
    }
  }

  // Check if a host should be handled by chat manager
  isChatHost(host) {
    // Direct match
    if (chatInstances[host]) return true;
    // Dev mapping
    if (this.dev && devHostMappings[host]) return true;
    return false;
  }

  // Get the instance for a host
  getInstance(host) {
    // Direct match
    if (this.instances[host]) return this.instances[host];
    // Dev mapping
    if (this.dev && devHostMappings[host]) {
      return this.instances[devHostMappings[host]];
    }
    return null;
  }

  // Handle a new WebSocket connection
  async handleConnection(ws, req) {
    const host = req.headers.host;
    const instance = this.getInstance(host);
    
    if (!instance) {
      console.log("💬 Unknown chat host:", host);
      ws.close(1008, "Unknown host");
      return;
    }

    // Validate host in production
    if (!this.dev && host !== instance.config.allowedHost) {
      ws.close(1008, "Policy violation");
      return;
    }

    const id = instance.connectionId++;
    instance.connections[id] = ws;
    
    const ip = req.socket.remoteAddress || "localhost";
    ws.isAlive = true;

    ws.on("pong", () => {
      ws.isAlive = true;
    });

    console.log(
      `💬 [${instance.config.name}] Connection ${id} from ${ip}, online: ${Object.keys(instance.connections).length}`
    );

    // Set up ping interval for this connection
    const pingInterval = setInterval(() => {
      if (ws.isAlive === false) {
        clearInterval(pingInterval);
        return ws.terminate();
      }
      ws.isAlive = false;
      ws.ping();
    }, 15000);

    ws.on("message", async (data) => {
      await this.handleMessage(instance, ws, id, data);
    });

    ws.on("close", () => {
      clearInterval(pingInterval);
      delete instance.connections[id];
      delete instance.authorizedConnections[id];

      console.log(
        `💬 [${instance.config.name}] Connection ${id} closed, online: ${Object.keys(instance.connections).length}`
      );

      // Broadcast updated online handles after removing this connection
      this.broadcastOnlineHandles(instance);
      
      this.broadcast(instance, this.pack("left", { 
        chatters: Object.keys(instance.connections).length,
        handles: this.getOnlineHandles(instance)
      }, id));
    });

    // Load heart counts for current message window
    let heartCounts = {};
    if (this.db) {
      const messageIds = instance.messages.filter((m) => m.id).map((m) => m.id);
      heartCounts = await countHearts(this.db, instance.config.name, messageIds);
    }

    // Send welcome message
    ws.send(
      this.pack(
        "connected",
        {
          message: `Joined \`${instance.config.name}\` • 🧑‍🤝‍🧑 ${Object.keys(instance.connections).length}`,
          chatters: Object.keys(instance.connections).length,
          handles: this.getOnlineHandles(instance),
          messages: instance.messages,
          heartCounts,
          id,
        },
        id,
      ),
    );

    // Notify others
    this.broadcastOthers(instance, ws, this.pack(
      "joined",
      {
        text: `${id} has joined. Connections open: ${Object.keys(instance.connections).length}`,
        chatters: Object.keys(instance.connections).length,
        handles: this.getOnlineHandles(instance),
      },
      id,
    ));
  }

  async handleMessage(instance, ws, id, data) {
    let msg;
    try {
      msg = JSON.parse(data.toString());
    } catch (err) {
      console.log("💬 Failed to parse message:", err);
      return;
    }

    msg.id = id;

    if (msg.type === "logout") {
      console.log(`💬 [${instance.config.name}] User logged out`);
      delete instance.authorizedConnections[id];
    } else if (msg.type === "chat:message") {
      await this.handleChatMessage(instance, ws, id, msg);
    } else if (msg.type === "chat:delete") {
      await this.handleDeleteMessage(instance, ws, id, msg);
    } else if (msg.type === "chat:heart") {
      await this.handleChatHeart(instance, ws, id, msg);
    }
  }

  async handleChatMessage(instance, ws, id, msg) {
    console.log(
      `💬 [${instance.config.name}] Message from ${msg.content.sub} (${msg.content.text.length} chars)`
    );

    // Mute check
    if (await this.isMuted(instance, msg.content.sub)) {
      ws.send(this.pack("muted", { message: "Your user has been muted." }));
      return;
    }

    // Length limit
    const len = 128;
    if (msg.content.text.length > len) {
      ws.send(this.pack("too-long", { message: `Please limit to ${len} characters.` }));
      return;
    }

    // Authorization
    let authorized;
    if (instance.authorizedConnections[id]?.token === msg.content.token) {
      authorized = instance.authorizedConnections[id].user;
      console.log("💬 Pre-authorized");
    } else {
      console.log("💬 Authorizing...");
      authorized = await this.authorize(instance, msg.content.token);
      if (authorized) {
        instance.authorizedConnections[id] = {
          token: msg.content.token,
          user: authorized,
        };
      }
    }

    // Get handle
    let handle, subscribed;
    if (authorized) {
      handle = await this.getHandleFromSub(instance, authorized.sub);
      
      // Store handle in authorizedConnections for online list
      instance.authorizedConnections[id].handle = handle;
      
      // Broadcast updated online handles to all clients
      this.broadcastOnlineHandles(instance);
      
      // Subscription check for sotce
      if (instance.config.name === "chat-sotce") {
        subscribed = await this.checkSubscription(instance, authorized.sub, msg.content.token);
      } else {
        subscribed = true;
      }
    }

    if (!authorized || !handle || !subscribed) {
      console.error("💬 Unauthorized:", { authorized: !!authorized, handle, subscribed });
      ws.send(this.pack("unauthorized", { message: "Please login and/or subscribe." }, id));
      return;
    }

    // Process and store message
    try {
      const message = msg.content;
      const fromSub = message.sub;
      let filteredText;
      const userIsMuted = await this.isMuted(instance, fromSub);

      if (userIsMuted) {
        redact(message);
        filteredText = message.text;
      } else {
        filteredText = filter(message.text, this.filterDebug);
      }

      // Get server time
      let when = new Date();
      if (!this.dev) {
        try {
          const clockResponse = await fetch("https://aesthetic.computer/api/clock");
          if (clockResponse.ok) {
            const serverTimeISO = await clockResponse.text();
            when = new Date(serverTimeISO);
          }
        } catch (err) {
          console.log("💬 Clock fetch failed, using local time");
        }
      }

      // Store in MongoDB (production only, non-muted users)
      let insertedId;
      if (!this.dev && !userIsMuted && this.db) {
        const dbmsg = {
          user: message.sub,
          text: message.text,
          when,
          font: message.font || "font_1", // 🔤 Store user's font preference
        };
        const collection = this.db.collection(instance.config.name);
        await collection.createIndex({ when: 1 });
        const result = await collection.insertOne(dbmsg);
        insertedId = result.insertedId?.toString();
        console.log("💬 Message stored");
      }

      const out = {
        from: handle,
        text: filteredText,
        redactedText: message.redactedText,
        when,
        sub: fromSub,
        font: message.font || "font_1", // 🔤 Include font in broadcast
      };
      if (insertedId) out.id = insertedId;

      // Duplicate detection
      const lastMsg = instance.messages[instance.messages.length - 1];
      if (lastMsg && lastMsg.sub === out.sub && lastMsg.text === out.text && !lastMsg.count) {
        lastMsg.count = (lastMsg.count || 1) + 1;
        this.broadcast(instance, this.pack("message:update", { index: instance.messages.length - 1, count: lastMsg.count }));
      } else {
        instance.messages.push(out);
        if (instance.messages.length > MAX_MESSAGES) instance.messages.shift();
        this.broadcast(instance, this.pack("message", out));
      }

      if (handle && handle.startsWith("@")) {
        const compactText = `${filteredText || ""}`.replace(/\s+/g, " ").trim();
        const preview =
          compactText.length > 80 ? `${compactText.slice(0, 77)}...` : compactText;

        this.emitActivity({
          handle,
          event: {
            type: "chat",
            when: when?.getTime?.() || Date.now(),
            label: `Chat ${instance.config.name.replace("chat-", "")}: ${preview}`,
            piece: instance.config.name,
            ref: insertedId || null,
            text: compactText,
          },
          countsDelta: { chats: 1 },
        });
      }

      // Push notification (production only, non-muted, chat-system and chat-clock only)
      // Note: chat-sotce is intentionally excluded from push notifications
      if (!this.dev && !userIsMuted && instance.config.name !== "chat-sotce") {
        this.notify(instance, handle, filteredText, when);
      }
    } catch (err) {
      console.error("💬 Message handling error:", err);
    }
  }

  async handleDeleteMessage(instance, ws, id, msg) {
    const { token, sub, id: messageId } = msg.content;
    console.log(
      `💬 [${instance.config.name}] Delete request from ${sub} for message ${messageId}`
    );

    if (!messageId) {
      ws.send(this.pack("error", { message: "Missing message id." }));
      return;
    }

    // Authorize the user
    let authorized;
    if (instance.authorizedConnections[id]?.token === token) {
      authorized = instance.authorizedConnections[id].user;
    } else {
      authorized = await this.authorize(instance, token);
      if (authorized) {
        instance.authorizedConnections[id] = { token, user: authorized };
      }
    }

    if (!authorized || authorized.sub !== sub) {
      ws.send(this.pack("unauthorized", { message: "Please login." }, id));
      return;
    }

    // Find the message in memory and verify ownership
    const message = instance.messages.find((m) => m.id === messageId);
    if (!message) {
      ws.send(this.pack("error", { message: "Message not found." }));
      return;
    }
    if (message.sub !== sub) {
      ws.send(this.pack("error", { message: "You can only delete your own messages." }));
      return;
    }

    // Already deleted
    if (message.deleted) return;

    // Soft-delete in MongoDB
    if (this.db) {
      try {
        const collection = this.db.collection(instance.config.name);
        await collection.updateOne(
          { _id: new ObjectId(messageId) },
          { $set: { deleted: true } }
        );
        console.log("💬 Message soft-deleted in DB");
      } catch (err) {
        console.error("💬 Failed to delete message in DB:", err);
      }
    }

    // Update in-memory
    message.text = "[deleted]";
    message.deleted = true;
    delete message.redactedText;

    // Broadcast deletion to all clients
    this.broadcast(instance, this.pack("message:delete", { id: messageId }));
  }

  async handleChatHeart(instance, ws, id, msg) {
    const { for: forId, token } = msg.content || {};
    if (!forId || !token) return;

    // Reuse cached auth or re-authorize
    let authorized;
    if (instance.authorizedConnections[id]?.token === token) {
      authorized = instance.authorizedConnections[id].user;
    } else {
      authorized = await this.authorize(instance, token);
      if (authorized) {
        instance.authorizedConnections[id] = { token, user: authorized };
      }
    }

    if (!authorized) {
      ws.send(this.pack("unauthorized", { message: "Please login." }, id));
      return;
    }

    if (!this.db) return;

    try {
      const { hearted, count } = await toggleHeart(this.db, {
        user: authorized.sub,
        type: instance.config.name,
        for: forId,
      });
      console.log(`💬 [${instance.config.name}] Heart ${hearted ? "+" : "-"} on ${forId} → ${count}`);
      this.broadcast(instance, this.pack("message:hearts", { for: forId, count }));
    } catch (err) {
      console.error("💬 Heart error:", err);
    }
  }

  async authorize(instance, token) {
    try {
      const response = await fetch(instance.config.userInfoEndpoint, {
        headers: {
          Authorization: "Bearer " + token,
          "Content-Type": "application/json",
        },
      });

      if (response.status === 200) {
        return response.json();
      }
      return undefined;
    } catch (err) {
      console.error("💬 Authorization error:", err);
      return undefined;
    }
  }

  async getHandleFromSub(instance, fromSub) {
    if (instance.subsToHandles[fromSub]) {
      return "@" + instance.subsToHandles[fromSub];
    }

    try {
      let prefix = instance.config.name === "chat-sotce" ? "sotce-" : "";
      let host = this.dev ? "https://localhost:8888" : 
        (instance.config.name === "chat-sotce" ? "https://sotce.net" : "https://aesthetic.computer");

      const options = {};
      if (this.dev) options.agent = this.agent;

      const response = await fetch(`${host}/handle?for=${prefix}${fromSub}`, options);
      if (response.status === 200) {
        const data = await response.json();
        instance.subsToHandles[fromSub] = data.handle;
        return "@" + data.handle;
      }
    } catch (err) {
      console.error("💬 Handle lookup error:", err);
    }

    return "@unknown";
  }

  async checkSubscription(instance, sub, token) {
    if (instance.subsToSubscribers[sub] !== undefined) {
      return instance.subsToSubscribers[sub];
    }

    const host = this.dev ? "https://localhost:8888" : "https://sotce.net";
    const options = {
      method: "POST",
      body: JSON.stringify({ retrieve: "subscription" }),
      headers: {
        Authorization: "Bearer " + token,
        "Content-Type": "application/json",
      },
    };

    if (this.dev) options.agent = this.agent;

    try {
      const response = await fetch(`${host}/sotce-net/subscribed`, options);
      if (response.status === 200) {
        const data = await response.json();
        instance.subsToSubscribers[sub] = data.subscribed;
        return data.subscribed;
      }
    } catch (err) {
      console.error("💬 Subscription check error:", err);
    }

    instance.subsToSubscribers[sub] = false;
    return false;
  }

  async isMuted(instance, sub) {
    if (!sub || !this.db) return false;
    try {
      const mutesCollection = this.db.collection(instance.config.name + "-mutes");
      const mute = await mutesCollection.findOne({ user: sub });
      return !!mute;
    } catch (err) {
      return false;
    }
  }

  notify(instance, handle, text, when) {
    let title = handle + " 💬";
    
    if (instance.config.name === "chat-clock") {
      const getClockEmoji = (date) => {
        let hour = date.getHours() % 12 || 12;
        const minutes = date.getMinutes();
        const emojiCode = minutes < 30 ? (0x1f550 + hour - 1) : (0x1f55c + hour - 1);
        return String.fromCodePoint(emojiCode);
      };
      title = handle + " " + getClockEmoji(when);
    }

    try {
      getMessaging().send({
        notification: { title, body: text },
        apns: {
          payload: {
            aps: {
              "mutable-content": 1,
              "interruption-level": "time-sensitive",
              priority: 10,
              "content-available": 1,
            },
          },
          headers: {
            "apns-priority": "10",
            "apns-push-type": "alert",
            "apns-expiration": "0",
          },
        },
        webpush: {
          headers: {
            Urgency: "high",
            TTL: "0",
            image: "https://aesthetic.computer/api/logo.png",
          },
        },
        topic: instance.config.topic,
        data: { piece: "chat" },
      }).then((response) => {
        console.log("💬 Notification sent:", response);
      }).catch((err) => {
        console.log("💬 Notification error:", err);
      });
    } catch (err) {
      console.error("💬 Notification error:", err);
    }
  }

  // Handle HTTP log endpoint
  async handleLog(instance, body, authHeader) {
    const token = authHeader?.split(" ")[1];
    if (token !== this.loggerKey) {
      return { status: 403, body: { status: "error", message: "Forbidden" } };
    }

    try {
      const parsed = typeof body === "string" ? JSON.parse(body) : body;
      console.log(`💬 [${instance.config.name}] Log received from: ${parsed.from || 'unknown'}`);

      instance.messages.push(parsed);
      if (instance.messages.length > MAX_MESSAGES) instance.messages.shift();

      // Handle actions (mute/unmute, handle updates)
      if (parsed.action) {
        await this.handleLogAction(instance, parsed);
      }

      this.broadcast(instance, this.pack("message", parsed));

      if (instance.config.name === "chat-system") {
        this.notify(instance, "log 🪵", parsed.text, new Date());
      }

      return { status: 200, body: { status: "success", message: "Log received" } };
    } catch (err) {
      return { status: 400, body: { status: "error", message: "Malformed log JSON" } };
    }
  }

  async handleLogAction(instance, parsed) {
    let [object, behavior] = (parsed.action || "").split(":");
    if (!behavior) {
      behavior = object;
      object = null;
    }

    if (object === "chat-system" && (behavior === "mute" || behavior === "unmute")) {
      const user = parsed.users[0];
      if (behavior === "mute") {
        instance.messages.forEach((msg) => {
          if (msg.sub === user) redact(msg);
        });
      } else {
        instance.messages.forEach((msg) => {
          if (msg.sub === user) unredact(msg);
        });
      }
      this.broadcast(instance, this.pack(parsed.action, { user }));
    }

    if (object === "handle") {
      if (behavior === "colors") {
        // Broadcast handle color changes to all connected clients.
        const data = JSON.parse(parsed.value);
        this.broadcast(instance, this.pack("handle:colors", { user: parsed.users[0], handle: data.handle, colors: data.colors }));
      } else {
        instance.subsToHandles[parsed.users[0]] = parsed.value;

        if (behavior === "update" || behavior === "strip") {
          const from = behavior === "update" ? "@" + parsed.value : "nohandle";
          instance.messages.forEach((msg) => {
            if (msg.sub === parsed.users[0]) {
              msg.from = from;
            }
          });
          this.broadcast(instance, this.pack(parsed.action, { user: parsed.users[0], handle: from }));
        }
      }
    }
  }

  pack(type, content, id) {
    if (typeof content === "object") content = JSON.stringify(content);
    return JSON.stringify({ type, content, id });
  }

  broadcast(instance, message) {
    Object.values(instance.connections).forEach((c) => {
      if (c?.readyState === WebSocket.OPEN) c.send(message);
    });
  }

  broadcastOthers(instance, exclude, message) {
    Object.values(instance.connections).forEach((c) => {
      if (c !== exclude && c?.readyState === WebSocket.OPEN) c.send(message);
    });
  }

  // Set the presence resolver function (called from session.mjs)
  // This allows us to query which users are on a specific piece
  setPresenceResolver(resolver) {
    this.presenceResolver = resolver;
  }

  // Get list of online handles for an instance (all connected & authorized)
  getOnlineHandles(instance) {
    const handles = [];
    for (const [id, auth] of Object.entries(instance.authorizedConnections)) {
      if (auth.handle && instance.connections[id]) {
        handles.push(auth.handle);
      }
    }
    return [...new Set(handles)]; // Remove duplicates
  }

  // Get handles of users actually viewing the chat piece right now
  getHereHandles(instance) {
    if (!this.presenceResolver) return [];
    // Get all users on "chat" piece from session server
    const onChatPiece = this.presenceResolver("chat");
    // Intersect with authorized handles for this chat instance
    const onlineHandles = this.getOnlineHandles(instance);
    return onChatPiece.filter(h => onlineHandles.includes(h));
  }

  // Broadcast presence data (online + here) to all clients
  broadcastOnlineHandles(instance) {
    const online = this.getOnlineHandles(instance);
    const here = this.getHereHandles(instance);
    // Send both for backwards compatibility and new "here" feature
    this.broadcast(instance, this.pack("presence", { 
      handles: online,  // backwards compat with old "online-handles"
      online,           // all authorized chat connections
      here              // only those actually on chat piece
    }));
  }

  // Get status for a specific instance or all
  getStatus(host = null) {
    if (host) {
      const instance = this.getInstance(host);
      if (!instance) return null;
      return {
        name: instance.config.name,
        connections: Object.keys(instance.connections).length,
        messages: instance.messages.length,
      };
    }

    return Object.entries(this.instances).map(([host, instance]) => ({
      host,
      name: instance.config.name,
      connections: Object.keys(instance.connections).length,
      messages: instance.messages.length,
    }));
  }

  // Get recent messages for a specific instance (for dashboard)
  getRecentMessages(host, count = 10) {
    const instance = this.getInstance(host);
    if (!instance) return [];
    
    // Return the most recent messages, but don't expose sensitive data
    return instance.messages.slice(-count).map(msg => ({
      from: msg.from || 'unknown',
      text: msg.text || '',
      when: msg.when
    })).reverse(); // Most recent first
  }

  // Persist any in-memory messages that weren't saved to MongoDB.
  // Messages received while this.db was null (broken env) have a `sub` field
  // but were never inserted. Messages loaded from DB on startup do not have `sub`.
  async persistAllMessages() {
    // Establish a connection if we don't have one
    if (!this.db && this.mongoConnectionString) {
      try {
        console.log("💬 Connecting to MongoDB for message persistence...");
        this.mongoClient = new MongoClient(this.mongoConnectionString);
        await this.mongoClient.connect();
        this.db = this.mongoClient.db(this.mongoDbName);
        console.log("💬 MongoDB connected for persistence!");
      } catch (err) {
        console.error("💬 Failed to connect to MongoDB for persistence:", err);
        return 0;
      }
    }

    if (!this.db) {
      console.error("💬 No MongoDB connection available, cannot persist messages");
      return 0;
    }

    let totalPersisted = 0;

    for (const [, instance] of Object.entries(this.instances)) {
      const collectionName = instance.config.name;
      // Only persist messages that were never saved to DB.
      // DB-loaded and successfully-stored messages have an `id` (from MongoDB _id).
      // Messages received while DB was unavailable have `sub` but no `id`.
      const unpersisted = instance.messages.filter(msg => msg.sub && !msg.id);

      if (unpersisted.length === 0) {
        console.log(`💬 [${collectionName}] No unpersisted messages`);
        continue;
      }

      try {
        const collection = this.db.collection(collectionName);
        const docs = unpersisted.map(msg => ({
          user: msg.sub,
          text: msg.text,
          when: msg.when,
          font: msg.font || "font_1",
        }));

        const result = await collection.insertMany(docs, { ordered: false });
        totalPersisted += result.insertedCount;
        console.log(`💬 [${collectionName}] Persisted ${result.insertedCount} messages`);
      } catch (err) {
        // insertMany with ordered:false continues on duplicate errors
        if (err.insertedCount) totalPersisted += err.insertedCount;
        console.error(`💬 [${collectionName}] Persistence error:`, err.message);
      }
    }

    return totalPersisted;
  }

  async shutdown() {
    console.log("💬 ChatManager shutting down...");
    const count = await this.persistAllMessages();
    console.log(`💬 Persisted ${count} total messages`);

    if (this.mongoClient) {
      await this.mongoClient.close();
      console.log("💬 MongoDB connection closed");
    }
  }
}
