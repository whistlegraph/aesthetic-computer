#!/usr/bin/env node
// Manually trigger handle refresh on chat servers
// Usage: node refresh-chat-handles.mjs [chat-system|chat-clock|chat-sotce]

import dotenv from "dotenv";
import fetch from "node-fetch";

dotenv.config({ path: "chat.env" });

const instance = process.argv[2] || "chat-clock";
const dev = process.env.NODE_ENV === "development";

const servers = {
  "chat-system": dev ? "https://localhost:8083/log" : "https://chat-system.aesthetic.computer/log",
  "chat-clock": dev ? "https://localhost:8085/log" : "https://chat-clock.aesthetic.computer/log",
  "chat-sotce": dev ? "https://localhost:8084/log" : "https://chat.sotce.net/log",
};

const url = servers[instance];

if (!url) {
  console.error(`❌ Invalid instance: ${instance}`);
  console.log("Valid instances: chat-system, chat-clock, chat-sotce");
  process.exit(1);
}

// Send a dummy log message to trigger the server
const msg = {
  from: "refresh-script",
  text: `🔄 Handle refresh triggered for ${instance}`,
  when: new Date(),
  users: [],
  action: "system:refresh"
};

console.log(`🔄 Triggering handle refresh for ${instance}...`);
console.log(`📡 URL: ${url}`);

try {
  const response = await fetch(url, {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
      "Authorization": `Bearer ${process.env.LOGGER_KEY}`,
    },
    body: JSON.stringify(msg),
  });

  if (response.ok) {
    const result = await response.json();
    console.log(`✅ Success:`, result);
  } else {
    console.error(`❌ Failed: ${response.status} ${response.statusText}`);
    const text = await response.text();
    console.error(text);
  }
} catch (error) {
  console.error(`❌ Error:`, error.message);
}
