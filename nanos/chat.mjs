// chat, 24.03.30.14.53
// This file is a unikernal compatible chat server designed to eventually replace `session-server/session.mjs` by adding both websocket and udp support.

// But its first job is to be the chat server for AC.

// Management:
// https://console.cloud.google.com/compute/instances?project=aesthetic-computer

import { WebSocketServer } from "ws";
import http from "http";

console.log("Environment:", process.env);

const server = http.createServer((req, res) => {
  res.writeHead(200, { "Content-Type": "text/plain" });
  res.end("😱 Aesthetic Computer\n");
});

const port = process.env.NODE_ENV === "development" ? 8083 : 80;

server.listen(port, "0.0.0.0", () => {
  console.log(`HTTP server running at http://127.0.0.1:${port}`);
});

const wss = new WebSocketServer({ server });

wss.on("connection", (ws) => {
  ws.on("message", (message) => {
    console.log("received: %s", message);
  });

  ws.send("Welcome to the WebSocket server!");
});

console.log(`WebSocket server running alongside HTTP server on port ${port}`);
