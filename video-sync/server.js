// Server

const WebSocket = require("ws");

const wss = new WebSocket.Server({ port: 8080 });
let clients = [];
let playing = false;
let canRestart = true;

wss.on("connection", (ws) => {
  clients.push(ws);
  ws.on("message", (message) => {
    const msg = message.toString();
    console.log(msg, clients.length);
    if (msg === "start" && clients.length > 1) {
      // Make sure this isn't the first connected client, and then
      // if this is the first client to start the video,
      // broadcast a 'play' message to all clients.
      if (!playing) {
        playing = true;
        clients.forEach((client) => {
          if (client.readyState === WebSocket.OPEN) {
            client.send("play");
          }
        });
      }
    } else if (msg === "restart" && canRestart) {
      // Broadcast a 'restart' message to all clients to trigger
      // the video playback to start over
      clients.forEach((client) => {
        if (client.readyState === WebSocket.OPEN) client.send("restart");
      });
      canRestart = false;
      // Prevent double restarts by adding a .5 second buffer.
      setTimeout(() => (canRestart = true), 500);
    }
  });

  ws.on("close", () => {
    clients = clients.filter((client) => client !== ws); // Remove on close.
  });
});