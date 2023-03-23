// const http = require("http");
// const fs = require("fs");
const WebSocket = require("ws");

// const server = http.createServer((req, res) => {
//   if (req.url === "/") {
//     // Serve the HTML page
//     fs.readFile("index.html", (err, data) => {
//       if (err) {
//         res.writeHead(500);
//         res.end("Error loading index.html");
//         return;
//       }
//       res.writeHead(200, { "Content-Type": "text/html" });
//       res.end(data);
//     });
//     //} else if (req.url === "/video.mp4") {
//     // Serve the video file from the RPI boot partition.
//     //const stream = fs.createReadStream("/boot/screenplay.mp4");
//     //res.writeHead(200, { "Content-Type": "video/mp4" });
//     //stream.pipe(res);
//   } else {
//     // 404 error
//     res.writeHead(404);
//     res.end("Page not found");
//   }
// });

// server.listen(8000, () => {
//   console.log("Server running on port 8000");
// });

const wss = new WebSocket.Server({ port: 8080 });

let clients = [];
let playing = false;
let canRestart = true;

wss.on("connection", (ws) => {
  clients.push(ws);

  ws.on("message", (message) => {
    console.log(message, clients.length);
    if (message === "start" && clients.length > 1) {
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
    } else if (message === "restart" && canRestart) {
      // Broadcast a 'restart' message to all clients to trigger the video playback to start over
      clients.forEach((client) => {
        if (client.readyState === WebSocket.OPEN) {
          client.send("restart");
        }
      });

      canRestart = false;
      setTimeout(() => canRestart = true, 500); // Prevent double restarts by adding a .5 second buffer.
    }
  });

  ws.on("close", () => {
    // Remove the client from the list of clients when the connection is closed.
    clients = clients.filter((client) => client !== ws);
  });
});