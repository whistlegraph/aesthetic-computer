const http = require("http");
const fs = require("fs");
const WebSocket = require("ws");

const server = http.createServer((req, res) => {
  if (req.url === "/") {
    // Serve the HTML page
    fs.readFile("index.html", (err, data) => {
      if (err) {
        res.writeHead(500);
        res.end("Error loading index.html");
        return;
      }
      res.writeHead(200, { "Content-Type": "text/html" });
      res.end(data);
    });
  } else if (req.url === "/video.mp4") {
    // Serve the video file
    const stream = fs.createReadStream("video.mp4");
    res.writeHead(200, { "Content-Type": "video/mp4" });
    stream.pipe(res);
  } else {
    // 404 error
    res.writeHead(404);
    res.end("Page not found");
  }
});

server.listen(8000, () => {
  console.log("Server running on port 8000");
});

const wss = new WebSocket.Server({ port: 8080 });

let clients = [];

wss.on("connection", (ws) => {
  clients.push(ws);

  ws.on("message", (message) => {
    // When a message is received, broadcast it to all clients
    clients.forEach((client) => {
      if (client !== ws && client.readyState === WebSocket.OPEN) {
        client.send(message);
      }
    });
  });

  ws.on("close", () => {
    // Remove the client from the list of clients when the connection is closed
    clients = clients.filter((client) => client !== ws);
  });
});