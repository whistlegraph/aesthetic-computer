import { WebSocket, WebSocketServer } from "ws";
import http from 'http';

console.log(WebSocket);

const server = http.createServer((req, res) => {
    res.writeHead(200, {'Content-Type': 'text/plain'});
    res.end('😱 Aesthetic Computer\n');
});

server.listen(8083, "0.0.0.0", () => {
    console.log('Server running at http://127.0.0.1:8083/');
});