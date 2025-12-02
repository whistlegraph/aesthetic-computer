// AC WebSocket Audio Server for Max for Live
// Receives audio samples via WebSocket and outputs to Max

const Max = require("max-api");
const WebSocket = require("ws");

const PORT = 9999;
let wss = null;
let clientConnected = false;

// Start WebSocket server
function startServer() {
    if (wss) {
        Max.post("Server already running on port " + PORT);
        return;
    }
    
    wss = new WebSocket.Server({ port: PORT });
    Max.post("🎛️ AC Audio WebSocket server started on port " + PORT);
    
    wss.on("connection", (ws) => {
        clientConnected = true;
        Max.post("🎛️ Client connected!");
        Max.outlet("status", "connected");
        
        ws.on("message", (data) => {
            try {
                const msg = JSON.parse(data);
                
                if (msg.type === "samples") {
                    // Output samples to Max
                    // Format: left samples, then right samples
                    const numSamples = msg.left.length;
                    Max.outlet("samples", numSamples, ...msg.left, ...msg.right);
                } else if (msg.type === "ping") {
                    ws.send(JSON.stringify({ type: "pong" }));
                }
            } catch (e) {
                // Binary data or parse error
            }
        });
        
        ws.on("close", () => {
            clientConnected = false;
            Max.post("🎛️ Client disconnected");
            Max.outlet("status", "disconnected");
        });
        
        ws.on("error", (err) => {
            Max.post("🎛️ WebSocket error: " + err.message);
        });
    });
    
    wss.on("error", (err) => {
        Max.post("🎛️ Server error: " + err.message);
        if (err.code === "EADDRINUSE") {
            Max.post("🎛️ Port " + PORT + " is already in use!");
        }
    });
}

function stopServer() {
    if (wss) {
        wss.close();
        wss = null;
        Max.post("🎛️ Server stopped");
    }
}

// Max handlers
Max.addHandler("start", startServer);
Max.addHandler("stop", stopServer);
Max.addHandler("status", () => {
    Max.outlet("status", clientConnected ? "connected" : "disconnected");
});

// Auto-start
startServer();
