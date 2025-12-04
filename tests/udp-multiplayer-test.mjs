#!/usr/bin/env node
// UDP Multiplayer Test Script (WebSocket Version)
// Tests the 1v1:move channel via WebSocket on localhost session server
// 
// Note: This tests via WebSocket since geckos.io client requires a browser.
//       The actual UDP path is tested by running the game in two browser windows.
//
// Usage: 
//   cd session-server && npm run dev  # Start session server first
//   node tests/udp-multiplayer-test.mjs

import WebSocket from "ws";

const SESSION_URL = process.env.SESSION_URL || "wss://localhost:8889";
const NUM_FAKE_PLAYERS = 2;
const UPDATE_INTERVAL_MS = 50; // 20 updates per second

console.log("🧪 Multiplayer Position Test (WebSocket)");
console.log("=========================================");
console.log(`📡 Connecting to: ${SESSION_URL}`);
console.log(`👥 Simulating ${NUM_FAKE_PLAYERS} players\n`);
console.log("ℹ️  Note: This tests WebSocket path. For UDP, test with two browser windows.\n");

const players = [];

class FakePlayer {
  constructor(id) {
    this.id = id;
    this.handle = `test_player_${id}`;
    this.pos = { x: Math.random() * 10 - 5, y: 1.6, z: Math.random() * 10 - 5 };
    this.rot = { x: 0, y: Math.random() * 360, z: 0 };
    this.ws = null;
    this.connected = false;
    this.receivedMoves = 0;
    this.wsId = null;
  }

  async connect() {
    return new Promise((resolve, reject) => {
      this.ws = new WebSocket(SESSION_URL);

      const timeout = setTimeout(() => {
        reject(new Error(`Player ${this.id} connection timeout`));
      }, 10000);

      this.ws.on("open", () => {
        clearTimeout(timeout);
        this.connected = true;
        console.log(`✅ Player ${this.id} (${this.handle}) WebSocket connected`);
      });

      this.ws.on("message", (data) => {
        try {
          const msg = JSON.parse(data.toString());
          
          // Handle connection acknowledgment
          if (msg.type === "connected" || msg.type === "connected:already") {
            this.wsId = msg.id;
            console.log(`🆔 Player ${this.id} assigned WebSocket ID: ${this.wsId}`);
            
            // Send join message
            this.send("1v1:join", {
              handle: this.handle,
              pos: this.pos,
              rot: this.rot,
              health: 100
            });
            resolve();
          }
          
          // Handle moves from other players
          if (msg.type === "1v1:move" && msg.id !== this.wsId) {
            this.receivedMoves++;
            if (this.receivedMoves <= 3 || this.receivedMoves % 100 === 0) {
              console.log(`📨 Player ${this.id} received move from ${msg.id}:`,
                `pos(${msg.content.pos.x.toFixed(2)}, ${msg.content.pos.y.toFixed(2)}, ${msg.content.pos.z.toFixed(2)})`);
            }
          }
          
          // Handle join from other players
          if (msg.type === "1v1:join" && msg.id !== this.wsId) {
            console.log(`👋 Player ${this.id} sees ${msg.content.handle} joined`);
          }
        } catch (e) {
          // Ignore non-JSON messages
        }
      });

      this.ws.on("error", (err) => {
        console.error(`❌ Player ${this.id} WebSocket error:`, err.message);
        reject(err);
      });

      this.ws.on("close", () => {
        this.connected = false;
        console.log(`🔌 Player ${this.id} disconnected`);
      });
    });
  }

  send(type, content) {
    if (!this.connected || !this.ws) return;
    this.ws.send(JSON.stringify({ type, content }));
  }

  sendPosition() {
    if (!this.connected) return;

    // Simulate movement
    this.pos.x += (Math.random() - 0.5) * 0.1;
    this.pos.z += (Math.random() - 0.5) * 0.1;
    this.rot.y += Math.random() * 2;

    this.send("1v1:move", {
      pos: this.pos,
      rot: this.rot
    });
  }

  disconnect() {
    if (this.ws) {
      this.ws.close();
      this.connected = false;
    }
  }

  getStats() {
    return {
      id: this.id,
      handle: this.handle,
      wsId: this.wsId,
      connected: this.connected,
      receivedMoves: this.receivedMoves
    };
  }
}

async function runTest() {
  // Create fake players
  for (let i = 0; i < NUM_FAKE_PLAYERS; i++) {
    players.push(new FakePlayer(i));
  }

  // Connect all players
  console.log("🔗 Connecting players...\n");
  try {
    await Promise.all(players.map(p => p.connect()));
  } catch (err) {
    console.error("❌ Failed to connect all players:", err.message);
    process.exit(1);
  }

  console.log("\n✅ All players connected!\n");
  console.log("📤 Starting position updates...\n");

  // Start sending position updates
  const updateInterval = setInterval(() => {
    players.forEach(p => p.sendPosition());
  }, UPDATE_INTERVAL_MS);

  // Run for 5 seconds then print stats
  await new Promise(resolve => setTimeout(resolve, 5000));

  clearInterval(updateInterval);

  console.log("\n📊 Test Results:");
  console.log("================");
  players.forEach(p => {
    const stats = p.getStats();
    console.log(`Player ${stats.id} (${stats.handle}): ${stats.receivedMoves} moves received`);
  });

  // Calculate expected moves
  const expectedMovesPerPlayer = (5000 / UPDATE_INTERVAL_MS) * (NUM_FAKE_PLAYERS - 1);
  console.log(`\n📈 Expected moves per player: ~${expectedMovesPerPlayer}`);
  
  const avgReceived = players.reduce((sum, p) => sum + p.getStats().receivedMoves, 0) / players.length;
  const successRate = (avgReceived / expectedMovesPerPlayer * 100).toFixed(1);
  console.log(`📈 Average received: ${avgReceived.toFixed(0)} (${successRate}% success rate)`);

  // Cleanup
  console.log("\n🧹 Cleaning up...");
  players.forEach(p => p.disconnect());

  console.log("✅ Test complete!\n");
  process.exit(0);
}

// Handle errors
process.on("unhandledRejection", (err) => {
  console.error("❌ Unhandled error:", err);
  process.exit(1);
});

runTest();
