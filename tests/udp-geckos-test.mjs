#!/usr/bin/env node
// UDP Multiplayer Test Script (geckos.io / WebRTC DataChannel)
// Tests the 1v1:move channel via UDP on localhost session server
//
// Usage: 
//   cd session-server && npm run dev  # Start session server first
//   node tests/udp-geckos-test.mjs
//
// This script tests the actual UDP path using geckos.io client in Node.js

// Allow self-signed certificates for local development
if (process.env.SESSION_HOST?.includes("localhost") || !process.env.SESSION_HOST) {
  process.env.NODE_TLS_REJECT_UNAUTHORIZED = "0";
}

// Polyfill WebRTC for Node.js (required for geckos.io v3)
import * as nodeDataChannel from "node-datachannel/polyfill";
// Set globals for geckos.io client
globalThis.RTCPeerConnection = nodeDataChannel.RTCPeerConnection;
globalThis.RTCSessionDescription = nodeDataChannel.RTCSessionDescription;
globalThis.RTCIceCandidate = nodeDataChannel.RTCIceCandidate;

import geckos from "@geckos.io/client";

const SESSION_HOST = process.env.SESSION_HOST || "https://localhost";
const SESSION_PORT = parseInt(process.env.SESSION_PORT || "8889");
const NUM_FAKE_PLAYERS = 2;
const UPDATE_INTERVAL_MS = 50; // 20 updates per second
const TEST_DURATION_MS = 5000;

console.log("🧪 UDP Multiplayer Test (geckos.io / WebRTC)");
console.log("=============================================");
console.log(`📡 Connecting to: ${SESSION_HOST}:${SESSION_PORT}`);
console.log(`👥 Simulating ${NUM_FAKE_PLAYERS} players\n`);

const players = [];

class FakePlayer {
  constructor(id) {
    this.id = id;
    this.handle = `udp_test_${id}`;
    this.pos = { x: Math.random() * 10 - 5, y: 1.6, z: Math.random() * 10 - 5 };
    this.rot = { x: 0, y: Math.random() * 360, z: 0 };
    this.channel = null;
    this.connected = false;
    this.receivedMoves = 0;
    this.sentMoves = 0;
  }

  async connect() {
    return new Promise((resolve, reject) => {
      // geckos.io client for Node.js
      this.channel = geckos({
        url: SESSION_HOST,
        port: SESSION_PORT,
        // Empty ICE servers for local testing
        iceServers: []
      });

      const timeout = setTimeout(() => {
        reject(new Error(`Player ${this.id} UDP connection timeout`));
      }, 15000);

      this.channel.onConnect((error) => {
        clearTimeout(timeout);
        
        if (error) {
          console.error(`❌ Player ${this.id} failed to connect:`, error.message || error);
          reject(error);
          return;
        }

        this.connected = true;
        console.log(`✅ Player ${this.id} (${this.handle}) UDP connected via WebRTC`);

        // Send identity
        this.channel.emit("udp:identity", JSON.stringify({
          user: { sub: `test-user-${this.id}` },
          handle: this.handle
        }));

        // Listen for moves from other players
        this.channel.on("1v1:move", (data) => {
          this.receivedMoves++;
          try {
            const parsed = typeof data === "string" ? JSON.parse(data) : data;
            if (this.receivedMoves <= 3 || this.receivedMoves % 50 === 0) {
              console.log(`📨 Player ${this.id} received UDP move from ${parsed.handle}:`,
                `pos(${parsed.pos.x.toFixed(2)}, ${parsed.pos.y.toFixed(2)}, ${parsed.pos.z.toFixed(2)})`);
            }
          } catch (e) {
            // Ignore parse errors
          }
        });

        resolve();
      });

      this.channel.onDisconnect(() => {
        this.connected = false;
        console.log(`🔌 Player ${this.id} UDP disconnected`);
      });
    });
  }

  sendPosition() {
    if (!this.connected || !this.channel) return;

    // Simulate movement
    this.pos.x += (Math.random() - 0.5) * 0.1;
    this.pos.z += (Math.random() - 0.5) * 0.1;
    this.rot.y += Math.random() * 2;

    this.channel.emit("1v1:move", JSON.stringify({
      handle: this.handle,
      pos: this.pos,
      rot: this.rot
    }));
    this.sentMoves++;
  }

  disconnect() {
    if (this.channel) {
      this.channel.close();
      this.connected = false;
    }
  }

  getStats() {
    return {
      id: this.id,
      handle: this.handle,
      connected: this.connected,
      sentMoves: this.sentMoves,
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
  console.log("🔗 Connecting players via UDP...\n");
  try {
    await Promise.all(players.map(p => p.connect()));
  } catch (err) {
    console.error("❌ Failed to connect all players:", err.message);
    console.error("\n💡 Make sure the session server is running: cd session-server && npm run dev");
    process.exit(1);
  }

  console.log("\n✅ All players connected via UDP!\n");
  console.log("📤 Starting position updates...\n");

  // Start sending position updates
  const updateInterval = setInterval(() => {
    players.forEach(p => p.sendPosition());
  }, UPDATE_INTERVAL_MS);

  // Run for test duration then print stats
  await new Promise(resolve => setTimeout(resolve, TEST_DURATION_MS));

  clearInterval(updateInterval);

  console.log("\n📊 UDP Test Results:");
  console.log("====================");
  players.forEach(p => {
    const stats = p.getStats();
    console.log(`Player ${stats.id} (${stats.handle}): sent ${stats.sentMoves}, received ${stats.receivedMoves} moves`);
  });

  // Calculate expected moves
  const expectedSentPerPlayer = TEST_DURATION_MS / UPDATE_INTERVAL_MS;
  const expectedReceivedPerPlayer = expectedSentPerPlayer * (NUM_FAKE_PLAYERS - 1);
  
  console.log(`\n📈 Expected sent per player: ~${expectedSentPerPlayer}`);
  console.log(`📈 Expected received per player: ~${expectedReceivedPerPlayer}`);
  
  const avgReceived = players.reduce((sum, p) => sum + p.getStats().receivedMoves, 0) / players.length;
  const successRate = (avgReceived / expectedReceivedPerPlayer * 100).toFixed(1);
  console.log(`📈 Average received: ${avgReceived.toFixed(0)} (${successRate}% success rate)`);

  if (avgReceived > 0) {
    console.log("\n✅ UDP communication working!");
  } else {
    console.log("\n⚠️  No UDP messages received - check server 1v1:move handler");
  }

  // Cleanup
  console.log("\n🧹 Cleaning up...");
  players.forEach(p => p.disconnect());

  console.log("✅ Test complete!\n");
  
  // Give time for cleanup
  setTimeout(() => process.exit(0), 500);
}

// Handle errors
process.on("unhandledRejection", (err) => {
  console.error("❌ Unhandled error:", err);
  process.exit(1);
});

runTest();
