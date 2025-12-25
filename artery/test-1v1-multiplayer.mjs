#!/usr/bin/env node
/**
 * 🎮 1v1 Automated Multiplayer Test
 * 
 * Opens split~1v1 to create two players in the same arena, then uses
 * the Camdoll protocol to autonomously control both players.
 * They follow the bot and each other around the arena.
 * 
 * Usage: node artery/test-1v1-multiplayer.mjs [--duration=30]
 */

import Artery from './artery.mjs';

// ═══════════════════════════════════════════════════════════════════════════
// TERMINAL COLORS
// ═══════════════════════════════════════════════════════════════════════════
const RESET = '\x1b[0m';
const BOLD = '\x1b[1m';
const DIM = '\x1b[2m';
const GREEN = '\x1b[92m';
const CYAN = '\x1b[96m';
const YELLOW = '\x1b[93m';
const MAGENTA = '\x1b[95m';
const RED = '\x1b[91m';
const WHITE = '\x1b[97m';
const ORANGE = '\x1b[38;5;208m';

const log = (msg) => console.log(`${MAGENTA}${BOLD}🎮${RESET} ${msg}`);
const p1log = (msg) => console.log(`${GREEN}[P1]${RESET} ${msg}`);
const p2log = (msg) => console.log(`${ORANGE}[P2]${RESET} ${msg}`);
const pass = (msg) => console.log(`${GREEN}✅ ${msg}${RESET}`);
const fail = (msg) => console.log(`${RED}❌ ${msg}${RESET}`);
const info = (msg) => console.log(`${CYAN}ℹ️  ${msg}${RESET}`);

// ═══════════════════════════════════════════════════════════════════════════
// CAMDOLL - Remote puppet control via CDP
// ═══════════════════════════════════════════════════════════════════════════

class Camdoll {
  constructor(name, color, client) {
    this.name = name;
    this.color = color;
    this.client = client;
    this.pos = { x: 0, y: 0, z: 0 };
    this.rot = { x: 0, y: 0, z: 0 };
    this.botPos = null;
    this.otherPos = null;
    this.state = 'exploring';
    this.stateTimer = 0;
    this.tickInterval = null;
    this.log = (msg) => console.log(`${this.color}[${this.name}]${RESET} ${msg}`);
  }

  // Inject camdoll bridge into the frame
  async init() {
    await this.client.send('Runtime.evaluate', {
      expression: `
        window.__camdoll = {
          name: "${this.name}",
          pressKey: function(key, type = 'keydown') {
            document.dispatchEvent(new KeyboardEvent(type, {
              key: key,
              code: key.length === 1 ? 'Key' + key.toUpperCase() : key,
              bubbles: true, cancelable: true,
            }));
          },
          getState: function() {
            if (typeof self === 'undefined') return null;
            return {
              handle: self.handle,
              pos: self.pos,
              rot: self.rot,
              health: self.health,
              bot: typeof bot !== 'undefined' ? { pos: bot.pos, state: bot.state } : null,
              others: typeof others !== 'undefined' ? Object.values(others).map(o => ({ 
                pos: o.pos, handle: o.handle 
              })) : [],
              gameState: typeof gameState !== 'undefined' ? gameState : 'unknown',
              udpConnected: typeof udpChannel !== 'undefined' ? udpChannel?.connected : false,
            };
          }
        };
        true;
      `
    });
    this.log('Camdoll bridge installed');
  }

  // Send a key press
  async pressKey(key, duration = 80) {
    await this.client.send('Runtime.evaluate', {
      expression: `window.__camdoll.pressKey('${key}', 'keydown')`
    });
    await new Promise(r => setTimeout(r, duration));
    await this.client.send('Runtime.evaluate', {
      expression: `window.__camdoll.pressKey('${key}', 'keyup')`
    });
  }

  // Get current state from the piece
  async getState() {
    try {
      const result = await this.client.send('Runtime.evaluate', {
        expression: `JSON.stringify(window.__camdoll?.getState() || null)`
      });
      if (result.result?.value) {
        const state = JSON.parse(result.result.value);
        if (state) {
          this.pos = state.pos || this.pos;
          this.rot = state.rot || this.rot;
          this.botPos = state.bot?.pos || null;
          this.otherPos = state.others?.[0]?.pos || null;
        }
        return state;
      }
    } catch (e) {}
    return null;
  }

  // AI behavior tick
  async tick() {
    const state = await this.getState();
    if (!state) return;

    this.stateTimer++;

    switch (this.state) {
      case 'exploring':
        // Random exploration
        if (this.stateTimer % 20 === 0) {
          const keys = ['w', 'a', 's', 'd', 'ArrowLeft', 'ArrowRight'];
          const key = keys[Math.floor(Math.random() * keys.length)];
          await this.pressKey(key, 100);
        }
        // Switch to following bot
        if (this.stateTimer > 50 && this.botPos && Math.random() < 0.04) {
          this.state = 'followBot';
          this.stateTimer = 0;
          this.log('🤖 Following bot');
        }
        // Switch to following other player
        if (this.stateTimer > 80 && this.otherPos && Math.random() < 0.04) {
          this.state = 'followPlayer';
          this.stateTimer = 0;
          this.log('👤 Following player');
        }
        break;

      case 'followBot':
        if (this.botPos) await this.moveToward(this.botPos);
        if (this.stateTimer > 100 || Math.random() < 0.015) {
          this.state = 'exploring';
          this.stateTimer = 0;
          this.log('🔍 Exploring');
        }
        break;

      case 'followPlayer':
        if (this.otherPos) await this.moveToward(this.otherPos);
        if (this.stateTimer > 80 || Math.random() < 0.02) {
          this.state = 'followBot';
          this.stateTimer = 0;
          this.log('🤖 Following bot');
        }
        break;
    }
  }

  // Move toward a target
  async moveToward(target) {
    const dx = target.x - this.pos.x;
    const dz = target.z - this.pos.z;
    const dist = Math.sqrt(dx * dx + dz * dz);

    if (dist < 1.0) {
      // Close enough - strafe
      await this.pressKey(Math.random() < 0.5 ? 'a' : 'd', 80);
      return;
    }

    // Calculate angle to target
    const targetAngle = Math.atan2(dx, -dz) * (180 / Math.PI);
    let angleDiff = targetAngle - (this.rot?.y || 0);
    while (angleDiff > 180) angleDiff -= 360;
    while (angleDiff < -180) angleDiff += 360;

    // Turn toward target
    if (Math.abs(angleDiff) > 20) {
      await this.pressKey(angleDiff > 0 ? 'ArrowRight' : 'ArrowLeft', 60);
    } else {
      // Move forward
      await this.pressKey('w', 100);
    }
  }

  start() {
    this.log('Starting AI');
    this.tickInterval = setInterval(() => this.tick(), 120);
  }

  stop() {
    if (this.tickInterval) {
      clearInterval(this.tickInterval);
      this.tickInterval = null;
    }
    this.log('Stopped');
  }
}

// ═══════════════════════════════════════════════════════════════════════════
// MAIN TEST
// ═══════════════════════════════════════════════════════════════════════════

async function runTest(durationSec = 30) {
  console.log(`\n${BOLD}${MAGENTA}🎮 1v1 Automated Multiplayer Test${RESET}`);
  console.log(`${DIM}Two AI-controlled players in split~1v1 arena${RESET}`);
  console.log(`${DIM}Duration: ${durationSec} seconds${RESET}\n`);

  let camdoll1, camdoll2;
  let client1, client2;
  let mainClient;
  try {
    // First open the panel
    log('Opening AC panel...');
    await Artery.openPanelStandalone();
    await new Promise(r => setTimeout(r, 2000));

    // Connect to main AC frame
    mainClient = new Artery();
    await mainClient.connect();
    pass('Connected to AC');

    // Navigate to split~1v1
    log('Navigating to split~1v1...');
    await mainClient.jump('split~1v1');
    await new Promise(r => setTimeout(r, 4000)); // Wait for both frames to load
    pass('Split view loaded');

    // Try to connect to both player frames
    let playerConnectFailed = false;
    try {
      log('Connecting to Player 1 frame...');
      client1 = new Artery();
      await client1.connectToPlayer(1);
      pass('Player 1 connected');

      log('Connecting to Player 2 frame...');
      client2 = new Artery();
      await client2.connectToPlayer(2);
      pass('Player 2 connected');
    } catch (e) {
      fail('Direct player frame connection failed, falling back to parent __splitFrames injection.');
      playerConnectFailed = true;
    }

    // Wait for pieces to initialize
    await new Promise(r => setTimeout(r, 2000));

    if (!playerConnectFailed) {
      // Normal path: direct CDP control
      camdoll1 = new Camdoll('P1', GREEN, client1);
      camdoll2 = new Camdoll('P2', ORANGE, client2);
      await camdoll1.init();
      await camdoll2.init();
    } else {
      // Fallback: inject into both split frames via parent __splitFrames
      // Use mainClient to inject code into parent, which relays to both frames
      const camdollBridge = (name) => `
        window.__camdoll = {
          name: "${name}",
          pressKey: function(key, type = 'keydown') {
            document.dispatchEvent(new KeyboardEvent(type, {
              key: key,
              code: key.length === 1 ? 'Key' + key.toUpperCase() : key,
              bubbles: true, cancelable: true,
            }));
          },
          getState: function() {
            if (typeof self === 'undefined') return null;
            return {
              handle: self.handle,
              pos: self.pos,
              rot: self.rot,
              health: self.health,
              bot: typeof bot !== 'undefined' ? { pos: bot.pos, state: bot.state } : null,
              others: typeof others !== 'undefined' ? Object.values(others).map(o => ({ 
                pos: o.pos, handle: o.handle 
              })) : [],
              gameState: typeof gameState !== 'undefined' ? gameState : 'unknown',
              udpConnected: typeof udpChannel !== 'undefined' ? udpChannel?.connected : false,
            };
          }
        };
        true;
      `;
      // Inject bridge into both frames
      await mainClient.send('Runtime.evaluate', {
        expression: `window.__splitFrames.sendToTop({ type: 'eval', code: ${JSON.stringify(camdollBridge('P1'))} })`
      });
      await mainClient.send('Runtime.evaluate', {
        expression: `window.__splitFrames.sendToBottom({ type: 'eval', code: ${JSON.stringify(camdollBridge('P2'))} })`
      });
      // Create proxy clients for both frames (using mainClient, but with playerNum)
      camdoll1 = new Camdoll('P1', GREEN, mainClient);
      camdoll2 = new Camdoll('P2', ORANGE, mainClient);
      // Patch Camdoll methods to send to correct frame
      camdoll1.clientSend = async (expr) => {
        await mainClient.send('Runtime.evaluate', {
          expression: `window.__splitFrames.sendToTop({ type: 'eval', code: ${JSON.stringify(expr)} })`
        });
      };
      camdoll2.clientSend = async (expr) => {
        await mainClient.send('Runtime.evaluate', {
          expression: `window.__splitFrames.sendToBottom({ type: 'eval', code: ${JSON.stringify(expr)} })`
        });
      };
      camdoll1.send = camdoll1.clientSend;
      camdoll2.send = camdoll2.clientSend;
      // Patch Camdoll methods for fallback
      camdoll1.pressKey = async function(key, duration = 80) {
        await this.send(`window.__camdoll.pressKey('${key}', 'keydown')`);
        await new Promise(r => setTimeout(r, duration));
        await this.send(`window.__camdoll.pressKey('${key}', 'keyup')`);
      };
      camdoll2.pressKey = async function(key, duration = 80) {
        await this.send(`window.__camdoll.pressKey('${key}', 'keydown')`);
        await new Promise(r => setTimeout(r, duration));
        await this.send(`window.__camdoll.pressKey('${key}', 'keyup')`);
      };
      camdoll1.getState = async function() {
        try {
          const res = await mainClient.send('Runtime.evaluate', {
            expression: `window.__splitFrames.top.contentWindow.__camdoll && JSON.stringify(window.__splitFrames.top.contentWindow.__camdoll.getState())`
          });
          if (res.result?.value) {
            const state = JSON.parse(res.result.value);
            if (state) {
              this.pos = state.pos || this.pos;
              this.rot = state.rot || this.rot;
              this.botPos = state.bot?.pos || null;
              this.otherPos = state.others?.[0]?.pos || null;
            }
            return state;
          }
        } catch (e) {}
        return null;
      };
      camdoll2.getState = async function() {
        try {
          const res = await mainClient.send('Runtime.evaluate', {
            expression: `window.__splitFrames.bottom.contentWindow.__camdoll && JSON.stringify(window.__splitFrames.bottom.contentWindow.__camdoll.getState())`
          });
          if (res.result?.value) {
            const state = JSON.parse(res.result.value);
            if (state) {
              this.pos = state.pos || this.pos;
              this.rot = state.rot || this.rot;
              this.botPos = state.bot?.pos || null;
              this.otherPos = state.others?.[0]?.pos || null;
            }
            return state;
          }
        } catch (e) {}
        return null;
      };
    }

    // Get initial states
    const state1 = await camdoll1.getState();
    const state2 = await camdoll2.getState();
    
    p1log(`Handle: ${state1?.handle || '?'}, UDP: ${state1?.udpConnected ? 'ON' : 'OFF'}`);
    p2log(`Handle: ${state2?.handle || '?'}, UDP: ${state2?.udpConnected ? 'ON' : 'OFF'}`);

    // Check if they see each other
    const p1SeesP2 = state1?.others?.length > 0;
    const p2SeesP1 = state2?.others?.length > 0;
    info(`P1 sees ${state1?.others?.length || 0} others, P2 sees ${state2?.others?.length || 0} others`);

    // Start autonomous movement
    log(`\nStarting ${durationSec}s autonomous test...`);
    console.log(`${DIM}─────────────────────────────────────────${RESET}`);

    camdoll1.start();
    camdoll2.start();

    // Run for duration, showing status
    const startTime = Date.now();
    while ((Date.now() - startTime) < durationSec * 1000) {
      await new Promise(r => setTimeout(r, 3000));

      const elapsed = Math.floor((Date.now() - startTime) / 1000);
      const remaining = durationSec - elapsed;

      const s1 = await camdoll1.getState();
      const s2 = await camdoll2.getState();

      const p1Pos = s1?.pos ? `(${s1.pos.x?.toFixed(1)},${s1.pos.z?.toFixed(1)})` : '?';
      const p2Pos = s2?.pos ? `(${s2.pos.x?.toFixed(1)},${s2.pos.z?.toFixed(1)})` : '?';
      const botState = s1?.bot?.state || '?';

      console.log(
        `${DIM}[${remaining}s]${RESET} ` +
        `${GREEN}P1${RESET}:${p1Pos}→${camdoll1.state.slice(0,3)} ` +
        `${ORANGE}P2${RESET}:${p2Pos}→${camdoll2.state.slice(0,3)} ` +
        `${CYAN}🤖${RESET}:${botState} ` +
        `${DIM}(sees: ${s1?.others?.length || 0}/${s2?.others?.length || 0})${RESET}`
      );
    }

    console.log(`${DIM}─────────────────────────────────────────${RESET}`);
    pass('Test completed!');

  } catch (e) {
    fail(`Error: ${e.message}`);
    console.error(e);
  } finally {
    camdoll1?.stop();
    camdoll2?.stop();
    client1?.ws?.close();
    client2?.ws?.close();
    mainClient?.ws?.close();
  }
}

// ═══════════════════════════════════════════════════════════════════════════
// MAIN
// ═══════════════════════════════════════════════════════════════════════════

async function main() {
  const args = process.argv.slice(2);

  if (args.includes('--help') || args.includes('-h')) {
    console.log(`
${BOLD}🎮 1v1 Automated Multiplayer Test${RESET}

Usage: node artery/test-1v1-multiplayer.mjs [options]

Options:
  --duration=N     Run test for N seconds (default: 30)
  --help, -h       Show this help

This test:
  1. Opens AC with split~1v1 (two 1v1 instances stacked)
  2. Connects to both player frames via CDP
  3. Injects Camdoll AI controllers into each frame
  4. Players autonomously explore, follow the bot, and follow each other
  5. Logs position updates and UDP connectivity status
    `);
    process.exit(0);
  }

  let duration = 30;
  for (const arg of args) {
    if (arg.startsWith('--duration=')) {
      duration = parseInt(arg.split('=')[1]) || 30;
    }
  }

  try {
    await runTest(duration);
  } catch (e) {
    console.error(`\n${RED}Error: ${e.message}${RESET}`);
    process.exit(1);
  }
}

main();
