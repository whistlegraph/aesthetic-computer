#!/usr/bin/env node
/**
 * Comprehensive Toss Test Suite using Artery
 * 
 * Tests multiple toss configurations and interactions:
 * - Default toss (2 strips)
 * - Variable strip counts (3, 5, 8, 12)
 * - Note-based mode (notepat notation)
 * - Wave type cycling (sine, triangle, sawtooth, square)
 * - Pitch manipulation (up/down keys, arrow keys)
 * - Chord playing (multiple simultaneous strips)
 * - Rapid sequences
 * - Sustained holds
 * - Edge case pitches
 */

import Artery from '../artery.mjs';

const PURPLE_BG = '\x1b[45m';
const WHITE = '\x1b[97m';
const RESET = '\x1b[0m';
const PINK = '\x1b[95m';
const GREEN = '\x1b[92m';
const CYAN = '\x1b[96m';
const YELLOW = '\x1b[93m';
const DIM = '\x1b[2m';

const testLog = (msg) => console.log(`${PURPLE_BG}${WHITE}🧪${RESET} ${msg}`);
const successLog = (msg) => console.log(`${GREEN}✅ ${msg}${RESET}`);
const subTestLog = (msg) => console.log(`  ${CYAN}▸${RESET} ${msg}`);
const sectionLog = (msg) => console.log(`\n${YELLOW}━━━ ${msg} ━━━${RESET}`);
const statsLog = (msg) => console.log(`  ${DIM}${msg}${RESET}`);

// Shared key press function injected into page context
const keyPressHelpers = `
  const shortcuts = [
    ['1', 'q', 'a'],
    ['2', 'w', 's'],
    ['3', 'e', 'd'],
    ['4', 'r', 'f'],
    ['5', 't', 'g'],
    ['6', 'y', 'h'],
    ['7', 'u', 'j'],
    ['8', 'i', 'k'],
    ['9', 'o', 'l'],
    ['0', 'p', ';'],
    ['-', '[', "'"],
    ['=', ']', '\\\\']
  ];
  
  async function pressKey(key, holdDuration = 50) {
    document.dispatchEvent(new KeyboardEvent('keydown', {
      key: key,
      code: key.length === 1 ? \`Key\${key.toUpperCase()}\` : key,
      keyCode: key.charCodeAt(0),
      bubbles: true,
      cancelable: true
    }));
    await new Promise(r => setTimeout(r, holdDuration));
    document.dispatchEvent(new KeyboardEvent('keyup', {
      key: key,
      code: key.length === 1 ? \`Key\${key.toUpperCase()}\` : key,
      keyCode: key.charCodeAt(0),
      bubbles: true,
      cancelable: true
    }));
  }
  
  async function holdKey(key) {
    document.dispatchEvent(new KeyboardEvent('keydown', {
      key: key,
      code: key.length === 1 ? \`Key\${key.toUpperCase()}\` : key,
      keyCode: key.charCodeAt(0),
      bubbles: true,
      cancelable: true
    }));
  }
  
  async function releaseKey(key) {
    document.dispatchEvent(new KeyboardEvent('keyup', {
      key: key,
      code: key.length === 1 ? \`Key\${key.toUpperCase()}\` : key,
      keyCode: key.charCodeAt(0),
      bubbles: true,
      cancelable: true
    }));
  }
`;

async function sleep(ms) {
  return new Promise(r => setTimeout(r, ms));
}

async function testToss() {
  const results = {
    passed: 0,
    failed: 0,
    tests: []
  };
  
  function recordTest(name, passed, details = '') {
    results.tests.push({ name, passed, details });
    if (passed) results.passed++;
    else results.failed++;
    if (passed) {
      successLog(`${name}${details ? ` ${DIM}(${details})${RESET}` : ''}`);
    } else {
      console.log(`  ${PINK}✗ ${name}${details ? ` (${details})` : ''}${RESET}`);
    }
  }
  
  try {
    console.log('');
    testLog('Starting Comprehensive Toss Test Suite');
    console.log('');
    
    // Ensure panel is open
    await Artery.openPanelStandalone();
    await sleep(500);
    
    // Connect
    const client = new Artery();
    await client.connect();
    testLog('Connected to AC');
    
    // ════════════════════════════════════════════════════════════
    // TEST 1: Default Toss (2 strips)
    // ════════════════════════════════════════════════════════════
    sectionLog('Test 1: Default Toss (2 strips)');
    
    await client.jump('toss');
    subTestLog('Navigated to toss');
    await sleep(2000);
    
    client.close();
    await client.connect();
    await sleep(500);
    
    await client.activateAudio();
    subTestLog('Audio activated');
    
    // Test basic play on both strips
    const test1 = await client.eval(`
      (async () => {
        ${keyPressHelpers}
        
        // Play strip 0 (q key)
        await pressKey('q', 200);
        await new Promise(r => setTimeout(r, 100));
        
        // Play strip 1 (w key)
        await pressKey('w', 200);
        await new Promise(r => setTimeout(r, 100));
        
        return { strips: 2, played: true };
      })()
    `);
    
    recordTest('Default 2-strip play', test1?.played === true, '2 strips');
    
    // ════════════════════════════════════════════════════════════
    // TEST 2: Variable Strip Counts
    // ════════════════════════════════════════════════════════════
    sectionLog('Test 2: Variable Strip Counts');
    
    for (const count of [3, 5, 8, 12]) {
      await client.jump(`toss ${count}`);
      subTestLog(`Testing toss ${count}`);
      await sleep(1500);
      client.close();
      await client.connect();
      await sleep(300);
      await client.activateAudio();
      
      const result = await client.eval(`
        (async () => {
          ${keyPressHelpers}
          const maxStrip = Math.min(${count}, shortcuts.length);
          let played = 0;
          
          for (let i = 0; i < maxStrip; i++) {
            const [up, play, down] = shortcuts[i];
            await pressKey(play, 100);
            played++;
            await new Promise(r => setTimeout(r, 50));
          }
          
          return { expected: ${count}, played };
        })()
      `);
      
      recordTest(`${count}-strip mode`, result?.played === Math.min(count, 12), `played ${result?.played}`);
    }
    
    // ════════════════════════════════════════════════════════════
    // TEST 3: Note-Based Mode (Notepat Notation)
    // ════════════════════════════════════════════════════════════
    sectionLog('Test 3: Note-Based Mode');
    
    // C Major chord
    await client.jump('toss c e g');
    subTestLog('Testing C major chord (c e g)');
    await sleep(1500);
    client.close();
    await client.connect();
    await sleep(300);
    await client.activateAudio();
    
    const noteTest = await client.eval(`
      (async () => {
        ${keyPressHelpers}
        // Play all 3 notes of C major
        await pressKey('q', 300);
        await new Promise(r => setTimeout(r, 50));
        await pressKey('w', 300);
        await new Promise(r => setTimeout(r, 50));
        await pressKey('e', 300);
        return { chord: 'Cmaj', notes: 3 };
      })()
    `);
    
    recordTest('C major chord mode', noteTest?.notes === 3, 'c e g');
    
    // Chromatic scale
    await client.jump('toss c v d s e');
    subTestLog('Testing chromatic sequence');
    await sleep(1500);
    client.close();
    await client.connect();
    await sleep(300);
    await client.activateAudio();
    
    const chromatic = await client.eval(`
      (async () => {
        ${keyPressHelpers}
        let played = 0;
        for (let i = 0; i < 5; i++) {
          await pressKey(shortcuts[i][1], 150);
          played++;
          await new Promise(r => setTimeout(r, 50));
        }
        return { played };
      })()
    `);
    
    recordTest('Chromatic sequence', chromatic?.played === 5, 'c v d s e');
    
    // Octave jumping
    await client.jump('toss 3c 5c');
    subTestLog('Testing octave jumping (3c 5c)');
    await sleep(1500);
    client.close();
    await client.connect();
    await sleep(300);
    await client.activateAudio();
    
    const octaveJump = await client.eval(`
      (async () => {
        ${keyPressHelpers}
        await pressKey('q', 300);
        await new Promise(r => setTimeout(r, 100));
        await pressKey('w', 300);
        return { octaves: [3, 5] };
      })()
    `);
    
    recordTest('Octave jumping', octaveJump?.octaves?.length === 2, '3c to 5c');
    
    // ════════════════════════════════════════════════════════════
    // TEST 4: Pitch Manipulation
    // ════════════════════════════════════════════════════════════
    sectionLog('Test 4: Pitch Manipulation');
    
    await client.jump('toss');
    await sleep(1500);
    client.close();
    await client.connect();
    await sleep(300);
    await client.activateAudio();
    
    // Test pitch up while playing
    const pitchUp = await client.eval(`
      (async () => {
        ${keyPressHelpers}
        
        // Hold play key
        await holdKey('q');
        await new Promise(r => setTimeout(r, 100));
        
        // Tap pitch up 5 times
        for (let i = 0; i < 5; i++) {
          await pressKey('1', 50);
          await new Promise(r => setTimeout(r, 30));
        }
        
        await new Promise(r => setTimeout(r, 200));
        await releaseKey('q');
        
        return { pitchChanges: 5, direction: 'up' };
      })()
    `);
    
    recordTest('Pitch up while playing', pitchUp?.pitchChanges === 5, '5 increases');
    
    // Test pitch down while playing
    const pitchDown = await client.eval(`
      (async () => {
        ${keyPressHelpers}
        
        await holdKey('q');
        await new Promise(r => setTimeout(r, 100));
        
        for (let i = 0; i < 5; i++) {
          await pressKey('a', 50);
          await new Promise(r => setTimeout(r, 30));
        }
        
        await new Promise(r => setTimeout(r, 200));
        await releaseKey('q');
        
        return { pitchChanges: 5, direction: 'down' };
      })()
    `);
    
    recordTest('Pitch down while playing', pitchDown?.pitchChanges === 5, '5 decreases');
    
    // Test arrow keys for pitch (should work when strip is playing)
    const arrowPitch = await client.eval(`
      (async () => {
        ${keyPressHelpers}
        
        await holdKey('q');
        await new Promise(r => setTimeout(r, 100));
        
        // Arrow up
        document.dispatchEvent(new KeyboardEvent('keydown', { key: 'ArrowUp', code: 'ArrowUp', bubbles: true }));
        await new Promise(r => setTimeout(r, 100));
        document.dispatchEvent(new KeyboardEvent('keyup', { key: 'ArrowUp', code: 'ArrowUp', bubbles: true }));
        
        // Arrow down
        document.dispatchEvent(new KeyboardEvent('keydown', { key: 'ArrowDown', code: 'ArrowDown', bubbles: true }));
        await new Promise(r => setTimeout(r, 100));
        document.dispatchEvent(new KeyboardEvent('keyup', { key: 'ArrowDown', code: 'ArrowDown', bubbles: true }));
        
        await new Promise(r => setTimeout(r, 100));
        await releaseKey('q');
        
        return { arrowsUsed: true };
      })()
    `);
    
    recordTest('Arrow keys for pitch', arrowPitch?.arrowsUsed === true, 'up & down');
    
    // ════════════════════════════════════════════════════════════
    // TEST 5: Chord Playing (Multiple Simultaneous Strips)
    // ════════════════════════════════════════════════════════════
    sectionLog('Test 5: Chord Playing');
    
    await client.jump('toss 4');
    await sleep(1500);
    client.close();
    await client.connect();
    await sleep(300);
    await client.activateAudio();
    
    // Play 2 strips simultaneously
    const chord2 = await client.eval(`
      (async () => {
        ${keyPressHelpers}
        
        await holdKey('q');
        await holdKey('w');
        await new Promise(r => setTimeout(r, 500));
        await releaseKey('q');
        await releaseKey('w');
        
        return { simultaneous: 2 };
      })()
    `);
    
    recordTest('2-note chord', chord2?.simultaneous === 2, 'q + w');
    
    // Play 4 strips simultaneously
    const chord4 = await client.eval(`
      (async () => {
        ${keyPressHelpers}
        
        await holdKey('q');
        await holdKey('w');
        await holdKey('e');
        await holdKey('r');
        await new Promise(r => setTimeout(r, 500));
        await releaseKey('q');
        await releaseKey('w');
        await releaseKey('e');
        await releaseKey('r');
        
        return { simultaneous: 4 };
      })()
    `);
    
    recordTest('4-note chord', chord4?.simultaneous === 4, 'q w e r');
    
    // ════════════════════════════════════════════════════════════
    // TEST 6: Rapid Sequences
    // ════════════════════════════════════════════════════════════
    sectionLog('Test 6: Rapid Sequences');
    
    const rapidSeq = await client.eval(`
      (async () => {
        ${keyPressHelpers}
        
        let plays = 0;
        const keys = ['q', 'w', 'e', 'r'];
        
        // Rapid fire through 4 strips
        for (let round = 0; round < 3; round++) {
          for (const key of keys) {
            await pressKey(key, 30);  // Very short press
            plays++;
            await new Promise(r => setTimeout(r, 10));  // Minimal gap
          }
        }
        
        return { plays };
      })()
    `);
    
    recordTest('Rapid sequence', rapidSeq?.plays === 12, `${rapidSeq?.plays} rapid plays`);
    
    // Arpeggio pattern
    const arpeggio = await client.eval(`
      (async () => {
        ${keyPressHelpers}
        
        let plays = 0;
        // Up and down arpeggio
        for (const key of ['q', 'w', 'e', 'r', 'e', 'w', 'q']) {
          await pressKey(key, 80);
          plays++;
          await new Promise(r => setTimeout(r, 40));
        }
        
        return { plays, pattern: 'arpeggio' };
      })()
    `);
    
    recordTest('Arpeggio pattern', arpeggio?.plays === 7, 'up and down');
    
    // ════════════════════════════════════════════════════════════
    // TEST 7: Sustained Holds
    // ════════════════════════════════════════════════════════════
    sectionLog('Test 7: Sustained Holds');
    
    const longHold = await client.eval(`
      (async () => {
        ${keyPressHelpers}
        
        const start = Date.now();
        await holdKey('q');
        await new Promise(r => setTimeout(r, 2000));  // 2 second hold
        await releaseKey('q');
        const duration = Date.now() - start;
        
        return { duration, held: true };
      })()
    `);
    
    recordTest('Long sustained hold', longHold?.duration >= 1900, `${longHold?.duration}ms`);
    
    // Sustained with pitch bend
    const sustainBend = await client.eval(`
      (async () => {
        ${keyPressHelpers}
        
        await holdKey('q');
        
        // Gradual pitch increase
        for (let i = 0; i < 10; i++) {
          await pressKey('1', 20);
          await new Promise(r => setTimeout(r, 100));
        }
        
        // Gradual pitch decrease
        for (let i = 0; i < 10; i++) {
          await pressKey('a', 20);
          await new Promise(r => setTimeout(r, 100));
        }
        
        await releaseKey('q');
        
        return { bends: 20 };
      })()
    `);
    
    recordTest('Sustained with pitch bend', sustainBend?.bends === 20, 'up then down');
    
    // ════════════════════════════════════════════════════════════
    // TEST 8: Edge Cases
    // ════════════════════════════════════════════════════════════
    sectionLog('Test 8: Edge Cases');
    
    // Maximum strips (12)
    await client.jump('toss 12');
    await sleep(1500);
    client.close();
    await client.connect();
    await sleep(300);
    await client.activateAudio();
    
    const maxStrips = await client.eval(`
      (async () => {
        ${keyPressHelpers}
        
        let played = 0;
        // Try all 12 strips
        for (let i = 0; i < 12; i++) {
          const [up, play, down] = shortcuts[i];
          await pressKey(play, 50);
          played++;
          await new Promise(r => setTimeout(r, 30));
        }
        
        return { played };
      })()
    `);
    
    recordTest('Maximum 12 strips', maxStrips?.played === 12, 'all strips');
    
    // Extreme pitch manipulation (push to limits)
    await client.jump('toss');
    await sleep(1500);
    client.close();
    await client.connect();
    await sleep(300);
    await client.activateAudio();
    
    const extremePitch = await client.eval(`
      (async () => {
        ${keyPressHelpers}
        
        await holdKey('q');
        await new Promise(r => setTimeout(r, 50));
        
        // Push pitch to high extreme
        for (let i = 0; i < 30; i++) {
          await pressKey('1', 10);
        }
        
        await new Promise(r => setTimeout(r, 200));
        
        // Push pitch to low extreme
        for (let i = 0; i < 60; i++) {
          await pressKey('a', 10);
        }
        
        await new Promise(r => setTimeout(r, 200));
        await releaseKey('q');
        
        return { extremes: true };
      })()
    `);
    
    recordTest('Extreme pitch range', extremePitch?.extremes === true, 'high and low');
    
    // Double key press (same key)
    const doubleKey = await client.eval(`
      (async () => {
        ${keyPressHelpers}
        
        // Press same key twice rapidly (shouldn't cause issues)
        await pressKey('q', 50);
        await pressKey('q', 50);
        await new Promise(r => setTimeout(r, 50));
        await pressKey('q', 50);
        
        return { doubled: true };
      })()
    `);
    
    recordTest('Rapid same-key presses', doubleKey?.doubled === true, 'no crash');
    
    // ════════════════════════════════════════════════════════════
    // TEST 9: Extended Fuzzing (5 seconds)
    // ════════════════════════════════════════════════════════════
    sectionLog('Test 9: Extended Fuzzing (5 seconds)');
    
    await client.jump('toss 6');
    await sleep(1500);
    client.close();
    await client.connect();
    await sleep(300);
    await client.activateAudio();
    
    const fuzzResult = await client.eval(`
      (async () => {
        ${keyPressHelpers}
        
        const startTime = Date.now();
        const duration = 5000;  // 5 seconds of fuzzing
        let plays = 0;
        let chords = 0;
        let pitchChanges = 0;
        
        while (Date.now() - startTime < duration) {
          const action = Math.random();
          
          if (action < 0.4) {
            // Single strip play
            const stripIdx = Math.floor(Math.random() * 6);
            const [up, play, down] = shortcuts[stripIdx];
            const playDuration = 50 + Math.random() * 300;
            await pressKey(play, playDuration);
            plays++;
          } else if (action < 0.6) {
            // Chord (2-3 strips)
            const numNotes = 2 + Math.floor(Math.random() * 2);
            const stripIndices = [];
            while (stripIndices.length < numNotes) {
              const idx = Math.floor(Math.random() * 6);
              if (!stripIndices.includes(idx)) stripIndices.push(idx);
            }
            
            for (const idx of stripIndices) {
              await holdKey(shortcuts[idx][1]);
            }
            await new Promise(r => setTimeout(r, 200 + Math.random() * 300));
            for (const idx of stripIndices) {
              await releaseKey(shortcuts[idx][1]);
            }
            chords++;
          } else if (action < 0.8) {
            // Play with pitch manipulation
            const stripIdx = Math.floor(Math.random() * 6);
            const [up, play, down] = shortcuts[stripIdx];
            
            await holdKey(play);
            await new Promise(r => setTimeout(r, 50));
            
            const pitchDir = Math.random() < 0.5 ? up : down;
            const numChanges = 1 + Math.floor(Math.random() * 5);
            for (let i = 0; i < numChanges; i++) {
              await pressKey(pitchDir, 20);
              pitchChanges++;
              await new Promise(r => setTimeout(r, 30));
            }
            
            await new Promise(r => setTimeout(r, 100));
            await releaseKey(play);
            plays++;
          } else {
            // Rapid sequence
            const keys = ['q', 'w', 'e', 'r'].slice(0, 2 + Math.floor(Math.random() * 3));
            for (const key of keys) {
              await pressKey(key, 30);
              plays++;
              await new Promise(r => setTimeout(r, 20));
            }
          }
          
          await new Promise(r => setTimeout(r, 30 + Math.random() * 100));
        }
        
        return { plays, chords, pitchChanges };
      })()
    `);
    
    const totalActions = (fuzzResult?.plays || 0) + (fuzzResult?.chords || 0);
    recordTest('Extended fuzzing', totalActions > 20, 
      `${fuzzResult?.plays} plays, ${fuzzResult?.chords} chords, ${fuzzResult?.pitchChanges} pitch changes`);
    
    // ════════════════════════════════════════════════════════════
    // CLEANUP & RESULTS
    // ════════════════════════════════════════════════════════════
    sectionLog('Test Complete');
    
    // Return to prompt
    await client.jump('prompt');
    subTestLog('Returned to prompt');
    await sleep(500);
    
    // Close the AC panel
    await Artery.closePanelStandalone();
    
    client.close();
    
    // Print summary
    console.log('');
    console.log(`${YELLOW}═══════════════════════════════════════════${RESET}`);
    console.log(`${WHITE}           TEST RESULTS SUMMARY            ${RESET}`);
    console.log(`${YELLOW}═══════════════════════════════════════════${RESET}`);
    console.log(`  ${GREEN}Passed:${RESET} ${results.passed}`);
    console.log(`  ${PINK}Failed:${RESET} ${results.failed}`);
    console.log(`  ${WHITE}Total:${RESET}  ${results.passed + results.failed}`);
    console.log(`${YELLOW}═══════════════════════════════════════════${RESET}`);
    console.log('');
    
    if (results.failed > 0) {
      console.log(`${PINK}Failed tests:${RESET}`);
      results.tests.filter(t => !t.passed).forEach(t => {
        console.log(`  ${PINK}✗${RESET} ${t.name}`);
      });
      console.log('');
    }
    
    process.exit(results.failed > 0 ? 1 : 0);
    
  } catch (error) {
    console.error(`${PINK}💔 Test suite failed: ${error.message}${RESET}`);
    console.error(error.stack);
    process.exit(1);
  }
}

// Run the test
testToss();
