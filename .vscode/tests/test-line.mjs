/**
 * Automated test for the 'line' piece
 * 
 * This test:
 * 1. Opens the AC panel
 * 2. Creates a new 128x128 painting with 'new 128'
 * 3. Navigates to the line tool
 * 4. Draws random lines with varying colors, positions, and thicknesses
 * 
 * Usage:
 *   test-line [duration_ms]
 * 
 * Example:
 *   test-line 30000  # Run for 30 seconds
 */

import Artery from '../../artery/artery.mjs';

async function sleep(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

async function testLine(duration = 30000) {
  
  try {
    console.log('\n🧪 Starting Line drawing test\n');
    
    // Ensure panel is open
    await Artery.openPanelStandalone();
    await sleep(500);
    
    // Connect to AC
    const client = new Artery();
    await client.connect();
    console.log('🧪 Connected to AC');
    
    // Jump to prompt to execute 'new 128'
    await client.jump('prompt');
    await sleep(500);
    console.log('🧪 Navigated to prompt');
    
    // Need to reconnect after navigation
    await client.close();
    await client.connect();
    console.log('🧪 Reconnected after navigation');
    
    // Type 'new 128' to create a new painting
    await client.type('new 128');
    await sleep(100);
    await client.pressKey('enter');
    await sleep(800); // Wait for painting to be created
    console.log('🧪 Created new 128x128 painting');
    
    // Reconnect after painting creation (execution context may change)
    await client.close();
    await client.connect();
    
    // Navigate to line tool
    await client.jump('line');
    await sleep(500);
    console.log('🧪 Navigated to line tool');
    
    // Reconnect after navigation
    await client.close();
    await client.connect();
    console.log('🧪 Reconnected after navigation to line');
    
    // Activate audio (for any sound effects)
    await client.activateAudio();
    await sleep(300);
    
    console.log(`🧪 Drawing random lines for ${duration}ms...\n`);
    
    // Inject the fuzzing algorithm into AC context
    const fuzzingCode = `
      (async () => {
        const canvas = document.querySelector('canvas');
        if (!canvas) {
          console.log('No canvas found');
          return 0;
        }
        
        const width = canvas.width;
        const height = canvas.height;
        let linesDrawn = 0;
        
        // Helper to generate random color
        function randomColor() {
          return Math.floor(Math.random() * 256);
        }
        
        // Helper to get random coordinate
        function randomX() {
          return Math.floor(Math.random() * width);
        }
        
        function randomY() {
          return Math.floor(Math.random() * height);
        }
        
        // Draw lines for the specified duration
        const startTime = Date.now();
        const duration = ${duration};
        
        while (Date.now() - startTime < duration) {
          // Random color change (30% chance)
          if (Math.random() < 0.3) {
            const r = randomColor();
            const g = randomColor();
            const b = randomColor();
            
            // Type color command
            const colorCmd = \`\${r} \${g} \${b} wipe\`;
            
            // Press escape to get to prompt
            document.dispatchEvent(new KeyboardEvent('keydown', {
              key: 'Escape',
              code: 'Escape',
              keyCode: 27,
              bubbles: true,
              cancelable: true
            }));
            await new Promise(r => setTimeout(r, 50));
            document.dispatchEvent(new KeyboardEvent('keyup', {
              key: 'Escape',
              code: 'Escape',
              keyCode: 27,
              bubbles: true,
              cancelable: true
            }));
            await new Promise(r => setTimeout(r, 150));
            
            // Type the color command
            for (const char of colorCmd) {
              document.dispatchEvent(new KeyboardEvent('keydown', {
                key: char,
                code: char === ' ' ? 'Space' : \`Key\${char.toUpperCase()}\`,
                keyCode: char.charCodeAt(0),
                bubbles: true,
                cancelable: true
              }));
              await new Promise(r => setTimeout(r, 10));
              document.dispatchEvent(new KeyboardEvent('keyup', {
                key: char,
                code: char === ' ' ? 'Space' : \`Key\${char.toUpperCase()}\`,
                keyCode: char.charCodeAt(0),
                bubbles: true,
                cancelable: true
              }));
              await new Promise(r => setTimeout(r, 5));
            }
            
            // Press enter
            document.dispatchEvent(new KeyboardEvent('keydown', {
              key: 'Enter',
              code: 'Enter',
              keyCode: 13,
              bubbles: true,
              cancelable: true
            }));
            await new Promise(r => setTimeout(r, 50));
            document.dispatchEvent(new KeyboardEvent('keyup', {
              key: 'Enter',
              code: 'Enter',
              keyCode: 13,
              bubbles: true,
              cancelable: true
            }));
            await new Promise(r => setTimeout(r, 200));
          }
          
          // Draw a line by dragging from random point to random point
          const x1 = randomX();
          const y1 = randomY();
          const x2 = randomX();
          const y2 = randomY();
          
          // Mouse down at starting point
          document.dispatchEvent(new MouseEvent('mousedown', {
            clientX: x1,
            clientY: y1,
            bubbles: true,
            cancelable: true,
            button: 0
          }));
          
          await new Promise(r => setTimeout(r, 20));
          
          // Interpolate several points between start and end for smooth line
          const steps = 5 + Math.floor(Math.random() * 10); // 5-15 steps
          for (let i = 1; i <= steps; i++) {
            const t = i / steps;
            const x = Math.floor(x1 + (x2 - x1) * t);
            const y = Math.floor(y1 + (y2 - y1) * t);
            
            document.dispatchEvent(new MouseEvent('mousemove', {
              clientX: x,
              clientY: y,
              bubbles: true,
              cancelable: true,
              buttons: 1
            }));
            
            await new Promise(r => setTimeout(r, 10 + Math.random() * 20)); // 10-30ms
          }
          
          // Mouse up at ending point
          document.dispatchEvent(new MouseEvent('mouseup', {
            clientX: x2,
            clientY: y2,
            bubbles: true,
            cancelable: true,
            button: 0
          }));
          
          linesDrawn++;
          
          // Small delay between lines
          await new Promise(r => setTimeout(r, 50 + Math.random() * 100)); // 50-150ms
        }
        
        return linesDrawn;
      })();
    `;
    
    // Execute the fuzzing algorithm
    const result = await client.eval(fuzzingCode);
    const linesDrawn = parseInt(result) || 0;
    
    console.log(`\n✅ Test completed! Drew ${linesDrawn} lines\n`);
    
    await client.close();
    return linesDrawn;
    
  } catch (error) {
    console.error(`\n❌ Test failed: ${error.message}\n`);
    await client.close();
    process.exit(1);
  }
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  const duration = parseInt(process.argv[2]) || 30000;
  testLine(duration);
}

export default testLine;
