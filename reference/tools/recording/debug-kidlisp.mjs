#!/usr/bin/env node

// Debug KidLisp $code evaluation to understand why embedded pieces aren't executing

import { KidLisp } from '../../../system/public/aesthetic.computer/lib/kidlisp.mjs';

console.log('🔍 Debugging KidLisp $code evaluation...\n');

// Create a KidLisp instance
const kidlispInstance = new KidLisp();

// The $cow source 
const cowSource = `($39i 0 0 w h 128)
($r2f 0 0 w h 128)
(contrast 1.5)`;

console.log('📄 $cow source:');
console.log(cowSource);
console.log('');

// Parse the source
console.log('🔍 Parsing...');
const parsed = kidlispInstance.parse(cowSource);
console.log('📊 Parsed AST:', JSON.stringify(parsed, null, 2));
console.log('📊 Number of statements:', parsed.length);
console.log('');

// Check global environment
console.log('🌍 Checking global environment...');
const globalEnv = kidlispInstance.getGlobalEnv();
console.log('📋 Global env keys:', Object.keys(globalEnv));
console.log('🔍 Has embed function:', 'embed' in globalEnv);
console.log('');

// Create a mock API
const mockAPI = {
  screen: { width: 256, height: 256 },
  pen: { x: 0, y: 0 },
  wipe: () => console.log('🎨 wipe called'),
  ink: () => console.log('🎨 ink called'),
  contrast: (...args) => console.log(`🎨 Contrast: ${args[0]}`),
  flood: () => console.log('🎨 flood called'),
  black: () => console.log('🎨 black called'),
  write: (...args) => console.log(`🎨 write called with:`, args),
  page: function(buffer) {
    if (buffer && buffer.width && buffer.height) {
      console.log(`📄 Switched to buffer: ${buffer.width}x${buffer.height}`);
      // Update the screen reference for embed system
      this.screen = {
        width: buffer.width,
        height: buffer.height,
        pixels: buffer.pixels || new Uint8ClampedArray(buffer.width * buffer.height * 4)
      };
    } else {
      console.warn('⚠️ page() called with invalid buffer:', buffer);
    }
  },
  paste: function(sourceBuffer, x = 0, y = 0) {
    if (sourceBuffer && sourceBuffer.pixels) {
      console.log(`🎨 PASTE CALLED: ${sourceBuffer.width}x${sourceBuffer.height} → (${x}, ${y})`);
      console.log(`🎨 PASTE: First pixel:`, sourceBuffer.pixels[0], sourceBuffer.pixels[1], sourceBuffer.pixels[2], sourceBuffer.pixels[3]);
      console.log(`✅ PASTE completed (debug mode)`);
    } else {
      console.warn('⚠️ paste() called with invalid sourceBuffer');
    }
  },
  w: 256,
  h: 256,
  width: 256,
  height: 256
};

// Set the API on the instance for $code evaluation
kidlispInstance.api = mockAPI;

console.log('🔄 Evaluating each statement...');
for (let i = 0; i < parsed.length; i++) {
  const stmt = parsed[i];
  console.log(`\n📝 Statement ${i + 1}: ${JSON.stringify(stmt)}`);
  
  try {
    const result = kidlispInstance.evaluate([stmt], mockAPI, kidlispInstance.localEnv);
    console.log(`✅ Result: ${result}`);
  } catch (error) {
    console.log(`❌ Error: ${error.message}`);
  }
}

console.log('\n🔄 Evaluating all statements together...');
try {
  const result = kidlispInstance.evaluate(parsed, mockAPI, kidlispInstance.localEnv);
  console.log(`✅ Final result: ${result}`);
} catch (error) {
  console.log(`❌ Error: ${error.message}`);
}

console.log('\n🏁 Debug complete!');