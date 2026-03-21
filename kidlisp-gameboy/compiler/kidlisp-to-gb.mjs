#!/usr/bin/env node
// KidLisp → Game Boy ROM Compiler
// Compiles simple command sequences to GBDK C code

import fs from 'fs';
import path from 'path';

// Note mapping for melody
const NOTE_MAP = {
  'c': 1046, 'd': 1175, 'e': 1319, 'f': 1397,
  'g': 1568, 'a': 1760, 'b': 1976
};

// Convert frequency to Game Boy register value
function freqToGB(freq) {
  // Game Boy frequency formula: f = 131072 / (2048 - x)
  // Solving for x: x = 2048 - (131072 / f)
  return Math.floor(2048 - (131072 / freq));
}

function generateMelodyC(notes) {
  // Generate simple direct sound register melody
  let c = '// Generated melody data\n';
  c += 'const uint16_t melody_notes[] = {\n';
  
  notes.forEach(note => {
    const freq = NOTE_MAP[note.toLowerCase()];
    const gbFreq = freqToGB(freq);
    c += `    ${gbFreq},  // ${note.toUpperCase()}\n`;
  });
  
  c += '};\n';
  c += `const uint8_t melody_length = ${notes.length};\n`;
  c += 'uint8_t melody_index = 0;\n';
  c += 'uint8_t melody_timer = 0;\n\n';
  
  return c;
}

function parseMelody(melodyString) {
  // Convert "ceg" → ["c", "e", "g"]
  return melodyString.split('');
}

function compileKidLisp(source, sourceName = 'kidlisp') {
  const lines = source.split('\n').map(l => l.trim()).filter(l => l && !l.startsWith(';'));
  // Keep only non-comment lines for splash screen
  const allLines = source.split('\n').filter(l => l.trim() && !l.trim().startsWith(';'));
  
  let graphicsCommands = [];
  let melodyNotes = null;
  let currentColor = 'WHITE';
  
  // Parse commands
  for (const line of lines) {
    const parts = line.split(/\s+/);
    const cmd = parts[0];
    
    if (cmd === 'wipe') {
      const color = parts[1]?.toUpperCase() || 'BLACK';
      graphicsCommands.push(`    // Wipe screen to ${color}`);
      // Invert: BLACK in KidLisp = WHITE (0) on GB, WHITE in KidLisp = BLACK (3) on GB
      const colorValue = color === 'WHITE' ? '3' : color === 'LTGREY' ? '2' : color === 'DKGREY' ? '1' : '0';
      graphicsCommands.push(`    fill_rect(0, 0, 160, 144, ${colorValue});`);
    }
    else if (cmd === 'ink') {
      currentColor = parts[1]?.toUpperCase() || 'WHITE';
      graphicsCommands.push(`    // Set ink color to ${currentColor}`);
      // Invert: WHITE in KidLisp = BLACK on GB, BLACK in KidLisp = WHITE on GB
      const colorValue = currentColor === 'WHITE' ? 'BLACK' : currentColor === 'BLACK' ? 'WHITE' : currentColor === 'LTGREY' ? 'DKGREY' : 'LTGREY';
      // For box/circle: forecolor = outline, backcolor = fill (when M_FILL used)
      // Set backcolor to opposite of forecolor for proper fill
      const fillColor = currentColor === 'BLACK' ? 'WHITE' : 'BLACK';
      graphicsCommands.push(`    color(${colorValue}, ${fillColor}, SOLID);`);
    }
    else if (cmd === 'line') {
      const [x1, y1, x2, y2] = parts.slice(1).map(Number);
      graphicsCommands.push(`    line(${x1}, ${y1}, ${x2}, ${y2});`);
    }
    else if (cmd === 'box') {
      const [x1, y1, x2, y2, fill] = parts.slice(1);
      const fillMode = fill === 'fill' ? 'M_FILL' : 'M_NOFILL';
      graphicsCommands.push(`    box(${x1}, ${y1}, ${x2}, ${y2}, ${fillMode});`);
    }
    else if (cmd === 'circle') {
      const [x, y, radius, fill] = parts.slice(1);
      const fillMode = fill === 'fill' ? 'M_FILL' : 'M_NOFILL';
      graphicsCommands.push(`    circle(${x}, ${y}, ${radius}, ${fillMode});`);
    }
    else if (cmd === 'melody') {
      const melodyStr = parts[1]?.replace(/['"]/g, '') || '';
      melodyNotes = parseMelody(melodyStr);
    }
  }
  
  // Generate C source
  let c = '#include <gb/gb.h>\n';
  c += '#include <stdint.h>\n';
  c += '#include <gb/drawing.h>\n';
  c += '#include <stdio.h>\n';
  c += '\n';
  
  // Add source code as string array for splash screen
  c += '// Original KidLisp source code\n';
  c += 'const char* source_lines[] = {\n';
  allLines.forEach(line => {
    // Escape quotes and backslashes
    const escaped = line.replace(/\\/g, '\\\\').replace(/"/g, '\\"');
    c += `    "${escaped}",\n`;
  });
  c += '    NULL\n';
  c += '};\n\n';
  
  c += `const char* rom_name = "${sourceName}";\n\n`;
  
  // Add melody data if present
  if (melodyNotes) {
    c += generateMelodyC(melodyNotes);
    c += '\n';
  }
  
  // Splash screen function
  c += '// Aesthetic Computer splash screen\n';
  c += 'void show_splash(void) {\n';
  c += '    uint8_t i, progress;\n';
  c += '    \n';
  c += '    // Clear screen\n';
  c += '    fill_rect(0, 0, 160, 144, BLACK);\n';
  c += '    \n';
  c += '    // Title\n';
  c += '    color(WHITE, BLACK, SOLID);\n';
  c += '    gotogxy(1, 1);\n';
  c += '    gprintf("AESTHETIC.COMPUTER");\n';
  c += '    gotogxy(2, 2);\n';
  c += '    gprintf("KidLisp ROM");\n';
  c += '    \n';
  c += '    // ROM name\n';
  c += '    gotogxy(2, 4);\n';
  c += '    gprintf("%s", rom_name);\n';
  c += '    \n';
  c += '    // Source code (max 8 lines to avoid overflow)\n';
  c += '    gotogxy(1, 6);\n';
  c += '    gprintf("Source:");\n';
  c += '    for (i = 0; i < 8 && source_lines[i] != NULL; i++) {\n';
  c += '        gotogxy(1, 7 + i);\n';
  c += '        // Truncate lines to 18 chars to prevent bleed\n';
  c += '        gprintf("%.18s", source_lines[i]);\n';
  c += '    }\n';
  c += '    \n';
  c += '    // Progress bar animation (3 seconds) - 1px line at top\n';
  c += '    for (progress = 0; progress <= 160; progress++) {\n';
  c += '        // Draw 1px progress using plot_point to avoid line() conflicts\n';
  c += '        if (progress > 0) {\n';
  c += '            plot_point(progress - 1, 0);\n';
  c += '        }\n';
  c += '        vsync();\n';
  c += '    }\n';
  c += '}\n\n';
  
  // Main function
  c += 'void main(void) {\n';
  
  c += '    // Graphics setup\n';
  c += '    DISPLAY_ON;\n';
  c += '    mode(get_mode() | M_NO_SCROLL | M_NO_INTERP);\n\n';
  
  c += '    // Show splash screen\n';
  c += '    show_splash();\n\n';
  
  c += '    // Completely reset graphics after splash\n';
  c += '    DISPLAY_OFF;\n';
  c += '    // Clear background tile map (20x18 tiles) - critical for removing text!\n';
  c += '    fill_bkg_rect(0, 0, 20, 18, 0);\n';
  c += '    // Clear screen buffer\n';
  c += '    fill_rect(0, 0, 160, 144, WHITE);\n';
  c += '    DISPLAY_ON;\n';
  c += '    mode(get_mode() | M_NO_SCROLL | M_NO_INTERP);\n\n';
  
  if (melodyNotes) {
    c += '    // Initialize sound system\n';
    c += '    NR52_REG = 0x80;  // Sound ON\n';
    c += '    NR51_REG = 0x11;  // Channel 1 to left and right\n';
    c += '    NR50_REG = 0x77;  // Max volume\n\n';
    c += '    // Configure Channel 1 (square wave)\n';
    c += '    NR10_REG = 0x00;  // No sweep\n';
    c += '    NR11_REG = 0x80;  // 50% duty cycle\n';
    c += '    NR12_REG = 0xF3;  // Max volume, no envelope\n\n';
  }
  
  c += '    // Execute KidLisp commands\n';
  c += graphicsCommands.join('\n');
  c += '\n\n';
  
  if (melodyNotes) {
    c += '    // Main loop with melody playback\n';
    c += '    while(1) {\n';
    c += '        vsync();\n';
    c += '        melody_timer++;\n';
    c += '        if (melody_timer >= 30) {  // ~0.5 seconds per note at 60fps\n';
    c += '            melody_timer = 0;\n';
    c += '            uint16_t freq = melody_notes[melody_index];\n';
    c += '            NR13_REG = freq & 0xFF;\n';
    c += '            NR14_REG = 0x80 | ((freq >> 8) & 0x07);  // Trigger note\n';
    c += '            melody_index++;\n';
    c += '            if (melody_index >= melody_length) melody_index = 0;\n';
    c += '        }\n';
    c += '    }\n';
  } else {
    c += '    // Main loop\n';
    c += '    while(1) {\n';
    c += '        vsync();\n';
    c += '    }\n';
  }
  c += '}\n';
  
  return c;
}

// CLI
if (process.argv.length < 3) {
  console.log('Usage: kidlisp-to-gb.mjs <input.lisp> [output.c]');
  process.exit(1);
}

const inputFile = process.argv[2];
const outputFile = process.argv[3] || inputFile.replace(/\.lisp$/, '.c');

// Extract ROM name from filename
const sourceName = path.basename(inputFile, '.lisp');

const source = fs.readFileSync(inputFile, 'utf-8');
const compiled = compileKidLisp(source, sourceName);

fs.writeFileSync(outputFile, compiled);
console.log(`✅ Compiled ${inputFile} → ${outputFile}`);
