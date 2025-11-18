#!/usr/bin/env node
// KidLisp → Game Boy ROM Compiler
// Compiles simple command sequences to GBDK C code

import fs from 'fs';
import path from 'path';

// Note mapping for melody
const NOTE_MAP = {
  'c': 'C_4', 'd': 'D_4', 'e': 'E_4', 'f': 'F_4',
  'g': 'G_4', 'a': 'A_4', 'b': 'B_4'
};

function parseMelody(melodyString) {
  // Convert "ceg" → ["C_4", "E_4", "G_4"]
  return melodyString.split('').map(note => NOTE_MAP[note.toLowerCase()] || '___');
}

function generateMelodyC(notes) {
  // Generate hUGEDriver-compatible pattern array
  let c = '// Generated melody pattern\n';
  c += 'static const unsigned char melody_pattern[] = {\n';
  
  notes.forEach((note, i) => {
    // Each note plays for ~0.5 second (8 ticks at tempo 150)
    c += `    DN(${note},0,0x000),\n`;
    for (let j = 0; j < 7; j++) {
      c += `    DN(___,0,0x000),\n`;
    }
  });
  
  c += '};\n\n';
  
  // Generate song structure
  c += 'static const unsigned char order_cnt = 1;\n';
  c += 'static const unsigned char* const order1[] = {melody_pattern};\n';
  c += 'static const unsigned char* const order2[] = {melody_pattern};\n';
  c += 'static const unsigned char* const order3[] = {melody_pattern};\n';
  c += 'static const unsigned char* const order4[] = {melody_pattern};\n\n';
  
  // Empty instruments for now
  c += 'static const hUGEDutyInstr_t duty_instruments[] = {{0,0,0,0,0}};\n';
  c += 'static const hUGEWaveInstr_t wave_instruments[] = {{0,0,NULL,0}};\n';
  c += 'static const hUGENoiseInstr_t noise_instruments[] = {{0,NULL,0,0,0}};\n';
  c += 'static const unsigned char waves[] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};\n\n';
  
  c += 'const hUGESong_t kidlisp_song = {150, &order_cnt, order1, order2, order3, order4, duty_instruments, wave_instruments, noise_instruments, NULL, waves};\n';
  
  return c;
}

function compileKidLisp(source) {
  const lines = source.split('\n').map(l => l.trim()).filter(l => l && !l.startsWith(';'));
  
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
      graphicsCommands.push(`    fill_rect(0, 0, 160, 144, ${color === 'BLACK' ? '0' : '3'});`);
    }
    else if (cmd === 'ink') {
      currentColor = parts[1]?.toUpperCase() || 'WHITE';
      graphicsCommands.push(`    // Set ink color to ${currentColor}`);
    }
    else if (cmd === 'line') {
      const [x1, y1, x2, y2] = parts.slice(1).map(Number);
      graphicsCommands.push(`    draw_line(${x1}, ${y1}, ${x2}, ${y2});`);
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
  
  if (melodyNotes) {
    c += '#include "hUGEDriver.h"\n';
  }
  
  c += '\n';
  
  // Add melody data if present
  if (melodyNotes) {
    c += generateMelodyC(melodyNotes);
    c += '\n';
  }
  
  // Simple line drawing function (Bresenham's algorithm)
  c += '// Simple line drawing using Bresenham\'s algorithm\n';
  c += 'void draw_line(uint8_t x0, uint8_t y0, uint8_t x1, uint8_t y1) {\n';
  c += '    int16_t dx = x1 > x0 ? x1 - x0 : x0 - x1;\n';
  c += '    int16_t dy = y1 > y0 ? y1 - y0 : y0 - y1;\n';
  c += '    int16_t sx = x0 < x1 ? 1 : -1;\n';
  c += '    int16_t sy = y0 < y1 ? 1 : -1;\n';
  c += '    int16_t err = dx - dy;\n';
  c += '    while (1) {\n';
  c += '        plot_point(x0, y0);\n';
  c += '        if (x0 == x1 && y0 == y1) break;\n';
  c += '        int16_t e2 = 2 * err;\n';
  c += '        if (e2 > -dy) { err -= dy; x0 += sx; }\n';
  c += '        if (e2 < dx) { err += dx; y0 += sy; }\n';
  c += '    }\n';
  c += '}\n\n';
  
  // Main function
  c += 'void main(void) {\n';
  
  if (melodyNotes) {
    c += '    // Initialize sound\n';
    c += '    NR52_REG = 0x80;\n';
    c += '    NR50_REG = 0xFF;\n';
    c += '    NR51_REG = 0xFF;\n\n';
    c += '    __critical {\n';
    c += '        hUGE_init(&kidlisp_song);\n';
    c += '        add_VBL(hUGE_dosound);\n';
    c += '    }\n';
    c += '    set_interrupts(VBL_IFLAG);\n\n';
  }
  
  c += '    // Graphics setup\n';
  c += '    DISPLAY_ON;\n';
  c += '    mode(get_mode() | M_NO_SCROLL | M_NO_INTERP);\n\n';
  
  c += '    // Execute KidLisp commands\n';
  c += graphicsCommands.join('\n');
  c += '\n\n';
  
  c += '    // Main loop\n';
  c += '    while(1) {\n';
  c += '        vsync();\n';
  c += '    }\n';
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

const source = fs.readFileSync(inputFile, 'utf-8');
const compiled = compileKidLisp(source);

fs.writeFileSync(outputFile, compiled);
console.log(`✅ Compiled ${inputFile} → ${outputFile}`);
