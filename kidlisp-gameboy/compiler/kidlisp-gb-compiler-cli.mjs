#!/usr/bin/env node
// kidlisp-gb-compiler-cli.mjs
// Command line KidLisp to GameBoy ROM compiler

import { writeFileSync, unlinkSync } from 'fs';
import { join, dirname } from 'path';
import { fileURLToPath } from 'url';
import { execSync } from 'child_process';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

// GameBoy screen dimensions
const GB_WIDTH = 160;
const GB_HEIGHT = 144;

// Boot ROM template
const BOOT_ROM_TEMPLATE = `
SECTION "Header", ROM0[$100]
  jp Main

SECTION "Nintendo Logo", ROM0[$104]
  db $CE,$ED,$66,$66,$CC,$0D,$00,$0B,$03,$73,$00,$83,$00,$0C,$00,$0D
  db $00,$08,$11,$1F,$88,$89,$00,$0E,$DC,$CC,$6E,$E6,$DD,$DD,$D9,$99
  db $BB,$BB,$67,$63,$6E,$0E,$EC,$CC,$DD,$DC,$99,$9F,$BB,$B9,$33,$3E

SECTION "Cartridge Header", ROM0[$134]
  db "KIDLISP-ROM"  ; Title start
  db 0,0,0,0,0       ; Title padding (15 bytes total)
  db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00 ; Manufacturer & cartridge
  db $00    ; CGB flag
  dw $0000  ; New licensee code
  db $00    ; SGB flag
  db $00    ; Cartridge type
  db $00    ; ROM size
  db $00    ; RAM size
  db $01    ; Destination code
  db $33    ; Old licensee code
  db $00    ; ROM version
  db $00    ; Header checksum (calculated by assembler)
  dw $0000  ; Global checksum (calculated by assembler)

SECTION "Main", ROM0[$150]
Main:
  ; Initialize LCD
  ld a, $91
  ldh [$FF40], a
  
  ; Call KidLisp compiled code
  call kidlisp_main
  
  ; Main loop
.loop:
    halt
    jr .loop

; Simple pixel plotting function
set_pixel:
  ; Input: B = X, C = Y, A = color (0-3)
  ; This is a simplified version for proof of concept
  push af
  push bc
  
  ; For now, just return (actual GameBoy graphics are more complex)
  ; In a full implementation, this would write to VRAM
  
  pop bc
  pop af
  ret

; KidLisp compiled code goes here
kidlisp_main:
{KIDLISP_CODE}
  ret
`;

class KidLispGameBoyCompiler {
  parseKidLisp(code) {
    console.log("🔍 Parsing KidLisp code:", code);
    
    const expressions = [];
    const tokens = code.match(/\([^)]+\)/g) || [];
    
    tokens.forEach(token => {
      const content = token.slice(1, -1).trim();
      const parts = content.split(/\s+/);
      
      if (parts[0] === 'point' && parts.length >= 3) {
        const x = parseInt(parts[1]);
        const y = parseInt(parts[2]);
        
        if (!isNaN(x) && !isNaN(y)) {
          expressions.push({
            type: 'point',
            x: x,
            y: y
          });
        }
      }
    });
    
    console.log("✅ Parsed expressions:", expressions);
    return expressions;
  }

  generateAssembly(expressions) {
    console.log("🏗️ Generating GameBoy assembly...");
    
    let assemblyCode = '';
    
    expressions.forEach((expr, index) => {
      switch (expr.type) {
        case 'point':
          const x = Math.max(0, Math.min(expr.x, GB_WIDTH - 1));
          const y = Math.max(0, Math.min(expr.y, GB_HEIGHT - 1));
          
          assemblyCode += `
    ; Plot point at (${x}, ${y})
    ld b, ${x}    ; X coordinate
    ld c, ${y}    ; Y coordinate  
    ld a, 3       ; Color (black)
    call set_pixel
`;
          break;
      }
    });
    
    if (assemblyCode === '') {
      assemblyCode = `
    ; Default: plot pixel at screen center
    ld b, 80      ; X coordinate (center)
    ld c, 72      ; Y coordinate (center)
    ld a, 3       ; Color (black)
    call set_pixel
`;
    }
    
    return assemblyCode;
  }

  assembleRom(kidlispAssembly, outputPath) {
    console.log("🔧 Assembling GameBoy ROM...");
    
    const completeAssembly = BOOT_ROM_TEMPLATE.replace('{KIDLISP_CODE}', kidlispAssembly);
    
    try {
      // Write assembly to temporary file  
      const tempAsmPath = outputPath.replace('.gb', '.asm');
      writeFileSync(tempAsmPath, completeAssembly);
      console.log(`📝 Assembly written to: ${tempAsmPath}`);
      
      // Use gbasm command line tool
      const cmd = `npx gbasm -o "${outputPath}" "${tempAsmPath}"`;
      console.log(`🔧 Running: ${cmd}`);
      
      execSync(cmd, { stdio: 'inherit' });
      
      // Clean up temporary file
      unlinkSync(tempAsmPath);
      
      console.log(`✅ ROM written to: ${outputPath}`);
      return outputPath;
    } catch (error) {
      console.error("💥 gbasm assembly failed:", error);
      throw error;
    }
  }

  compile(kidlispCode, outputPath = "kidlisp-test.gb") {
    try {
      console.log("🔨 Compiling KidLisp to GameBoy ROM...");
      console.log("Input:", kidlispCode);
      
      const commands = this.parseKidLisp(kidlispCode);
      const assembly = this.generateAssembly(commands);
      const romPath = this.assembleRom(assembly, outputPath);
      
      return {
        success: true,
        romPath: romPath,
        assembly: assembly,
        commands: commands
      };
    } catch (error) {
      console.error("❌ Compilation failed:", error);
      return {
        success: false,
        error: error.message
      };
    }
  }
}

// CLI Interface
if (import.meta.url === `file://${process.argv[1]}`) {
  const args = process.argv.slice(2);
  
  if (args.length < 1) {
    console.log("Usage: node kidlisp-gb-compiler-cli.mjs <kidlisp-code> [output.gb]");
    console.log("Example: node kidlisp-gb-compiler-cli.mjs '(point 80 72)' test.gb");
    process.exit(1);
  }
  
  const kidlispCode = args[0];
  const outputPath = args[1] || "kidlisp-test.gb";
  
  const compiler = new KidLispGameBoyCompiler();
  const result = compiler.compile(kidlispCode, outputPath);
  
  if (result.success) {
    console.log("🎉 Success! GameBoy ROM created:");
    console.log(`   File: ${result.romPath}`);
    console.log(`   Commands: ${result.commands.length}`);
    console.log(`   Assembly lines: ${result.assembly.split('\\n').length}`);
  } else {
    console.error("💥 Failed:", result.error);
    process.exit(1);
  }
}

export { KidLispGameBoyCompiler };
