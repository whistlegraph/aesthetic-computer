// kidlisp-gb-test.mjs
// Test piece for KidLisp → GameBoy ROM compilation (Phase 1)

// Simple KidLisp parser for browser testing (without gbasm dependency)
class SimpleKidLispParser {
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
    let assemblyCode = '';
    
    expressions.forEach((expr, index) => {
      switch (expr.type) {
        case 'point':
          const x = Math.max(0, Math.min(expr.x, 159));
          const y = Math.max(0, Math.min(expr.y, 143));
          
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
    
    return assemblyCode;
  }
}

let compiler;
let compilationResult = null;
let showingSuccess = false;

export function boot() {
  console.log("🎮 Booting KidLisp GameBoy compiler test");
  compiler = new SimpleKidLispParser();
}

export function paint({ wipe, ink, write, screen, box, line }) {
  wipe("navy");
  
  // Title
  ink("white");
  write("KidLisp → GameBoy ROM Compiler", 10, 20);
  write("Phase 1: Proof of Concept", 10, 35);
  
  // Instructions
  ink("cyan");
  write("Click to compile: (point 80 72)", 10, 60);
  write("This will create a GameBoy ROM that plots", 10, 75);
  write("a pixel at screen center (80, 72)", 10, 90);
  
  // Show compilation status
  if (compilationResult) {
    if (compilationResult.success) {
      ink("lime");
      write("✅ Parsing successful!", 10, 120);
      write(`Commands found: ${compilationResult.commands.length}`, 10, 135);
      write("Assembly generated ✓", 10, 150);
      write("Click again to test another point", 10, 165);
      
      // Show assembly preview
      ink("yellow");
      write("Generated Assembly:", 10, 185);
      const lines = compilationResult.assembly.split('\n').slice(0, 3);
      lines.forEach((line, i) => {
        write(line.trim(), 15, 200 + i * 12);
      });
      
      // Show success animation
      if (showingSuccess) {
        const time = Date.now();
        const flash = Math.sin(time / 200) > 0;
        if (flash) {
          ink("yellow");
          box(5, 115, screen.width - 10, 100);
        }
      }
    } else {
      ink("red");
      write("❌ Compilation failed:", 10, 120);
      write(compilationResult.error || "Unknown error", 10, 135);
    }
  }
  
  // Visual representation of GameBoy screen
  ink("white");
  write("GameBoy Screen Preview (160x144):", 10, screen.height - 120);
  
  // Draw GameBoy screen outline (scaled down)
  const gbX = 10;
  const gbY = screen.height - 100;
  const gbW = 160;
  const gbH = 72; // Half height for display
  
  ink("darkgreen");
  box(gbX - 2, gbY - 2, gbW + 4, gbH + 4);
  ink("lightgreen");
  box(gbX, gbY, gbW, gbH);
  
  // Show pixel location
  ink("black");
  const pixelX = gbX + 80; // Center X
  const pixelY = gbY + 36; // Center Y (scaled)
  box(pixelX - 1, pixelY - 1, 3, 3); // 3x3 pixel for visibility
  
  ink("white");
  write("Pixel at (80, 72)", pixelX + 10, pixelY - 5);
}

export function act({ event, download }) {
  if (event.is("click")) {
    console.log("🔨 Starting compilation...");
    showingSuccess = false;
    
    // Test with simple point command
    const testKidLisp = "(point 80 72)";
    
    try {
      const commands = compiler.parseKidLisp(testKidLisp);
      const assembly = compiler.generateAssembly(commands);
      
      compilationResult = {
        success: true,
        commands: commands,
        assembly: assembly,
        message: "Phase 1: KidLisp parsing successful!"
      };
      
      console.log("🎉 Compilation successful!");
      console.log("Generated assembly:", assembly);
      showingSuccess = true;
      
      // Stop success animation after 3 seconds
      setTimeout(() => {
        showingSuccess = false;
      }, 3000);
      
    } catch (error) {
      compilationResult = {
        success: false,
        error: error.message
      };
    }
  }
}
