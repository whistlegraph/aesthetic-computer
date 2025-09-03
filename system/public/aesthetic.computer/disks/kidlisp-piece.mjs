// KidLisp Piece, 2025.09.02
// Proper integration of KidLisp as a first-class AC piece

/* #region 📚 README
This implements KidLisp as a proper AC piece, similar to how chat.mjs works.
It provides the `kidlisp()` function for embedding KidLisp code in regions.
#endregion */

let globalKidLispInstance = null;

// Boot function - called when the piece loads
async function boot({ kidlisp: kidlispSystem, screen }) {
  console.log("🚀 KidLisp piece booting...");
  
  // Get the global KidLisp instance from the system
  if (typeof kidlispSystem === 'function') {
    globalKidLispInstance = kidlispSystem();
  } else {
    globalKidLispInstance = kidlispSystem;
  }
  
  if (!globalKidLispInstance) {
    console.error("❌ No global KidLisp instance available");
    return;
  }
  
  console.log("✅ KidLisp piece booted successfully");
}

// Paint function - called when rendering KidLisp in embedded mode
function paint({ screen, paste, painting }, options = {}) {
  // This would be called when KidLisp is used as an embedded piece
  // For now, we'll just return since kidlisp() function handles rendering
  return null;
}

// Main kidlisp function for rendering KidLisp code in regions
function kidlisp(api, x = 0, y = 0, width, height, source, options = {}) {
  if (!globalKidLispInstance) {
    console.error("❌ KidLisp not initialized. Call boot() first.");
    return null;
  }
  
  // Default dimensions to screen size if not provided
  if (!width) width = api.screen.width;
  if (!height) height = api.screen.height;
  
  console.log(`🎯 KidLisp piece: ${width}x${height} at (${x},${y})`);
  
  try {
    // Create a painting for the KidLisp region
    const lispPainting = api.painting(width, height, (paintApi) => {
      // Set the KidLisp API to the painting context
      globalKidLispInstance.setAPI(paintApi);
      
      // Ensure proper timing and animation support
      if (!paintApi.clock) {
        paintApi.clock = { time: () => new Date() };
      }
      
      // Parse and evaluate the KidLisp source
      try {
        globalKidLispInstance.parse(source);
        if (globalKidLispInstance.ast) {
          // Detect and apply first-line color if needed
          globalKidLispInstance.detectFirstLineColor();
          if (globalKidLispInstance.firstLineColor) {
            paintApi.wipe(globalKidLispInstance.firstLineColor);
          }
          
          // Evaluate using the main KidLisp evaluation system
          const result = globalKidLispInstance.evaluate(
            globalKidLispInstance.ast, 
            paintApi, 
            globalKidLispInstance.localEnv
          );
          
          console.log(`🎯 KidLisp evaluation result:`, result);
        }
      } catch (evalError) {
        console.error("🚫 KidLisp evaluation error:", evalError);
        paintApi.wipe(60, 0, 0);
        paintApi.ink(255, 255, 255);
        if (paintApi.write) {
          paintApi.write("KidLisp Error", 5, 15);
        }
      }
      
      // Restore API to main context
      globalKidLispInstance.setAPI(api);
    });
    
    // Paste the painting to the specified location
    if (api.paste && lispPainting) {
      api.paste(lispPainting, x, y);
    }
    
    return lispPainting;
    
  } catch (error) {
    console.error("🚫 KidLisp piece error:", error);
    return null;
  }
}

export { boot, paint, kidlisp };
