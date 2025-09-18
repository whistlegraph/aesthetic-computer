// Bake.mjs - Single frame renderer for Aesthetic Computer
// Optimized for high-quality static image generation

/* #region 🏁 TODO
  + Future Features
  - [] High resolution output support (2x, 4x, 8x scaling)
  - [] Multiple output formats (PNG, SVG, WebP)
  - [] Custom canvas sizes beyond 128x128
  - [] Batch processing multiple pieces
  - [] Color palette optimization
  - [] Anti-aliasing options
  - [] Custom DPI settings
  - [] Metadata embedding in output files
endregion */

import { HeadlessAC } from './headless.mjs';
import { timestamp } from "../../../system/public/aesthetic.computer/lib/num.mjs";
import chalk from 'chalk';
import boxen from 'boxen';
import figlet from 'figlet';

class Baker extends HeadlessAC {
  constructor(width = 128, height = 128) {
    super(width, height);
  }

  async bake(piecePath) {
    const ts = timestamp();
    console.log(`🚀 Baking piece: ${piecePath} [${ts}]`);
    console.log(`📐 Canvas: ${this.width}x${this.height}, Single frame render`);
    
    try {
      // Load the piece
      const paintFn = await this.loadPiece(piecePath);
      
      // Create API
      const api = this.createAPI();
      
      // Render single frame
      console.log('🎨 Baking single frame...');
      
      const startTime = Date.now();
      await paintFn(api);
      const elapsed = Date.now() - startTime;
      
      const stats = this.getStats();
      console.log(`✅ Frame baked in ${elapsed}ms • ${stats.apiCalls} calls • ${stats.uniqueAPIs} APIs`);
      
      return {
        success: true,
        timestamp: ts,
        frames: 1,
        apiCalls: stats.apiCalls,
        uniqueAPIs: stats.uniqueAPIs,
        apis: stats.apis,
        renderTime: elapsed
      };
      
    } catch (error) {
      console.error('💥 Error baking piece:', error);
      return {
        success: false,
        error: error.message,
        timestamp: ts,
        frames: 0,
        apiCalls: 0,
        uniqueAPIs: 0,
        apis: []
      };
    }
  }
}

// CLI interface
if (import.meta.url === `file://${process.argv[1]}`) {
  console.clear();
  console.log(chalk.cyan(figlet.textSync('BAKE', { font: 'Small' })));
  console.log(chalk.gray('Single Frame Renderer • High Quality Output\n'));
  
  const args = process.argv.slice(2);
  let piecePath = args[0];
  
  if (!piecePath) {
    console.error(chalk.red('Usage: node bake.mjs <piece-path>'));
    console.log(chalk.gray('Example: node bake.mjs test-simple.mjs'));
    console.log(chalk.gray('Or from bakes folder: node bake.mjs bakes/test-simple.mjs'));
    process.exit(1);
  }

  // Auto-prefix with bakes folder if just filename given
  if (!piecePath.includes('/') && !piecePath.startsWith('../')) {
    const bakesPath = `../test-pieces/bakes/${piecePath}`;
    // Check if file exists in bakes folder
    try {
      await import(bakesPath);
      piecePath = bakesPath;
    } catch {
      // Keep original path if not found in bakes folder
    }
  }
  
  console.log(chalk.blue(`🎬 Target piece: ${piecePath}`));
  console.log(chalk.blue(`🖼️  Single frame bake\n`));
  
  try {
    // Create baker with 128x128 canvas
    const baker = new Baker(128, 128);
    
    // Initialize AC system
    await baker.initializeAC();
    
    // Bake the piece
    const result = await baker.bake(piecePath);
    
    // Save output
    const saveResult = baker.savePNG('/workspaces/aesthetic-computer/reference/tools/output/bake');
    
    // Final summary
    const summary = `${chalk.green('✅ Baking Complete!')}\n\n` +
                   `${chalk.cyan('Success:')} ${result.success}\n` +
                   `${chalk.cyan('Frames:')} ${result.frames}\n` +
                   `${chalk.cyan('API Calls:')} ${result.apiCalls}\n` +
                   `${chalk.cyan('Unique APIs:')} ${result.uniqueAPIs}\n` +
                   `${chalk.cyan('APIs Used:')} ${result.apis.join(', ') || 'none'}\n` +
                   `${chalk.cyan('Output:')} ${saveResult.filename}`;
    
    const boxedSummary = boxen(summary, { 
      padding: 1, 
      borderColor: 'green',
      borderStyle: 'round'
    });
    
    console.log('\n' + boxedSummary);
    
    if (!result.success) {
      console.log(`\n${chalk.red('💥 Error:')} ${result.error}`);
      process.exit(1);
    }
    
    // Force exit to prevent hanging
    setTimeout(() => {
      console.log(chalk.gray('\n🔄 Force exit to prevent hanging...'));
      process.exit(0);
    }, 100);
    
  } catch (error) {
    console.error(chalk.red('\n💥 Fatal error:'), error);
    process.exit(1);
  }
}