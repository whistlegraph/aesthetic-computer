#!/usr/bin/env node
// Enhanced KidLisp Source Tree Analyzer with Syntax Highlighting
// Usage: ./source-tree-enhanced.mjs $cow --source
// Shows the embedded layer structure and syntax-highlighted source code of KidLisp pieces

import https from 'https';
import { URL } from 'url';

// ANSI terminal colors for syntax highlighting (expanded set)
const terminalColors = {
  reset: '\x1b[0m',
  bright: '\x1b[1m',
  dim: '\x1b[2m',
  red: '\x1b[31m',
  green: '\x1b[32m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m',
  magenta: '\x1b[35m',
  cyan: '\x1b[36m',
  white: '\x1b[37m',
  black: '\x1b[30m',
  orange: '\x1b[38;5;208m',
  lime: '\x1b[38;5;10m',
  purple: '\x1b[38;5;93m',
  olive: '\x1b[38;5;58m',
  gray: '\x1b[90m',
  pink: '\x1b[38;5;205m',
  brown: '\x1b[38;5;94m',
  silver: '\x1b[38;5;7m',
  gold: '\x1b[38;5;220m',
  coral: '\x1b[38;5;203m',
  azure: '\x1b[38;5;117m',
  violet: '\x1b[38;5;99m',
  khaki: '\x1b[38;5;143m',
  salmon: '\x1b[38;5;209m',
  plum: '\x1b[38;5;219m',
  tan: '\x1b[38;5;180m',
  navy: '\x1b[38;5;17m',
  teal: '\x1b[38;5;37m',
  maroon: '\x1b[38;5;52m',
  indigo: '\x1b[38;5;54m',
  crimson: '\x1b[38;5;196m',
  forestgreen: '\x1b[38;5;22m',
  mediumseagreen: '\x1b[38;5;72m',
  royalblue: '\x1b[38;5;62m',
  darkorchid: '\x1b[38;5;128m',
  darkslategray: '\x1b[38;5;239m'
};

// Load KidLisp module for syntax highlighting
let KidLisp = null;
try {
  // Try to load the KidLisp module from the system
  const kidlispPath = "/workspaces/aesthetic-computer/system/public/aesthetic.computer/lib/kidlisp.mjs";
  const kidlispModule = await import(kidlispPath);
  KidLisp = kidlispModule.KidLisp;
  console.log(`${terminalColors.green}✅ Loaded KidLisp syntax highlighter${terminalColors.reset}`);
} catch (error) {
  console.log(`${terminalColors.yellow}⚠️  Could not load KidLisp syntax highlighter: ${error.message}${terminalColors.reset}`);
}

// Function to convert KidLisp color codes to ANSI terminal colors
function convertKidLispColorsToTerminal(coloredText) {
  if (!coloredText) {
    return "";
  }
  
  let output = "";
  let currentPos = 0;
  
  // Parse color escape sequences like \red\, \green\, etc.
  const colorRegex = /\\([^\\]+)\\/g;
  let match;
  
  while ((match = colorRegex.exec(coloredText)) !== null) {
    // Add any text before this color code
    output += coloredText.substring(currentPos, match.index);
    
    // Add the ANSI color code
    const colorName = match[1];
    if (terminalColors[colorName]) {
      output += terminalColors[colorName];
    } else if (colorName.includes(',')) {
      // Handle RGB colors like "255,255,255,0" (transparent)
      const [r, g, b, a] = colorName.split(',').map(Number);
      if (a === 0) {
        // Transparent - use dark gray
        output += terminalColors.gray;
      } else {
        // Other RGB - try to map to closest ANSI color
        if (r > 200 && g < 100 && b < 100) output += terminalColors.red;
        else if (r < 100 && g > 200 && b < 100) output += terminalColors.green;
        else if (r < 100 && g < 100 && b > 200) output += terminalColors.blue;
        else if (r > 200 && g > 200 && b < 100) output += terminalColors.yellow;
        else if (r > 200 && g < 100 && b > 200) output += terminalColors.magenta;
        else if (r < 100 && g > 200 && b > 200) output += terminalColors.cyan;
        else output += terminalColors.white;
      }
    } else {
      // Unknown color, use default
      output += terminalColors.white;
    }
    
    currentPos = match.index + match[0].length;
  }
  
  // Add any remaining text
  output += coloredText.substring(currentPos);
  
  // Reset color at the end
  output += terminalColors.reset;
  
  return output;
}

// Function to apply syntax highlighting to KidLisp source code
function applySyntaxHighlighting(source) {
  if (!KidLisp || !source) {
    return source; // Return plain source if no highlighter available
  }
  
  try {
    const lisp = new KidLisp();
    lisp.initializeSyntaxHighlighting(source);
    const coloredString = lisp.buildColoredKidlispString();
    
    if (coloredString) {
      return convertKidLispColorsToTerminal(coloredString);
    }
  } catch (error) {
    console.error(`${terminalColors.yellow}Warning: Syntax highlighting failed: ${error.message}${terminalColors.reset}`);
  }
  
  return source; // Fallback to plain source
}

function printUsage() {
    console.log("Usage: source-tree-enhanced.mjs <piece-name> [--source]");
    console.log("Example: source-tree-enhanced.mjs $cow --source");
    console.log("         source-tree-enhanced.mjs cow --source");
    console.log("");
    console.log("Options:");
    console.log("  --source    Show complete syntax-highlighted source code for each piece");
    console.log("");
    console.log("Analyzes KidLisp pieces and shows their embedded layer tree structure.");
}

async function fetchSource(pieceName) {
    // Remove $ prefix if present
    const cleanName = pieceName.replace(/^\$/, '');
    
    const url = `https://localhost:8888/.netlify/functions/store-kidlisp?code=${cleanName}`;
    
    return new Promise((resolve, reject) => {
        // Allow self-signed certificates for localhost
        const options = {
            rejectUnauthorized: false
        };
        
        https.get(url, options, (res) => {
            let data = '';
            
            res.on('data', (chunk) => {
                data += chunk;
            });
            
            res.on('end', () => {
                try {
                    const response = JSON.parse(data);
                    
                    if (response.error) {
                        reject(new Error(`Piece '$${cleanName}' not found`));
                        return;
                    }
                    
                    if (!response.source) {
                        reject(new Error("Could not parse source code from response"));
                        return;
                    }
                    
                    resolve(response.source);
                } catch (error) {
                    reject(new Error("Could not parse JSON response"));
                }
            });
        }).on('error', (error) => {
            reject(new Error(`Could not connect to local API (https://localhost:8888)\n   Make sure the dev server is running with: npm run dev`));
        });
    });
}

function extractEmbeddedPieces(source) {
    // Find all $piece references like ($39i ...) or ($r2f ...)
    const regex = /\(\$[a-zA-Z0-9_-]+/g;
    const matches = source.match(regex) || [];
    
    // Extract piece names and remove duplicates
    const pieces = [...new Set(matches.map(match => match.replace('($', '')))];
    return pieces;
}

async function printTreeNode(pieceName, depth = 0, prefix = "", showSource = false) {
    // Create indentation
    const indent = "  ".repeat(depth);
    
    try {
        // Fetch source code
        const source = await fetchSource(pieceName);
        
        // Clean up source for display (replace \\n with actual newlines and basic unescaping)
        const cleanSource = source.replace(/\\n/g, '\n').replace(/\\"/g, '"');
        
        // Extract embedded pieces
        const embeddedPieces = extractEmbeddedPieces(source);
        
        // Print current piece
        if (embeddedPieces.length > 0) {
            console.log(`${indent}${prefix}${terminalColors.bright}${terminalColors.blue}📁 $${pieceName}${terminalColors.reset}`);
        } else {
            console.log(`${indent}${prefix}${terminalColors.cyan}📄 $${pieceName}${terminalColors.reset}`);
        }
        
        // Print source code if requested
        if (showSource) {
            const sourceLines = cleanSource.split('\n');
            
            // Apply syntax highlighting to the entire source
            const highlightedSource = applySyntaxHighlighting(cleanSource);
            const highlightedLines = highlightedSource.split('\n');
            
            console.log(`${indent}   ${terminalColors.dim}┌─ Source Code ─────────────────────────────────────────┐${terminalColors.reset}`);
            
            highlightedLines.forEach((line, index) => {
                const lineNumber = (index + 1).toString().padStart(3, ' ');
                // Reset colors before line number, then apply highlighting to the line content
                console.log(`${indent}   ${terminalColors.dim}${lineNumber}:${terminalColors.reset} ${line}${terminalColors.reset}`);
            });
            
            console.log(`${indent}   ${terminalColors.dim}└───────────────────────────────────────────────────────┘${terminalColors.reset}`);
            console.log(); // Empty line after source
        } else {
            // Show just first few lines as preview
            const sourceLines = cleanSource.split('\n').slice(0, 3);
            for (const line of sourceLines) {
                if (line.trim()) {
                    let truncatedLine = line.substring(0, 60);
                    if (line.length > 60) {
                        truncatedLine += "...";
                    }
                    console.log(`${indent}   ${terminalColors.dim}│${terminalColors.reset} ${truncatedLine}`);
                }
            }
            
            // If source is longer than 3 lines, show indicator
            const totalLines = cleanSource.split('\n').length;
            if (totalLines > 3) {
                console.log(`${indent}   ${terminalColors.dim}│ ... (${totalLines} lines total)${terminalColors.reset}`);
            }
        }
        
        // Process embedded pieces recursively (with depth limit)
        if (depth < 5) { // Prevent infinite recursion
            for (let i = 0; i < embeddedPieces.length; i++) {
                const embeddedPiece = embeddedPieces[i];
                const isLast = i === embeddedPieces.length - 1;
                const nodePrefix = isLast ? "└─ " : "├─ ";
                await printTreeNode(embeddedPiece, depth + 1, nodePrefix, showSource);
            }
        } else if (embeddedPieces.length > 0) {
            console.log(`${indent}   └─ ... (max depth reached)`);
        }
    } catch (error) {
        console.log(`${indent}${prefix}${terminalColors.red}❌ $${pieceName}${terminalColors.reset} ${terminalColors.dim}(${error.message})${terminalColors.reset}`);
    }
}

function analyzePerformanceFeatures(source) {
    console.log("");
    console.log(`${terminalColors.bright}🔍 Performance Analysis:${terminalColors.reset}`);
    
    // Check for expensive operations
    const expensiveOps = [];
    if (source.includes('blur')) expensiveOps.push('blur');
    if (source.includes('zoom')) expensiveOps.push('zoom');
    if (source.includes('contrast')) expensiveOps.push('contrast');
    if (source.includes('spin')) expensiveOps.push('spin');
    if (source.includes('flood')) expensiveOps.push('flood');
    
    if (expensiveOps.length > 0) {
        console.log(`   ${terminalColors.yellow}⚠️  Expensive operations detected: ${expensiveOps.join(", ")}${terminalColors.reset}`);
    } else {
        console.log(`   ${terminalColors.green}✅ No expensive operations detected${terminalColors.reset}`);
    }
    
    // Check for timing expressions
    if (source.includes('s(')) {
        console.log(`   ${terminalColors.blue}⏱️  Contains timing expressions (animated)${terminalColors.reset}`);
    }
    
    // Check for randomness
    if (source.includes('?')) {
        console.log(`   ${terminalColors.magenta}🎲 Contains randomness (?)${terminalColors.reset}`);
    }
    
    // Count embedded layers
    const embeddedCount = extractEmbeddedPieces(source).length;
    if (embeddedCount > 0) {
        console.log(`   ${terminalColors.cyan}📁 Embeds ${embeddedCount} layer(s)${terminalColors.reset}`);
    }
}

// Main script
async function main() {
    const args = process.argv.slice(2);
    
    if (args.length === 0) {
        printUsage();
        process.exit(1);
    }
    
    const pieceName = args[0];
    const showSource = args.includes('--source');
    
    // Remove $ prefix if present for display
    const displayName = pieceName.startsWith('$') ? pieceName : `$${pieceName}`;
    
    console.log(`${terminalColors.bright}🌳 KidLisp Source Tree for ${displayName}${terminalColors.reset}`);
    if (showSource && KidLisp) {
        console.log(`${terminalColors.green}🎨 Syntax highlighting enabled${terminalColors.reset}`);
    }
    console.log("═══════════════════════════════════════════");
    
    try {
        // Build the tree
        await printTreeNode(pieceName, 0, "", showSource);
        
        // Get the main source for analysis
        const mainSource = await fetchSource(pieceName);
        analyzePerformanceFeatures(mainSource);
        
        console.log("");
        console.log(`${terminalColors.bright}💡 Use this analysis to understand performance bottlenecks!${terminalColors.reset}`);
        console.log(`   ${terminalColors.dim}Run with different pieces: source-tree-enhanced.mjs $39i --source${terminalColors.reset}`);
    } catch (error) {
        console.error(`${terminalColors.red}❌ Error: ${error.message}${terminalColors.reset}`);
        process.exit(1);
    }
}

main().catch(console.error);
