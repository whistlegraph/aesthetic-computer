#!/usr/bin/env node
// KidLisp Source Tree Analyzer
// Usage: ./source-tree.mjs $cow --source
// Shows the embedded layer structure and source code of KidLisp pieces

import https from 'https';

// ANSI terminal colors
import https from 'https';

const COLORS = {
  reset: '\x1b[0m',
  bright: '\x1b[1m',
  dim: '\x1b[2m',
  
  // Text colors
  red: '\x1b[31m',
  green: '\x1b[32m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m',
  magenta: '\x1b[35m',
  cyan: '\x1b[36m',
  white: '\x1b[37m',
  gray: '\x1b[90m',
  
  // Background colors
  bgRed: '\x1b[41m',
  bgGreen: '\x1b[42m',
  bgYellow: '\x1b[43m',
  bgBlue: '\x1b[44m',
  bgMagenta: '\x1b[45m',
  bgCyan: '\x1b[46m',
};

// Track which server is working to use it for all subsequent calls
let workingServer = null;
let workingUrl = null;

function printUsage() {
    console.log("Usage: source-tree.mjs <piece-name> [--source]");
    console.log("Example: source-tree.mjs $cow --source");
    console.log("         source-tree.mjs cow --source");
    console.log("");
    console.log("Options:");
    console.log("  --source    Show complete source code for each piece");
    console.log("");
    console.log("Analyzes KidLisp pieces and shows their embedded layer tree structure.");
}

// Function to syntax highlight KidLisp code
function syntaxHighlight(code) {
  if (!code) return '';
  
  return code
    // Comments
    .replace(/;[^\n]*/g, match => `${COLORS.gray}${match}${COLORS.reset}`)
    // String literals
    .replace(/"[^"]*"/g, match => `${COLORS.green}${match}${COLORS.reset}`)
    // Numbers
    .replace(/\b\d+(\.\d+)?\b/g, match => `${COLORS.cyan}${match}${COLORS.reset}`)
    // Keywords and special forms
    .replace(/\b(define|if|cond|let|lambda|quote|quasiquote|unquote|and|or|not|car|cdr|cons|list|map|filter|fold|apply)\b/g, 
             match => `${COLORS.magenta}${match}${COLORS.reset}`)
    // Function calls (first item in parentheses)
    .replace(/\(([a-zA-Z][a-zA-Z0-9-]*)/g, match => {
      const parts = match.split('(');
      const funcName = parts[1];
      return `(${COLORS.yellow}${funcName}${COLORS.reset}`;
    })
    // Special variables starting with $
    .replace(/\$[a-zA-Z0-9-]+/g, match => `${COLORS.blue}${match}${COLORS.reset}`)
    // Embed calls
    .replace(/#embed\s+([a-zA-Z0-9-]+)/g, match => {
      const parts = match.split(' ');
      return `${COLORS.red}#embed${COLORS.reset} ${COLORS.blue}${parts[1]}${COLORS.reset}`;
    });
}

async function fetchSourceFromEndpoint(url, cleanName, isProduction = false) {
    return new Promise((resolve, reject) => {
        const timeout = setTimeout(() => {
            reject(new Error(`Request timeout for ${cleanName} (${isProduction ? 'production' : 'local'})`));
        }, 3000); // 3 second timeout for faster fallback
        
        const options = isProduction ? {} : { rejectUnauthorized: false };
        
        https.get(url, options, (res) => {
            clearTimeout(timeout);
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
            clearTimeout(timeout);
            reject(new Error(`Connection failed: ${error.message}`));
        });
    });
}

async function fetchSource(pieceName) {
    const cleanName = pieceName.replace(/^\$/, '');
    
    // If we already know which server works, use it directly
    if (workingServer && workingUrl) {
        const url = workingUrl.replace(/code=[^&]*/, `code=${cleanName}`);
        try {
            return await fetchSourceFromEndpoint(url, cleanName, workingServer);
        } catch (error) {
            console.log(`${COLORS.yellow}⚠️ ${workingServer} server failed: ${error.message}${COLORS.reset}`);
            // Reset and try both servers again
            workingServer = null;
            workingUrl = null;
        }
    }
    
    // Try local development server first
    const localUrl = `https://localhost:8888/.netlify/functions/store-kidlisp?code=${cleanName}`;
    
    try {
        if (!workingServer) console.log(`${COLORS.dim}Trying local dev server...${COLORS.reset}`);
        const source = await fetchSourceFromEndpoint(localUrl, cleanName, 'local');
        if (!workingServer) {
            console.log(`${COLORS.green}✅ Found on local dev server - using for all requests${COLORS.reset}`);
            workingServer = 'local';
            workingUrl = localUrl;
        }
        return source;
    } catch (localError) {
        if (!workingServer) console.log(`${COLORS.yellow}⚠️ Local dev server failed: ${localError.message}${COLORS.reset}`);
        
        // Fallback to production
        const productionUrl = `https://aesthetic.computer/api/store-kidlisp?code=${cleanName}`;
        
        try {
            if (!workingServer) console.log(`${COLORS.dim}Trying production server...${COLORS.reset}`);
            const source = await fetchSourceFromEndpoint(productionUrl, cleanName, 'production');
            if (!workingServer) {
                console.log(`${COLORS.green}✅ Found on production server - using for all requests${COLORS.reset}`);
                workingServer = 'production';
                workingUrl = productionUrl;
            }
            return source;
        } catch (prodError) {
            throw new Error(`Both servers failed: Local: ${localError.message}, Production: ${prodError.message}`);
        }
    }
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
            console.log(`${indent}${prefix}${colors.bright}${colors.blue}📁 $${pieceName}${colors.reset}`);
        } else {
            console.log(`${indent}${prefix}${colors.cyan}📄 $${pieceName}${colors.reset}`);
        }
        
        // Print source code if requested
        if (showSource) {
            const sourceLines = cleanSource.split('\n');
            
            console.log(`${indent}   ${colors.dim}┌─ Source Code ─────────────────────────────────────────┐${colors.reset}`);
            
            sourceLines.forEach((line, index) => {
                const lineNumber = (index + 1).toString().padStart(3, ' ');
                console.log(`${indent}   ${colors.dim}${lineNumber}:${colors.reset} ${line}`);
            });
            
            console.log(`${indent}   ${colors.dim}└───────────────────────────────────────────────────────┘${colors.reset}`);
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
                    console.log(`${indent}   ${colors.dim}│${colors.reset} ${truncatedLine}`);
                }
            }
            
            // If source is longer than 3 lines, show indicator
            const totalLines = cleanSource.split('\n').length;
            if (totalLines > 3) {
                console.log(`${indent}   ${colors.dim}│ ... (${totalLines} lines total)${colors.reset}`);
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
        console.log(`${indent}${prefix}${colors.red}❌ $${pieceName}${colors.reset} ${colors.dim}(${error.message})${colors.reset}`);
    }
}

function analyzePerformanceFeatures(source) {
    console.log("");
    console.log(`${colors.bright}🔍 Performance Analysis:${colors.reset}`);
    
    // Check for expensive operations
    const expensiveOps = [];
    if (source.includes('blur')) expensiveOps.push('blur');
    if (source.includes('zoom')) expensiveOps.push('zoom');
    if (source.includes('contrast')) expensiveOps.push('contrast');
    if (source.includes('spin')) expensiveOps.push('spin');
    if (source.includes('flood')) expensiveOps.push('flood');
    
    if (expensiveOps.length > 0) {
        console.log(`   ${colors.yellow}⚠️  Expensive operations detected: ${expensiveOps.join(", ")}${colors.reset}`);
    } else {
        console.log(`   ${colors.green}✅ No expensive operations detected${colors.reset}`);
    }
    
    // Check for timing expressions
    if (source.includes('s(')) {
        console.log(`   ${colors.blue}⏱️  Contains timing expressions (animated)${colors.reset}`);
    }
    
    // Check for randomness
    if (source.includes('?')) {
        console.log(`   ${colors.magenta}🎲 Contains randomness (?)${colors.reset}`);
    }
    
    // Count embedded layers
    const embeddedCount = extractEmbeddedPieces(source).length;
    if (embeddedCount > 0) {
        console.log(`   ${colors.cyan}📁 Embeds ${embeddedCount} layer(s)${colors.reset}`);
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
    
    console.log(`${colors.bright}🌳 KidLisp Source Tree for ${displayName}${colors.reset}`);
    console.log("═══════════════════════════════════════════");
    
    try {
        // Build the tree
        await printTreeNode(pieceName, 0, "", showSource);
        
        // Get the main source for analysis
        const mainSource = await fetchSource(pieceName);
        analyzePerformanceFeatures(mainSource);
        
        console.log("");
        console.log(`${colors.bright}💡 Use this analysis to understand performance bottlenecks!${colors.reset}`);
        console.log(`   ${colors.dim}Run with different pieces: source-tree.mjs $39i --source${colors.reset}`);
    } catch (error) {
        console.error(`${colors.red}❌ Error: ${error.message}${colors.reset}`);
        process.exit(1);
    }
}

main().catch(console.error);
