#!/usr/bin/env node
// KidLisp Source Tree Analyzer
// Usage: ./source-tree.mjs $cow
// Shows the embedded layer structure and source code of KidLisp pieces

import https from 'https';
import { URL } from 'url';

function printUsage() {
    console.log("Usage: source-tree.mjs <piece-name>");
    console.log("Example: source-tree.mjs $cow");
    console.log("         source-tree.mjs cow");
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

async function printTreeNode(pieceName, depth = 0, prefix = "") {
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
            console.log(`${indent}${prefix}📁 $${pieceName}`);
        } else {
            console.log(`${indent}${prefix}📄 $${pieceName}`);
        }
        
        // Print source code (first few lines)
        const sourceLines = cleanSource.split('\n').slice(0, 3);
        for (const line of sourceLines) {
            if (line.trim()) {
                let truncatedLine = line.substring(0, 60);
                if (line.length > 60) {
                    truncatedLine += "...";
                }
                console.log(`${indent}   │ ${truncatedLine}`);
            }
        }
        
        // If source is longer than 3 lines, show indicator
        const totalLines = cleanSource.split('\n').length;
        if (totalLines > 3) {
            console.log(`${indent}   │ ... (${totalLines} lines total)`);
        }
        
        // Process embedded pieces recursively (with depth limit)
        if (depth < 5) { // Prevent infinite recursion
            for (let i = 0; i < embeddedPieces.length; i++) {
                const embeddedPiece = embeddedPieces[i];
                const isLast = i === embeddedPieces.length - 1;
                const nodePrefix = isLast ? "└─ " : "├─ ";
                await printTreeNode(embeddedPiece, depth + 1, nodePrefix);
            }
        } else if (embeddedPieces.length > 0) {
            console.log(`${indent}   └─ ... (max depth reached)`);
        }
    } catch (error) {
        console.log(`${indent}${prefix}❌ $${pieceName} (${error.message})`);
    }
}

function analyzePerformanceFeatures(source) {
    console.log("");
    console.log("🔍 Performance Analysis:");
    
    // Check for expensive operations
    const expensiveOps = [];
    if (source.includes('blur')) expensiveOps.push('blur');
    if (source.includes('zoom')) expensiveOps.push('zoom');
    if (source.includes('contrast')) expensiveOps.push('contrast');
    if (source.includes('spin')) expensiveOps.push('spin');
    if (source.includes('flood')) expensiveOps.push('flood');
    
    if (expensiveOps.length > 0) {
        console.log(`   ⚠️  Expensive operations detected: ${expensiveOps.join(", ")}`);
    } else {
        console.log("   ✅ No expensive operations detected");
    }
    
    // Check for timing expressions
    if (source.includes('s(')) {
        console.log("   ⏱️  Contains timing expressions (animated)");
    }
    
    // Check for randomness
    if (source.includes('?')) {
        console.log("   🎲 Contains randomness (?)");
    }
    
    // Count embedded layers
    const embeddedCount = extractEmbeddedPieces(source).length;
    if (embeddedCount > 0) {
        console.log(`   📁 Embeds ${embeddedCount} layer(s)`);
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
    
    // Remove $ prefix if present for display
    const displayName = pieceName.startsWith('$') ? pieceName : `$${pieceName}`;
    
    console.log(`🌳 KidLisp Source Tree for ${displayName}`);
    console.log("═══════════════════════════════════════════");
    
    try {
        // Build the tree
        await printTreeNode(pieceName, 0, "");
        
        // Get the main source for analysis
        const mainSource = await fetchSource(pieceName);
        analyzePerformanceFeatures(mainSource);
        
        console.log("");
        console.log("💡 Use this analysis to understand performance bottlenecks!");
        console.log("   Run with different pieces: source-tree.mjs $39i");
    } catch (error) {
        console.error(`❌ Error: ${error.message}`);
        process.exit(1);
    }
}

main().catch(console.error);
