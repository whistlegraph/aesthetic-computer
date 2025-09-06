#!/usr/bin/env node
// Simple KidLisp Source Fetcher with Embed Support
// Usage: ./get-source.mjs $cow
// Fetches and displays the raw source code of a KidLisp piece and its embeds

import https from 'https';

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

async function fetchAndDisplayEmbeds(pieceName, depth = 0) {
    const indent = "  ".repeat(depth);
    
    try {
        const source = await fetchSource(pieceName);
        const cleanSource = source.replace(/\\n/g, '\n').replace(/\\"/g, '"');
        const embeddedPieces = extractEmbeddedPieces(source);
        
        console.log(`${indent}📄 $${pieceName}:`);
        console.log(`${indent}${"─".repeat(30)}`);
        console.log(cleanSource.split('\n').map(line => `${indent}${line}`).join('\n'));
        console.log(`${indent}${"─".repeat(30)}`);
        
        // Recursively fetch embeds (with depth limit)
        if (depth < 3 && embeddedPieces.length > 0) {
            console.log(`${indent}🔗 Embeds: ${embeddedPieces.map(p => '$' + p).join(', ')}`);
            console.log("");
            
            for (const embeddedPiece of embeddedPieces) {
                await fetchAndDisplayEmbeds(embeddedPiece, depth + 1);
            }
        }
        
    } catch (error) {
        console.log(`${indent}❌ $${pieceName}: ${error.message}`);
    }
}

// Main script
async function main() {
    const args = process.argv.slice(2);
    
    if (args.length === 0) {
        console.log("Usage: get-source.mjs <piece-name>");
        console.log("Example: get-source.mjs $cow");
        console.log("         get-source.mjs cow");
        console.log("");
        console.log("Fetches source code of a KidLisp piece and all its embeds.");
        process.exit(1);
    }
    
    const pieceName = args[0];
    const displayName = pieceName.startsWith('$') ? pieceName : `$${pieceName}`;
    
    console.log(`📄 Complete source tree for ${displayName}:`);
    console.log("═".repeat(50));
    console.log("");
    
    try {
        await fetchAndDisplayEmbeds(pieceName, 0);
        console.log("");
        console.log("═".repeat(50));
        
    } catch (error) {
        console.error(`❌ Error: ${error.message}`);
        process.exit(1);
    }
}

main().catch(console.error);
