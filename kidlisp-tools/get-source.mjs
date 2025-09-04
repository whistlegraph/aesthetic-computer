#!/usr/bin/env node
// Simple KidLisp Source Fetcher
// Usage: ./get-source.mjs $cow
// Just fetches and displays the raw source code of a KidLisp piece

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

// Main script
async function main() {
    const args = process.argv.slice(2);
    
    if (args.length === 0) {
        console.log("Usage: get-source.mjs <piece-name>");
        console.log("Example: get-source.mjs $cow");
        console.log("         get-source.mjs cow");
        process.exit(1);
    }
    
    const pieceName = args[0];
    const displayName = pieceName.startsWith('$') ? pieceName : `$${pieceName}`;
    
    try {
        console.log(`📄 Source code for ${displayName}:`);
        console.log("═".repeat(50));
        
        const source = await fetchSource(pieceName);
        
        // Clean up source for display (replace \\n with actual newlines and basic unescaping)
        const cleanSource = source.replace(/\\n/g, '\n').replace(/\\"/g, '"');
        
        console.log(cleanSource);
        console.log("═".repeat(50));
        
    } catch (error) {
        console.error(`❌ Error: ${error.message}`);
        process.exit(1);
    }
}

main().catch(console.error);
