#!/usr/bin/env node
// KidLisp Source Tree Analyzer
// Usage: ./source-tree.mjs $cow
// Output: A visual tree representation of KidLisp piece dependencies

import { execSync } from 'child_process';
import chalk from 'chalk';
import terminalKit from 'terminal-kit';
import gradient from 'gradient-string';

const term = terminalKit.terminal;
// Shows the embedd// Format piece name with bright gradient effects and $ prefix
function formatPieceName(pieceName, showDollar = true) {
  let result = '';
  
  // Always add bright gradient $ prefix for piece names
  if (showDollar) {
    result += gradient('lime', 'cyan')('$');
  }
  
  // Create brighter gradient colors for better visibility
  const brightGradientColors = ['#ff1493', '#00ffff', '#ffff00', '#ff4500', '#adff2f', '#ff69b4'];
  const startColor = brightGradientColors[pieceName.length % brightGradientColors.length];
  const endColor = brightGradientColors[(pieceName.length + 2) % brightGradientColors.length];
  
  // Apply bright gradient to the piece name
  const gradientText = gradient(startColor, endColor)(pieceName);
  return result + gradientText;
}

import https from 'https';

// Import KidLisp modules for native syntax highlighting
let kidlisp, graph, num;
try {
  kidlisp = await import('../../system/public/aesthetic.computer/lib/kidlisp.mjs');
  graph = await import('../../system/public/aesthetic.computer/lib/graph.mjs');
  num = await import('../../system/public/aesthetic.computer/lib/num.mjs');
} catch (error) {
  console.warn("⚠️ Warning: Could not load KidLisp modules, using fallback highlighting");
  console.warn("Error:", error.message);
}

// ANSI terminal colors - using custom RGB instead of default terminal colors
// Color utilities using chalk for simpler API
const colors = {
  // Basic colors using chalk for text output
  red: chalk.rgb(220, 50, 47),
  green: chalk.rgb(50, 200, 50),
  yellow: chalk.rgb(255, 193, 7),
  blue: chalk.rgb(38, 139, 210),
  magenta: chalk.rgb(211, 54, 130),
  cyan: chalk.rgb(42, 161, 152),
  white: chalk.rgb(253, 246, 227),
  gray: chalk.rgb(147, 161, 161),
  dim: chalk.dim,
  
  // ANSI codes for complex formatting (still needed for syntax highlighting)
  redCode: '\x1b[38;2;220;50;47m',
  greenCode: '\x1b[38;2;50;200;50m',
  yellowCode: '\x1b[38;2;255;193;7m',
  blueCode: '\x1b[38;2;38;139;210m',
  magentaCode: '\x1b[38;2;211;54;130m',
  cyanCode: '\x1b[38;2;42;161;152m',
  whiteCode: '\x1b[38;2;253;246;227m',
  grayCode: '\x1b[38;2;147;161;161m',
  reset: '\x1b[0m',
};

// Convert KidLisp's \color\text format to ANSI colored text
// Convert KidLisp colors to ANSI escape codes with piece recognition
function convertKidlispColorsToAnsi(coloredString) {
  if (!coloredString) return '';
  
  return coloredString
    .replace(/\$(\w+)/g, (match, pieceName) => {
      // Apply character-by-character formatting to piece references
      return formatPieceName(pieceName, true);
    })
    // Handle KidLisp backslash RGB format: \r,g,b\
    .replace(/\\(\d+),(\d+),(\d+)\\/g, (match, r, g, b) => {
      return `\x1b[38;2;${r};${g};${b}m`;
    })
    // Handle KidLisp backslash RGBA format: \r,g,b,a\ (ignore alpha for terminal)
    .replace(/\\(\d+),(\d+),(\d+),(\d+)\\/g, (match, r, g, b, a) => {
      return `\x1b[38;2;${r};${g};${b}m`;
    })
    // Handle KidLisp named colors with backslashes: \colorname\
    .replace(/\\([a-zA-Z]+)\\/g, (match, colorName) => {
      return simpleColorToAnsi(colorName) || '';
    })
    // Handle curly brace RGB format: {r:123,g:456,b:789}
    .replace(/\{r:(\d+),g:(\d+),b:(\d+)\}/g, (match, r, g, b) => {
      return `\x1b[38;2;${r};${g};${b}m`;
    })
    .replace(/\{\/\}/g, '\x1b[0m')
    .replace(/\{\}/g, '\x1b[0m');
}

// Convert RGB values to ANSI color (24-bit true color if supported, fallback to 256-color)
function rgbToAnsi(r, g, b) {
  // Calculate brightness to determine if we need special handling
  const brightness = (r * 0.299 + g * 0.587 + b * 0.114);
  
  // Use 24-bit true color if supported (most modern terminals)
  if (process.env.COLORTERM === 'truecolor' || process.env.COLORTERM === '24bit') {
    // For very light colors (white, very light gray), use background color for visibility
    if (brightness > 240) {
      return `\x1b[48;2;${r};${g};${b}m\x1b[38;2;0;0;0m`; // light background, dark text
    }
    // For very dark colors (black, very dark colors), use brighter version or background
    else if (brightness < 30) {
      return `\x1b[48;2;${r};${g};${b}m\x1b[38;2;255;255;255m`; // dark background, light text
    }
    // Normal foreground color for everything else
    else {
      return `\x1b[38;2;${r};${g};${b}m`;
    }
  }
  
  // Fallback to 256-color approximation
  if (r === g && g === b) {
    // Grayscale
    if (r === 0) return '\x1b[38;5;16m'; // Black
    if (r === 255) return '\x1b[38;5;231m'; // White
    const gray = Math.round(r / 255 * 23) + 232;
    return '\x1b[38;5;' + gray + 'm';
  }
  
  // Color cube (216 colors)
  const rIndex = Math.round(r / 255 * 5);
  const gIndex = Math.round(g / 255 * 5);
  const bIndex = Math.round(b / 255 * 5);
  const colorIndex = 16 + (36 * rIndex) + (6 * gIndex) + bIndex;
  return '\x1b[38;5;' + colorIndex + 'm';
}

// Simple color name to ANSI fallback
function simpleColorToAnsi(colorName) {
  switch (colorName.toLowerCase()) {
    case "red": return '\x1b[38;2;220;50;47m';
    case "green": return '\x1b[38;2;50;200;50m';
    case "yellow": return '\x1b[38;2;255;193;7m';
    case "blue": return '\x1b[38;2;38;139;210m';
    case "magenta": return '\x1b[38;2;211;54;130m';
    case "cyan": return '\x1b[38;2;42;161;152m';
    case "white": return '\x1b[38;2;253;246;227m';
    case "gray": case "grey": return '\x1b[38;2;147;161;161m';
    case "orange": return '\x1b[38;2;255;165;0m';  // Orange
    case "purple": return '\x1b[38;2;211;54;130m';
    case "lime": return '\x1b[38;2;50;205;50m';    // Lime green
    case "limegreen": return '\x1b[38;2;50;205;50m';
    case "pink": return '\x1b[38;2;255;192;203m';  // Pink
    case "salmon": return '\x1b[38;2;250;128;114m'; // Salmon
    case "mediumseagreen": return '\x1b[38;2;60;179;113m';
    case "palegreen": return '\x1b[38;2;152;251;152m';
    default: return '';
  }
}

// Track which server is working to use it for all subsequent calls
let workingServer = null;
let workingUrl = null;

function printUsage() {
    console.log("Usage: source-tree.mjs <piece-name>");
    console.log("       source-tree.mjs --test-colors");
    console.log("       source-tree.mjs --debug-colors");
    console.log("       source-tree.mjs --test-css-colors");
    console.log("Example: source-tree.mjs $cow");
    console.log("         source-tree.mjs cow");
    console.log("");
    console.log("Options:");
    console.log("  --test-colors     Test terminal color capabilities");
    console.log("  --debug-colors    Debug color name conversions");
    console.log("  --test-css-colors Test CSS color name mappings");
    console.log("");
    console.log("Shows complete source code tree for each piece by default.");
}

// Function to syntax highlight KidLisp code using native KidLisp highlighting
function syntaxHighlight(code) {
  if (!code) return '';
  
  try {
    // If we have KidLisp and graph modules loaded, use native syntax highlighting
    if (kidlisp && graph && kidlisp.KidLisp) {
      // Create a KidLisp instance and use buildColoredKidlispString
      const kidlispInstance = new kidlisp.KidLisp();
      kidlispInstance.initializeSyntaxHighlighting(code);
      const coloredText = kidlispInstance.buildColoredKidlispString();
      
      // Convert KidLisp's \color\text format to ANSI codes
      return convertKidlispColorsToAnsi(coloredText);
    } else {
      // Use simple fallback highlighting
      return simpleHighlight(code);
    }
  } catch (error) {
    console.warn('⚠️ KidLisp native highlighting failed:', error.message);
    return simpleHighlight(code);
  }
}

// Simple fallback highlighting function using chalk for clean color API
function simpleHighlight(code) {
  return code
    // Piece name references (like $39i, $cow, etc.)
    .replace(/\$([a-zA-Z0-9_-]+)/g, (match, pieceName) => {
      return formatPieceName(pieceName, true);
    })
    // Comments
    .replace(/;[^\n]*/g, match => colors.gray(match))
    // String literals
    .replace(/"[^"]*"/g, match => colors.green(match))
    // Numbers
    .replace(/\b\d+(\.\d+)?\b/g, match => colors.cyan(match))
    // Keywords and special forms
    .replace(/\b(define|if|cond|let|lambda|quote|quasiquote|unquote|and|or|not|car|cdr|cons|list|map|filter|fold|apply)\b/g, 
             match => colors.magenta(match))
    // Function calls (first item in parentheses)
    .replace(/\(([a-zA-Z][a-zA-Z0-9-]*)/g, match => {
      const parts = match.split('(');
      const funcName = parts[1];
      return `(${colors.yellow(funcName)}`;
    })
    // Embed calls
    .replace(/#embed\s+([a-zA-Z0-9-]+)/g, match => {
      const parts = match.split(' ');
      return `${colors.red('#embed')} ${colors.blue(parts[1])}`;
    });
}

async function fetchSourceFromEndpoint(url, cleanName, isProduction = false) {
    return new Promise((resolve, reject) => {
        const timeout = setTimeout(() => {
            reject(new Error(`Request timeout for ${cleanName}`));
        }, 3000);
        
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
            // Reset and try both servers again
            workingServer = null;
            workingUrl = null;
        }
    }
    
    // Try local development server first
    const localUrl = `https://localhost:8888/.netlify/functions/store-kidlisp?code=${cleanName}`;
    
    try {
        const source = await fetchSourceFromEndpoint(localUrl, cleanName, 'local');
        workingServer = 'local';
        workingUrl = localUrl;
        return source;
    } catch (localError) {
        // Fallback to production
        const productionUrl = `https://aesthetic.computer/api/store-kidlisp?code=${cleanName}`;
        
        try {
            const source = await fetchSourceFromEndpoint(productionUrl, cleanName, 'production');
            workingServer = 'production';
            workingUrl = productionUrl;
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

// ASCII art generation using toilet command (matching project aesthetic)

// Generate ASCII art for piece names using toilet with beautiful gradients
function generateAsciiArt(text, isSubtree = false) {
  try {
    // Escape the text properly for shell
    const safeText = text.replace(/\$/g, '\\$');
    
    // Use different fonts for main vs subtree items
    const font = isSubtree ? 'smbraille' : 'smblock';
    
    // Generate ASCII art using toilet with appropriate font
    const result = execSync(`echo "${safeText}" | toilet -f ${font}`, { encoding: 'utf8' });
    
    const lines = result.split('\n').filter(line => line.trim() !== '');
    
    // Apply gradient effects to the ASCII art
    if (lines.length > 0) {
      return lines.map(line => {
        if (isSubtree) {
          // Green coloring for subtree items (piece references)
          return gradient(['lime', 'green'])(line);
        } else {
          // Dynamic gradient based on the text content for main titles
          const gradients = [
            gradient(['cyan', 'magenta']),
            gradient(['#ff6b6b', '#4ecdc4']),
            gradient(['gold', 'orange']),
            gradient(['lime', 'cyan']),
            gradient(['purple', 'pink'])
          ];
          
          const gradientIndex = text.length % gradients.length;
          return gradients[gradientIndex](line);
        }
      });
    }
    
    // Fallback
    if (isSubtree) {
      return [gradient('lime', 'green')(text.toUpperCase())];
    } else {
      return [gradient('cyan', 'magenta')(text.toUpperCase())];
    }
  } catch (error) {
    // Fallback to gradient colored text if toilet fails
    if (isSubtree) {
      return [gradient('lime', 'green')(text.toUpperCase())];
    } else {
      return [gradient('cyan', 'magenta')(text.toUpperCase())];
    }
  }
}

// Add a beautiful border around the entire output
function addBorder() {
    const terminalWidth = process.stdout.columns || 80;
    const borderChar = '█';
    const border = borderChar.repeat(terminalWidth);
    console.log(addFullWidthBackground(border, '40;40;40'));
}

// Add full-width background to any line with optional side borders
function addFullWidthBackground(content, bgColor = '20;20;20', withBorders = false) {
  const terminalWidth = process.stdout.columns || 80;
  // More comprehensive ANSI code removal (including 24-bit RGB codes)
  const cleanContent = content.replace(/\x1b\[[0-9;]*m/g, '').replace(/\x1b\[48;2;[0-9;]*m/g, '').replace(/\x1b\[38;2;[0-9;]*m/g, '');
  
  if (withBorders) {
    // Calculate available content width (minus 4 for double borders on each side)
    const availableWidth = terminalWidth - 4;
    const padding = Math.max(0, availableWidth - cleanContent.length);
    const spaces = ' '.repeat(padding);
    
    const borderChar = '██'; // Double block for thicker border
    return `\x1b[48;2;40;40;40m${borderChar}\x1b[0m\x1b[48;2;${bgColor}m${content}${spaces}\x1b[0m\x1b[48;2;40;40;40m${borderChar}\x1b[0m`;
  } else {
    // Original behavior for content lines
    const padding = Math.max(0, terminalWidth - cleanContent.length);
    const spaces = ' '.repeat(padding);
    return `\x1b[48;2;${bgColor}m${content}${spaces}\x1b[0m`;
  }
}

// Wrap long lines to fit within the bordered display
function wrapLine(text, maxWidth) {
  const terminalWidth = process.stdout.columns || 80;
  const availableWidth = maxWidth || (terminalWidth - 8); // Account for borders and spacing
  
  // Remove ANSI codes to measure actual text length
  const stripAnsi = (str) => str.replace(/\x1b\[[0-9;]*m/g, '').replace(/\x1b\[48;2;[0-9;]*m/g, '').replace(/\x1b\[38;2;[0-9;]*m/g, '');
  
  if (stripAnsi(text).length <= availableWidth) {
    return [text];
  }
  
  const words = text.split(' ');
  const lines = [];
  let currentLine = '';
  
  for (const word of words) {
    const testLine = currentLine ? `${currentLine} ${word}` : word;
    if (stripAnsi(testLine).length <= availableWidth) {
      currentLine = testLine;
    } else {
      if (currentLine) {
        lines.push(currentLine);
        currentLine = word;
      } else {
        // Word is too long, force break
        lines.push(word);
      }
    }
  }
  
  if (currentLine) {
    lines.push(currentLine);
  }
  
  return lines;
}

// Generate unique color for each character based on its value
function getCharacterColor(char, index, pieceName) {
  // Create hash from character and its position
  const charCode = char.charCodeAt(0);
  const positionHash = (index + 1) * 37; // Prime number for better distribution
  const nameHash = pieceName.length * 23;
  const combinedHash = (charCode + positionHash + nameHash) * 17;
  
  // Generate RGB values
  const r = 120 + (combinedHash * 7) % 135;  // 120-255 range for good visibility
  const g = 120 + (combinedHash * 11) % 135;
  const b = 120 + (combinedHash * 13) % 135;
  
  // Generate background RGB (darker)
  const bgR = 20 + (combinedHash * 3) % 40;   // 20-60 range for dark background
  const bgG = 20 + (combinedHash * 5) % 40;
  const bgB = 20 + (combinedHash * 7) % 40;
  
  return {
    fg: `\x1b[38;2;${r};${g};${b}m`,
    bg: `\x1b[48;2;${bgR};${bgG};${bgB}m`,
    reset: '\x1b[0m'
  };
}

// Get color for tree depth (creates a nice gradient effect)
function getDepthColor(depth) {
  const depthColors = [
    colors.cyanCode,     // Level 0: cyan
    colors.blueCode,     // Level 1: blue  
    colors.magentaCode,  // Level 2: magenta
    colors.yellowCode,   // Level 3: yellow
    colors.greenCode,    // Level 4: green
    colors.redCode       // Level 5+: red
  ];
  return depthColors[Math.min(depth, depthColors.length - 1)];
}

async function printTreeNode(pieceName, depth = 0, prefix = "", showSource = false) {
    const indent = "  ".repeat(depth); // 2-character indentation
    
    try {
        const source = await fetchSource(pieceName);
        const cleanSource = source.replace(/\\n/g, '\n').replace(/\\"/g, '"');
        const embeddedPieces = extractEmbeddedPieces(source);
        
        // Print current piece with ASCII art for subtree $codes (depth > 0)
        if (depth > 0) {
            // Add extra spacing before subtree items
            console.log(addFullWidthBackground('', '20;20;20', true));
            
            // Generate ASCII art for subtree piece names using smbraille font with green coloring
            const asciiLines = generateAsciiArt('$' + pieceName, true);
            asciiLines.forEach(line => {
                console.log(addFullWidthBackground(`    ${line}`, '20;20;20', true)); // 4-space indent with borders
            });
        }
        
        if (showSource) {
            // Print full source code with dark backdrop and barbershop-style striped line numbers
            const sourceLines = cleanSource.split('\n');
            sourceLines.forEach((line, index) => {
                if (line.trim()) {
                    const lineNumber = (index + 1).toString().padStart(3, ' '); // 3-digit line numbers for consistent alignment
                    const highlightedLine = syntaxHighlight(line);
                    
                    // Wrap long lines to fit within borders
                    const wrappedLines = wrapLine(highlightedLine);
                    
                    wrappedLines.forEach((wrappedLine, wrapIndex) => {
                        // Barbershop alternating backgrounds only on line numbers
                        const lineNumBgColor = (index + 1) % 2 === 0 ? '35;35;35' : '10;10;10'; // Striped line numbers
                        
                        if (wrapIndex === 0) {
                            // First line gets the line number
                            const lineNumFormatted = `\x1b[48;2;${lineNumBgColor}m${lineNumber}\x1b[0m\x1b[48;2;20;20;20m`; // Line number with stripe, then immediately switch to dark backdrop
                            console.log(addFullWidthBackground(`${lineNumFormatted} ${wrappedLine}`, '20;20;20', true));
                        } else {
                            // Continuation lines get indented without line numbers
                            const continuationSpaces = `\x1b[48;2;20;20;20m   `; // 3 spaces to align with line numbers
                            console.log(addFullWidthBackground(`${continuationSpaces} ${wrappedLine}`, '20;20;20', true));
                        }
                    });
                }
            });
            // Remove extra spacing before children - only add if we're at root level AND have embedded pieces
            if (embeddedPieces.length > 0 && depth === 0) {
                console.log(addFullWidthBackground('', '20;20;20', true)); // Space only after root level
            }
        } else {
            // Show just first line as preview with striped line number and dark backdrop
            const firstLine = cleanSource.split('\n')[0];
            if (firstLine && firstLine.trim()) {
                const highlightedLine = syntaxHighlight(firstLine);
                
                // Wrap the preview line too
                const wrappedLines = wrapLine(highlightedLine);
                
                wrappedLines.forEach((wrappedLine, wrapIndex) => {
                    if (wrapIndex === 0) {
                        // First line gets line number
                        const lineNumFormatted = `\x1b[48;2;10;10;10m  1\x1b[0m\x1b[48;2;20;20;20m`; // First line gets dark stripe, then dark backdrop
                        console.log(addFullWidthBackground(`${lineNumFormatted} ${wrappedLine}`, '20;20;20', true));
                    } else {
                        // Continuation lines
                        const continuationSpaces = `\x1b[48;2;20;20;20m   `;
                        console.log(addFullWidthBackground(`${continuationSpaces} ${wrappedLine}`, '20;20;20', true));
                    }
                });
            }
        }
        
        // Process embedded pieces recursively (no pipe graphics, just indentation)
        if (depth < 5) {
            for (let i = 0; i < embeddedPieces.length; i++) {
                const embeddedPiece = embeddedPieces[i];
                await printTreeNode(embeddedPiece, depth + 1, "", showSource);
            }
        } else if (embeddedPieces.length > 0) {
            console.log(addFullWidthBackground(`${indent}  ... (max depth reached)`, '20;20;20', true));
        }
    } catch (error) {
        const formattedName = formatPieceName(pieceName);
        console.log(`${indent}${prefix}${formattedName} ${colors.gray(`(${error.message})`)}`);
    }
}

function analyzePerformanceFeatures(source) {
    process.stdout.write(`\n${colors.gray('Performance: ')}`);
    
    // Check for expensive operations
    const expensiveOps = [];
    if (source.includes('blur')) expensiveOps.push('blur');
    if (source.includes('zoom')) expensiveOps.push('zoom');
    if (source.includes('contrast')) expensiveOps.push('contrast');
    if (source.includes('spin')) expensiveOps.push('spin');
    if (source.includes('flood')) expensiveOps.push('flood');
    
    if (expensiveOps.length > 0) {
        console.log(`${expensiveOps.join(', ')}`);
    } else {
        console.log('optimized');
    }
    
    // Check for timing expressions and randomness on same line
    const features = [];
    if (source.includes('s(')) features.push('animated');
    if (source.includes('?')) features.push('random');
    
    // Count embedded layers
    const embeddedCount = extractEmbeddedPieces(source).length;
    if (embeddedCount > 0) features.push(`${embeddedCount} layers`);
    
    if (features.length > 0) {
        console.log(`${colors.gray('Features: ')}${features.join(', ')}`);
    }
}

function testCSSColorsMapping() {
  console.log('\n🎨 CSS Colors → Terminal Mapping Test:');
  
  // Test common CSS color names that should be recognizable
  const cssColors = [
    'white', 'black', 'gray', 'grey',
    'red', 'green', 'blue', 'yellow', 'cyan', 'magenta',
    'orange', 'purple', 'pink', 'brown',
    'lime', 'navy', 'teal', 'violet', 'indigo',
    'salmon', 'tan', 'gold', 'silver',
    'palegreen', 'lightblue', 'darkred', 'lightgray'
  ];
  
  console.log('\nColor Name → RGB → Terminal Result:');
  console.log('═══════════════════════════════════════');
  
  for (const colorName of cssColors) {
    // Get RGB from graph.findColor
    let rgba = null;
    if (graph && graph.findColor) {
      try {
        rgba = graph.findColor(colorName);
      } catch (e) {
        rgba = null;
      }
    }
    
    if (rgba && rgba.length >= 3) {
      const [r, g, b] = rgba;
      const ansiColor = rgbToAnsi(r, g, b);
      
      // Create test text with both foreground and background
      const fgText = `${ansiColor}${colorName}${COLORS.reset}`;
      const bgText = `\x1b[48;2;${r};${g};${b}m ${colorName} \x1b[0m`;
      
      console.log(`${colorName.padEnd(12)} → RGB(${r.toString().padStart(3)},${g.toString().padStart(3)},${b.toString().padStart(3)}) → FG: ${fgText} BG: ${bgText}`);
    } else {
      console.log(`${colorName.padEnd(12)} → NOT FOUND`);
    }
  }
  
  console.log('\n💡 Color visibility recommendations:');
  console.log('• Light colors (white, yellow, cyan) work better with dark backgrounds');
  console.log('• Dark colors (black, navy, darkred) work better with light backgrounds');
  console.log('• Consider using background colors for very light/dark text');
}

function testColorConversions() {
  console.log('\n🔍 Color Conversion Debug:');
  
  const testColors = ['white', 'black', 'gray', 'salmon', 'brown', 'palegreen', 'rainbow', 'red', 'orange'];
  
  for (const colorName of testColors) {
    // Test graph.findColor
    let rgba = null;
    if (graph && graph.findColor) {
      try {
        rgba = graph.findColor(colorName);
      } catch (e) {
        rgba = null;
      }
    }
    
    console.log(`\n${colorName}:`);
    console.log(`  graph.findColor: ${rgba ? `RGB(${rgba.slice(0,3).join(', ')})` : 'not found'}`);
    
    // Test rgbToAnsi directly
    if (rgba && rgba.length >= 3) {
      const [r, g, b] = rgba;
      const ansiColor = rgbToAnsi(r, g, b);
      console.log(`  terminal result: ${ansiColor}${colorName}${COLORS.reset}`);
    }
    
    // Test full KidLisp format conversion
    const testText = `\\${colorName}\\${colorName}`;
    const converted = convertKidlispColorsToAnsi(testText);
    console.log(`  kidlisp format result: ${converted}`);
  }
}

// Color test function
function testTerminalColors() {
  console.log('\n🎨 Terminal Color Support Test\n');
  
  // Test 16 basic colors
  console.log('Basic 16 Colors:');
  for (let i = 0; i < 16; i++) {
    process.stdout.write(`\x1b[48;5;${i}m ${i.toString().padStart(2)} \x1b[0m`);
    if ((i + 1) % 8 === 0) console.log('');
  }
  
  // Test 216 color cube (6x6x6)
  console.log('\n216 Color Cube (6x6x6):');
  for (let r = 0; r < 6; r++) {
    for (let g = 0; g < 6; g++) {
      for (let b = 0; b < 6; b++) {
        const color = 16 + (r * 36) + (g * 6) + b;
        process.stdout.write(`\x1b[48;5;${color}m  \x1b[0m`);
      }
      process.stdout.write(' ');
    }
    console.log('');
  }
  
  // Test 24 grayscale colors
  console.log('\nGrayscale (24 colors):');
  for (let i = 232; i < 256; i++) {
    process.stdout.write(`\x1b[48;5;${i}m  \x1b[0m`);
  }
  console.log('');
  
  // Test RGB colors (if supported)
  console.log('\nTrue Color (24-bit RGB) Test:');
  const rgbColors = [
    [255, 0, 0],   // Red
    [255, 165, 0], // Orange  
    [255, 255, 0], // Yellow
    [0, 255, 0],   // Green
    [0, 255, 255], // Cyan
    [0, 0, 255],   // Blue
    [128, 0, 128], // Purple
    [255, 192, 203] // Pink
  ];
  
  rgbColors.forEach(([r, g, b], i) => {
    process.stdout.write(`\x1b[48;2;${r};${g};${b}m RGB(${r},${g},${b}) \x1b[0m `);
    if ((i + 1) % 4 === 0) console.log('');
  });
  
  // Test KidLisp named colors
  console.log('\n\nKidLisp Named Colors Test:');
  const kidlispColors = [
    'red', 'orange', 'yellow', 'lime', 'green', 'cyan', 'blue', 'purple', 'magenta', 'pink',
    'white', 'gray', 'black', 'salmon', 'brown', 'tan', 'violet', 'indigo', 'navy', 'teal'
  ];
  
  kidlispColors.forEach((colorName, i) => {
    const ansiCode = simpleColorToAnsi(colorName);
    process.stdout.write(`${ansiCode}${colorName}\x1b[0m `);
    if ((i + 1) % 5 === 0) console.log('');
  });
  
  console.log('\n\n💡 Tips:');
  console.log('- If you see solid color blocks, your terminal supports 256 colors');
  console.log('- If RGB colors look smooth, your terminal supports 24-bit true color');
  console.log('- Modern terminals usually support 24-bit color (16.7M colors)');
  console.log('- Check $COLORTERM environment variable for truecolor support');
  console.log(`- Your COLORTERM: ${process.env.COLORTERM || 'not set'}`);
  console.log(`- Your TERM: ${process.env.TERM || 'not set'}`);
}

// Main script
async function main() {
    const args = process.argv.slice(2);
    
    if (args.length === 0) {
        printUsage();
        process.exit(1);
    }
    
    // Check for special commands
    if (args[0] === '--test-colors' || args[0] === 'test-colors') {
        testTerminalColors();
        process.exit(0);
    }
    
    if (args[0] === '--debug-colors' || args[0] === 'debug-colors') {
        testColorConversions();
        process.exit(0);
    }
    
    if (args[0] === '--test-css-colors' || args[0] === 'test-css-colors') {
        testCSSColorsMapping();
        process.exit(0);
    }
    
    const pieceName = args[0];
    const showSource = true; // Always show source by default
    
    // Remove $ prefix if present for display
    const cleanName = pieceName.startsWith('$') ? pieceName.substring(1) : pieceName;
    
    // Create beautiful ASCII art title with gradients
    const asciiLines = generateAsciiArt('$' + cleanName);
    
    // Add top border
    addBorder();
    console.log(addFullWidthBackground('', '20;20;20', true)); // Spacing with borders
    
    asciiLines.forEach(line => {
        console.log(addFullWidthBackground(`  ${line}`, '20;20;20', true)); // 2-space indent with borders
    });
    console.log(addFullWidthBackground('', '20;20;20', true)); // Single spacing line with borders
    
    try {
        // Build the tree
        await printTreeNode(pieceName, 0, "", showSource);
        
    } catch (error) {
        console.error(colors.red(`Error: ${error.message}`));
    } finally {
        // Add bottom border
        console.log(addFullWidthBackground('', '20;20;20', true)); // Spacing with borders
        addBorder();
        
        // Explicitly exit to prevent hanging
        process.exit(0);
    }
}

main().catch((error) => {
    console.error(colors.red(`❌ Unhandled error: ${error.message}`));
    process.exit(1);
});
