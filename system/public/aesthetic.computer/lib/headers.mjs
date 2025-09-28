// Import color highlighting utilities for KidLisp syntax highlighting
import { colorizeColorName, getColorTokenHighlight } from "./color-highlighting.mjs";
// Import KidLisp explanation functionality and tokenizer for proper syntax highlighting
import { explainKidLisp, tokenize, getSyntaxHighlightingColors } from "./kidlisp.mjs";

// Weekly color schemes for console styling
function getDayColorScheme(isDarkMode) {
  const day = new Date().getDay(); // 0 = Sunday, 1 = Monday, etc.
  
  const schemes = {
    // Sunday - Purple/Lavender
    0: {
      dark: {
        header: { bg: 'rgba(75, 0, 130)', color: 'rgb(230, 230, 250)', border: 'rgb(147, 112, 219)' },
        code: { bg: '#2d1b69', color: '#b19cd9', shadow: 'rgba(177, 156, 217, 0.3)' }
      },
      light: {
        header: { bg: 'rgba(248, 248, 255)', color: 'rgb(75, 0, 130)', border: 'rgb(147, 112, 219)' },
        code: { bg: '#f8f8ff', color: '#4b0082', border: '#9370db' }
      }
    },
    // Monday - Blue/Ocean
    1: {
      dark: {
        header: { bg: 'rgba(0, 20, 40)', color: 'rgb(173, 216, 230)', border: 'rgb(70, 130, 180)' },
        code: { bg: '#1e3a8a', color: '#7dd3fc', shadow: 'rgba(125, 211, 252, 0.3)' }
      },
      light: {
        header: { bg: 'rgba(240, 248, 255)', color: 'rgb(30, 58, 138)', border: 'rgb(70, 130, 180)' },
        code: { bg: '#f0f8ff', color: '#1e3a8a', border: '#4682b4' }
      }
    },
    // Tuesday - Green/Forest
    2: {
      dark: {
        header: { bg: 'rgba(0, 50, 0)', color: 'rgb(144, 238, 144)', border: 'rgb(34, 139, 34)' },
        code: { bg: '#064e3b', color: '#6ee7b7', shadow: 'rgba(110, 231, 183, 0.3)' }
      },
      light: {
        header: { bg: 'rgba(240, 255, 240)', color: 'rgb(6, 78, 59)', border: 'rgb(34, 139, 34)' },
        code: { bg: '#f0fff0', color: '#064e3b', border: '#228b22' }
      }
    },
    // Wednesday - Orange/Sunset
    3: {
      dark: {
        header: { bg: 'rgba(139, 69, 19)', color: 'rgb(255, 218, 185)', border: 'rgb(255, 140, 0)' },
        code: { bg: '#ea580c', color: '#fed7aa', shadow: 'rgba(254, 215, 170, 0.3)' }
      },
      light: {
        header: { bg: 'rgba(255, 248, 220)', color: 'rgb(234, 88, 12)', border: 'rgb(255, 140, 0)' },
        code: { bg: '#fff8dc', color: '#ea580c', border: '#ff8c00' }
      }
    },
    // Thursday - Red/Ruby
    4: {
      dark: {
        header: { bg: 'rgba(139, 0, 0)', color: 'rgb(255, 182, 193)', border: 'rgb(220, 20, 60)' },
        code: { bg: '#991b1b', color: '#fca5a5', shadow: 'rgba(252, 165, 165, 0.3)' }
      },
      light: {
        header: { bg: 'rgba(255, 245, 245)', color: 'rgb(153, 27, 27)', border: 'rgb(220, 20, 60)' },
        code: { bg: '#fff5f5', color: '#991b1b', border: '#dc143c' }
      }
    },
    // Friday - Pink/Rose
    5: {
      dark: {
        header: { bg: 'rgba(199, 21, 133)', color: 'rgb(252, 231, 243)', border: 'rgb(236, 72, 153)' },
        code: { bg: '#be185d', color: '#fbcfe8', shadow: 'rgba(251, 207, 232, 0.3)' }
      },
      light: {
        header: { bg: 'rgba(253, 242, 248)', color: 'rgb(190, 24, 93)', border: 'rgb(236, 72, 153)' },
        code: { bg: '#fdf2f8', color: '#be185d', border: '#ec4899' }
      }
    },
    // Saturday - Teal/Cyan (Original aesthetic computer style)
    6: {
      dark: {
        header: { bg: 'rgba(10, 20, 40)', color: 'rgb(200, 200, 250)', border: 'rgb(120, 120, 170)' },
        code: { bg: '#1a1a2e', color: '#16a085', shadow: 'rgba(22, 160, 133, 0.3)' }
      },
      light: {
        header: { bg: 'rgba(248, 249, 250)', color: 'rgb(52, 58, 64)', border: 'rgb(108, 117, 125)' },
        code: { bg: '#f8f9fa', color: '#2c3e50', border: '#6c757d' }
      }
    }
  };

  return schemes[day];
}

export function headers(isDarkMode) {
  // Auto-detect dark mode if not provided
  if (isDarkMode === undefined) {
    isDarkMode = true; // Default to dark mode
    if (typeof window !== 'undefined' && window.matchMedia) {
      isDarkMode = window.matchMedia("(prefers-color-scheme: dark)").matches;
    }
  }

  const colorScheme = getDayColorScheme(isDarkMode);
  const theme = isDarkMode ? colorScheme.dark : colorScheme.light;

  // Define styles based on day and theme
  const titleStyle = `background: ${theme.header.bg};
       color: ${theme.header.color};
       font-size: 18px;
       padding: 0 0.25em;
       border-radius: 0.15em;
       border-bottom: 0.75px solid ${theme.header.border};
       border-right: 0.75px solid ${theme.header.border};`;

  const teiaStyle = isDarkMode
    ? `background: rgba(0, 0, 0, 0.9);
       color: rgb(255, 255, 255);
       font-size: 16px;
       padding: 0 0.35em;
       border-radius: 0.15em;
       border: 0.75px solid rgb(120, 120, 120);`
    : `background: rgba(255, 255, 255, 0.9);
       color: rgb(33, 37, 41);
       font-size: 16px;
       padding: 0 0.35em;
       border-radius: 0.15em;
       border: 0.75px solid rgb(108, 117, 125);`;

  // Title with colored dot
  console.log(
    "%cAesthetic%c.%cComputer",
    "color: #ff6b9d; font-weight: bold; font-size: 16px;",
    "color: #4ecdc4; font-weight: bold; font-size: 16px;",
    "color: #ff6b9d; font-weight: bold; font-size: 16px;"
  );
  
  // Show TEIA mode in TEIA environments (check for window first)
  if (typeof window !== 'undefined' && window.acTEIA_MODE) {
    
    // Display minimal colophonic information if available
    if (window.acTEIA_COLOPHON) {
      const colophon = window.acTEIA_COLOPHON;
      const piece = colophon.piece;
      const build = colophon.build;
      
      // Override the title with git commit info in TEIA mode
      console.clear();
      
      // Get today's color scheme for the title
      const today = new Date().getDay();
      const isDarkMode = true; // Console is typically dark
      const colorScheme = getDayColorScheme(isDarkMode);
      const theme = colorScheme.dark;
      
      // First line: Aesthetic Computer title with daily changing background and colored dot
      console.log(
        `%cAesthetic.Computer`,
        `background: ${theme.header.bg}; color: ${theme.header.color}; font-weight: bold; font-size: 12px; padding: 2px 4px; border-radius: 3px; border: 1px solid ${theme.header.border};`
      );
      

      
      // Third line: Piece info with colored piece name
      console.log(
        `%c${piece.name} %cis a %cpiece%c by %c${build.author}`,
        "color: #ffc107; font-weight: bold; font-size: 11px;",
        "color: #6c757d; font-size: 10px;",
        "color: #343a40; font-weight: bold; font-size: 10px;",
        "color: #6c757d; font-size: 10px;",
        "color: #dc3545; font-size: 10px;"
      );
      
      if (piece.sourceCode && piece.isKidLisp) {
        // Use syntax highlighting for KidLisp code with type indicator
        try {
          const highlighted = formatKidLispForConsole(piece.sourceCode);
          console.log(
            `%cIts %cKidLisp%c source is:%c ${highlighted.text}`,
            "color: #6c757d; font-size: 10px;",
            "color: #28a745; font-weight: bold; font-size: 10px;",
            "color: #6c757d; font-size: 10px;",
            "color: #28a745; font-size: 10px;",
            ...highlighted.styles
          );
        } catch (error) {
          console.log(
            `%cIts %cKidLisp%c source is:%c ${piece.sourceCode}`,
            "color: #6c757d; font-size: 10px;",
            "color: #28a745; font-weight: bold; font-size: 10px;",
            "color: #6c757d; font-size: 10px;",
            "color: #28a745; font-family: monospace; font-size: 11px;"
          );
        }
      }
      
      // Extract date from filename or use packTime as fallback
      let formattedDate;
      if (build.zipFilename) {
        // Extract date from filename like "$bop-2025.09.23.17.34.00.680.zip"
        const dateMatch = build.zipFilename.match(/(\d{4})\.(\d{2})\.(\d{2})\.(\d{2})\.(\d{2})\.(\d{2})/);
        if (dateMatch) {
          const [, year, month, day, hour, minute, second] = dateMatch;
          const packDate = new Date(year, month - 1, day, hour, minute, second);
          const monthNames = ["January", "February", "March", "April", "May", "June",
            "July", "August", "September", "October", "November", "December"];
          const monthName = monthNames[packDate.getMonth()];
          const dayNum = packDate.getDate();
          const ordinalSuffix = (d) => {
            if (d > 3 && d < 21) return 'th';
            switch (d % 10) {
              case 1: return 'st';
              case 2: return 'nd';
              case 3: return 'rd';
              default: return 'th';
            }
          };
          const time = packDate.toLocaleTimeString('en-US', { 
            hour: 'numeric', 
            minute: '2-digit', 
            second: 'numeric',
            hour12: true 
          });
          formattedDate = `${monthName} ${dayNum}${ordinalSuffix(dayNum)}, ${year} at ${time}`;
        } else {
          // Fallback to packTime
          const packDate = new Date(build.packTime);
          formattedDate = packDate.toLocaleDateString();
        }
      } else {
        // Fallback to packTime
        const packDate = new Date(build.packTime);
        formattedDate = packDate.toLocaleDateString();
      }

      // Get file count info
      const fileCount = build.fileCount ? ` ${build.fileCount} files` : '';
      const gitHash = ` ${build.gitCommit}`;
      const dirtyStatus = build.gitIsDirty ? ' (dirty)' : '';
      
      console.log(`%cThis copy was packed for %cTEIA%c on %c${formattedDate}%c`, 
        "color: #6c757d; font-size: 11px;",
        "background: #2c3e50; color: #ecf0f1; font-weight: bold; font-size: 11px; padding: 1px 3px; border-radius: 2px;",
        "color: #6c757d; font-size: 11px;",
        "color: #4ecdc4; font-size: 11px;",
        "color: #6c757d; font-size: 11px;"
      );
      
      console.log(`%cUsing %caesthetic-computer%c git version%c${gitHash}%c${dirtyStatus}`, 
        "color: #6c757d; font-size: 11px;",
        "color: #e83e8c; font-weight: bold; font-size: 11px;",
        "color: #6c757d; font-size: 11px;",
        "color: #ffc107; font-size: 11px;",
        build.gitIsDirty ? "color: #dc3545; font-size: 11px;" : "color: #343a40; font-size: 11px;"
      );
    }
  }
}

/**
 * Format file size in bytes to human readable format
 * @param {number} bytes - Size in bytes
 * @returns {string} Formatted size string
 */
function formatFileSize(bytes) {
  if (bytes === 0) return '0 B';
  const k = 1024;
  const sizes = ['B', 'KB', 'MB', 'GB'];
  const i = Math.floor(Math.log(bytes) / Math.log(k));
  return parseFloat((bytes / Math.pow(k, i)).toFixed(1)) + ' ' + sizes[i];
}

/**
 * Apply KidLisp syntax highlighting to code for console display using the actual tokenizer
 * @param {string} code - The KidLisp code to highlight
 * @returns {object} Object with formatted text and styles array for console.log
 */
function formatKidLispForConsole(code) {
  if (!code || typeof code !== 'string') {
    return { text: code || '', styles: ['color: #6c757d; font-family: monospace;'] };
  }

  try {
    // Use the actual KidLisp tokenizer (returns array of token strings)
    const tokens = tokenize(code);
    
    let formattedText = '';
    const styles = [];
    let needsSpace = false;
    
    tokens.forEach((token, index) => {
      // Add space between tokens (except at the beginning)
      if (needsSpace && !['(', ')', ','].includes(token)) {
        formattedText += '%c ';
        styles.push('color: #6c757d; font-family: monospace; font-weight: normal;');
      }
      
      let tokenColor = '#6c757d'; // default gray
      let fontWeight = 'normal';
      
      // Classify token type and apply appropriate styling
      if (token.startsWith('"') && token.endsWith('"')) {
        // String literals
        tokenColor = '#e83e8c'; // pink
        fontWeight = 'normal';
      } else if (!isNaN(token) && token !== '' && !isNaN(parseFloat(token))) {
        // Numbers
        tokenColor = '#28a745'; // green
        fontWeight = 'bold';
      } else if (['(', ')', ','].includes(token)) {
        // Punctuation
        tokenColor = '#6c757d'; // gray
        fontWeight = 'normal';
      } else {
        // Check if it's a color name first
        const colorHighlight = getColorTokenHighlight(token);
        if (colorHighlight && colorHighlight !== '#ff7043') {
          // Handle color names with their actual colors
          if (colorHighlight === 'RAINBOW') {
            // Rainbow highlighting - each character gets a different color
            const rainbowColors = ['#ff0000', '#ff7f00', '#ffff00', '#00ff00', '#0000ff', '#4b0082', '#9400d3'];
            for (let i = 0; i < token.length; i++) {
              formattedText += `%c${token[i]}`;
              styles.push(`color: ${rainbowColors[i % rainbowColors.length]}; font-family: monospace; font-weight: bold;`);
            }
            needsSpace = true;
            return;
          } else if (colorHighlight.includes(',')) {
            // RGB color
            tokenColor = `rgb(${colorHighlight})`;
            fontWeight = 'bold';
          } else {
            // Named color
            tokenColor = colorHighlight;
            fontWeight = 'bold';
          }
        } else {
          // Check for KidLisp keywords
          const lowerToken = token.toLowerCase();
          if (['line', 'rect', 'circle', 'text', 'ink', 'paper', 'brush', 'wipe', 'clear', 'blur', 'noblur'].includes(lowerToken)) {
            // Drawing commands - blue
            tokenColor = '#17a2b8';
            fontWeight = 'bold';
          } else if (['if', 'when', 'unless', 'each', 'repeat', 'loop'].includes(lowerToken)) {
            // Control flow - purple
            tokenColor = '#6f42c1';
            fontWeight = 'bold';
          } else if (/^[a-zA-Z][a-zA-Z0-9_-]*$/.test(token)) {
            // Function names/identifiers - orange
            tokenColor = '#fd7e14';
            fontWeight = 'bold';
          }
        }
      }
      
      // Apply styling to each character of the token
      for (let i = 0; i < token.length; i++) {
        formattedText += `%c${token[i]}`;
        styles.push(`color: ${tokenColor}; font-family: monospace; font-weight: ${fontWeight};`);
      }
      
      // Add space after comma
      if (token === ',') {
        formattedText += '%c ';
        styles.push('color: #6c757d; font-family: monospace; font-weight: normal;');
      }
      
      needsSpace = !['(', ','].includes(token);
    });
    
    return { text: formattedText, styles: styles };
  } catch (error) {
    console.warn('KidLisp syntax highlighting error:', error);
    // Fallback to plain text
    return { text: code, styles: ['color: #6c757d; font-family: monospace;'] };
  }
}

// Function for logging kidlisp codes with day-based theming
export function logKidlispCode(source, code, isDarkMode) {
  // Auto-detect dark mode if not provided
  if (isDarkMode === undefined) {
    isDarkMode = true; // Default to dark mode
    if (typeof window !== 'undefined' && window.matchMedia) {
      isDarkMode = window.matchMedia("(prefers-color-scheme: dark)").matches;
    }
  }

  const colorScheme = getDayColorScheme(isDarkMode);
  const theme = isDarkMode ? colorScheme.dark : colorScheme.light;

  // Log the source code with KidLisp syntax highlighting
  const highlighted = formatKidLispForConsole(source);
  console.log(highlighted.text, ...highlighted.styles);

  // Create the styled code log based on current day theme
  const codeStyle = isDarkMode 
    ? `display: inline-block; background: ${theme.code.bg}; color: ${theme.code.color}; font-family: monospace; font-weight: bold; font-size: 14px; line-height: 1.4; white-space: pre-wrap; padding: 3px 6px; box-shadow: 0 2px 8px rgba(0, 0, 0, 0.3); text-shadow: 0 0 4px ${theme.code.shadow}, 0 0 8px ${theme.code.shadow};`
    : `display: inline-block; background: ${theme.code.bg}; color: ${theme.code.color}; font-family: monospace; font-weight: bold; font-size: 14px; line-height: 1.4; white-space: pre-wrap; padding: 3px 6px; border: 1px solid ${theme.code.border}; box-shadow: 0 2px 6px rgba(0, 0, 0, 0.15);`;

  // Log the generated $code with day-themed styling
  console.log(
    `%c$${code}`,
    codeStyle
  );
}