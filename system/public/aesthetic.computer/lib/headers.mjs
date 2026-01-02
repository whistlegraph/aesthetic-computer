import { getColorTokenHighlight } from "./color-highlighting.mjs";
import { tokenize } from "./kidlisp.mjs";
import { qrcode, ErrorCorrectLevel } from "../dep/@akamfoad/qr/qr.mjs";

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

/**
 * Render a QR code to the browser console using CSS-styled block characters.
 * Uses Unicode half-blocks to render 2 rows per line for compact display.
 * @param {string} url - The URL to encode in the QR code
 * @param {string} darkColor - CSS color for dark modules (default: black)
 * @param {string} lightColor - CSS color for light modules (default: white)
 */
function renderQRToConsole(url, darkColor = '#4ecdc4', lightColor = '#1a1a2e') {
  try {
    const qr = qrcode(url, { errorCorrectLevel: ErrorCorrectLevel.L });
    const size = qr.getModuleCount();
    
    // Generate QR as SVG data URL for console image display
    const moduleSize = 4; // pixels per module
    const margin = moduleSize; // quiet zone
    const svgSize = size * moduleSize + margin * 2;
    
    // Use a single path for all dark modules to avoid gaps
    let pathData = '';
    for (let row = 0; row < size; row++) {
      for (let col = 0; col < size; col++) {
        if (qr.isDark(row, col)) {
          const x = col * moduleSize + margin;
          const y = row * moduleSize + margin;
          // Add each square to the path (M = moveto, h = horizontal line, v = vertical line, z = close)
          pathData += `M${x} ${y}h${moduleSize}v${moduleSize}h-${moduleSize}z`;
        }
      }
    }
    
    const svg = `<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 ${svgSize} ${svgSize}' shape-rendering='crispEdges'><rect width='100%' height='100%' fill='${lightColor}'/><path d='${pathData}' fill='${darkColor}'/></svg>`;
    const dataUrl = `data:image/svg+xml,${encodeURIComponent(svg)}`;
    
    // Display as console image using CSS background trick
    const displaySize = 100; // display size in pixels
    console.log(
      '%c ',
      `font-size: 1px; padding: ${displaySize/2}px; background: url("${dataUrl}") no-repeat; background-size: ${displaySize}px ${displaySize}px;`
    );
  } catch (e) {
    // Silently fail - QR code is not essential
    console.warn('QR generation failed:', e);
  }
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
  
  // Show PACK mode in PACK environments (check for window first)
  if (typeof window !== 'undefined' && window.acPACK_MODE) {
    
    // Display minimal colophonic information if available
    if (window.acPACK_COLOPHON) {
      const colophon = window.acPACK_COLOPHON;
      const piece = colophon.piece;
      const build = colophon.build;
      
      // Override the title with git commit info in PACK mode
      console.clear();
      
      // Get today's color scheme for the title
      const today = new Date().getDay();
      const isDarkMode = true; // Console is typically dark
      const colorScheme = getDayColorScheme(isDarkMode);
      const theme = colorScheme.dark;
      
      // Per-character colored "Aesthetic.Computer" title (subtle pastel rainbow)
      const titleChars = "Aesthetic.Computer".split('');
      const colors = ['#ff9999', '#ffcc99', '#ffff99', '#99ff99', '#99ccff', '#cc99ff', '#ff99cc'];
      let titleFormatted = '';
      let titleStyles = [];
      
      titleChars.forEach((char, i) => {
        titleFormatted += `%c${char}`;
        const color = colors[i % colors.length];
        titleStyles.push(`color: ${color}; font-weight: bold; font-size: 14px;`);
      });
      
      console.log(titleFormatted, ...titleStyles);
      
      // Render QR code to piece URL right after the title
      const pieceSlugForQR = piece.isKidLisp ? `$${piece.name}` : piece.name;
      const pieceUrlForQR = `https://aesthetic.computer/${pieceSlugForQR}`;
      renderQRToConsole(pieceUrlForQR);
      
      // Third line: Piece info with colored piece name ($ prefix only for KidLisp)
      const piecePrefix = piece.isKidLisp ? '$' : '';
      // Format author: strip any leading @ and only add it back for non-anon authors
      const authorRaw = build.author.replace(/^@/, '');
      const authorDisplay = authorRaw === 'anon' ? 'anon' : `@${authorRaw}`;
      console.log(
        `%c${piecePrefix}${piece.name} %cis a %cpiece%c by %c${authorDisplay}`,
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
            `%cIts %cKidLisp%c source is... %c${highlighted.text}`,
            "color: #6c757d; font-size: 10px;",
            "color: #28a745; font-weight: bold; font-size: 10px;",
            "color: #6c757d; font-size: 10px;",
            "font-family: monospace; font-size: 10px;",
            ...highlighted.styles
          );
        } catch (error) {
          console.log(
            `%cIts %cKidLisp%c source is... %c${piece.sourceCode}`,
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
      
      // Show filename if available
      const bundleFilename = build.filename || null;
      if (bundleFilename) {
        console.log(`%cPacked as %c${bundleFilename}%c on %c${formattedDate}%c`, 
          "color: #6c757d; font-size: 11px;",
          "color: #e83e8c; font-weight: bold; font-size: 11px;",
          "color: #6c757d; font-size: 11px;",
          "color: #4ecdc4; font-size: 11px;",
          "color: #6c757d; font-size: 11px;"
        );
      } else {
        console.log(`%cThis copy was packed on %c${formattedDate}%c`, 
          "color: #6c757d; font-size: 11px;",
          "color: #4ecdc4; font-size: 11px;",
          "color: #6c757d; font-size: 11px;"
        );
      }
      
      console.log(`%cUsing %caesthetic-computer%c git version%c${gitHash}%c${dirtyStatus}`, 
        "color: #6c757d; font-size: 11px;",
        "color: #e83e8c; font-weight: bold; font-size: 11px;",
        "color: #6c757d; font-size: 11px;",
        "color: #ffc107; font-size: 11px;",
        build.gitIsDirty ? "color: #dc3545; font-size: 11px;" : "color: #343a40; font-size: 11px;"
      );
      
      // Links at the end with labels
      const pieceSlug = piece.isKidLisp ? `/$${piece.name}` : `/${piece.name}`;
      console.log(
        `%cView this piece at %chttps://aesthetic.computer${pieceSlug}`,
        "color: #6c757d; font-size: 10px;",
        "color: #4ecdc4; font-size: 10px; text-decoration: underline;"
      );
      
      console.log(
        `%cLearn %cKidLisp%c at %chttps://kidlisp.com`,
        "color: #6c757d; font-size: 10px;",
        "color: #28a745; font-weight: bold; font-size: 10px;",
        "color: #6c757d; font-size: 10px;",
        "color: #28a745; font-size: 10px; text-decoration: underline;"
      );
      
      console.log(
        `%cContribute on %cGitHub%c at %chttps://github.com/whistlegraph/aesthetic-computer`,
        "color: #6c757d; font-size: 10px;",
        "color: #343a40; font-weight: bold; font-size: 10px;",
        "color: #6c757d; font-size: 10px;",
        "color: #6c757d; font-size: 10px; text-decoration: underline;"
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
 * Prepare KidLisp source for console logging while preserving whitespace.
 * @param {string} code - The KidLisp code block to display.
 * @returns {object} Console formatting payload.
 */
function formatKidLispForConsole(code) {
  const baseStyle = 'color: #6c757d; font-family: monospace; font-size: 11px; white-space: pre;';
  const isNodeEnv = typeof process !== 'undefined' && !!process.versions?.node;
  const isBrowserConsole =
    !isNodeEnv &&
    typeof window !== 'undefined' &&
    typeof window.document !== 'undefined' &&
    typeof window.navigator !== 'undefined';

  if (typeof code !== 'string' || code.length === 0) {
    if (isBrowserConsole) {
      return { text: '\n%c%c', styles: [baseStyle, ''] };
    }
    return { text: '\n', styles: [] };
  }

  const normalized = code.replace(/\r\n/g, '\n');

  if (!isBrowserConsole) {
    return { text: `\n${normalized}`, styles: [] };
  }

  try {
    const tokens = tokenize(normalized);
    const segments = [];

    const pushSegment = (text, style) => {
      if (!text) return;
      segments.push({ text, style });
    };

    const applyStyle = (overrides = '') => `${baseStyle} ${overrides}`.trim();

    const pushTokenSegments = (token) => {
      if (!token) return;

      const rainbowColors = ['#ff5555', '#ff9800', '#fdd835', '#4caf50', '#2196f3', '#673ab7', '#ff4081'];
      const zebraColors = ['#000000', '#ffffff'];

      const cleanToken = token.startsWith('"') && token.endsWith('"')
        ? token.slice(1, -1)
        : token;

      // Handle punctuation early
      if (token === '(' || token === ')' || token === ',') {
        pushSegment(token, applyStyle('color: #adb5bd;'));
        return;
      }

      if (token.startsWith('"') && token.endsWith('"')) {
        pushSegment(token, applyStyle('color: #e83e8c;'));
        return;
      }

      if (/^-?\d+(?:\.\d+)?$/.test(token)) {
        pushSegment(token, applyStyle('color: #28a745; font-weight: bold;'));
        return;
      }

      if (/^\d*\.?\d+s(?:\.\.|!|\.\.\.)?$/.test(token)) {
        pushSegment(token, applyStyle('color: #17a2b8; font-weight: bold;'));
        return;
      }

      const highlight = getColorTokenHighlight(token);
      if (highlight && highlight !== 'orange') {
        // Handle COMPOUND colors (like #codes with separate prefix/identifier colors)
        if (highlight.startsWith('COMPOUND:')) {
          const parts = highlight.split(':');
          const prefixColor = parts[1]; // Color for prefix symbol (# or $)
          const identifierColor = parts[2]; // Color for identifier part
          
          // Get the actual prefix character from the token
          const prefixChar = token.charAt(0); // Could be $ or #
          const identifierPart = token.substring(1);
          
          // Convert color names to CSS colors if needed
          const prefixCss = prefixColor.includes(',') ? `rgb(${prefixColor})` : prefixColor;
          const identifierCss = identifierColor.includes(',') ? `rgb(${identifierColor})` : identifierColor;
          
          // Apply colors to each part
          pushSegment(prefixChar, applyStyle(`color: ${prefixCss}; font-weight: bold;`));
          pushSegment(identifierPart, applyStyle(`color: ${identifierCss}; font-weight: bold;`));
          return;
        }

        if (highlight === 'RAINBOW') {
          for (let i = 0; i < token.length; i++) {
            const ch = token[i];
            const color = rainbowColors[i % rainbowColors.length];
            pushSegment(ch, applyStyle(`color: ${color}; font-weight: bold;`));
          }
          return;
        }

        if (highlight === 'ZEBRA') {
          for (let i = 0; i < token.length; i++) {
            const ch = token[i];
            const color = zebraColors[i % zebraColors.length];
            pushSegment(ch, applyStyle(`color: ${color}; font-weight: bold; background: ${color === '#000000' ? '#ffffff' : '#000000'};`));
          }
          return;
        }

        // Handle FADE expressions like "fade:red-blue-yellow"
        if (highlight.startsWith('FADE:')) {
          const fadeToken = highlight.substring(5); // Remove 'FADE:' prefix
          const parts = fadeToken.split(':');
          // Color "fade" in emerald
          pushSegment('fade', applyStyle('color: #3cb371; font-weight: bold;'));
          pushSegment(':', applyStyle('color: #888;'));
          
          // Get the color part (could be parts[1] or parts[2] if neat is first)
          let colorPart = parts[1] || '';
          let remaining = parts.slice(2);
          if (colorPart === 'neat' && parts[2]) {
            pushSegment('neat', applyStyle('color: #888; font-style: italic;'));
            pushSegment(':', applyStyle('color: #888;'));
            colorPart = parts[2];
            remaining = parts.slice(3);
          }
          
          // Split colors by dash and color each one
          const colors = colorPart.split('-');
          colors.forEach((colorName, i) => {
            const colorHighlight = getColorTokenHighlight(colorName);
            let cssColor = '#888';
            if (colorHighlight && !colorHighlight.startsWith('FADE:') && colorHighlight !== 'orange') {
              cssColor = colorHighlight.includes(',') ? `rgb(${colorHighlight})` : colorHighlight;
            } else if (cssColors && cssColors[colorName.toLowerCase()]) {
              const rgb = cssColors[colorName.toLowerCase()];
              cssColor = `rgb(${rgb[0]},${rgb[1]},${rgb[2]})`;
            }
            pushSegment(colorName, applyStyle(`color: ${cssColor}; font-weight: bold;`));
            if (i < colors.length - 1) {
              pushSegment('-', applyStyle('color: #888;'));
            }
          });
          
          // Handle remaining parts (direction, neat suffix)
          remaining.forEach(part => {
            pushSegment(':', applyStyle('color: #888;'));
            if (part === 'neat' || part === 'vertical' || part === 'horizontal') {
              pushSegment(part, applyStyle('color: #888; font-style: italic;'));
            } else {
              pushSegment(part, applyStyle('color: #888;'));
            }
          });
          return;
        }

        const cssColor = highlight.includes(',') ? `rgb(${highlight})` : highlight;
        pushSegment(token, applyStyle(`color: ${cssColor}; font-weight: bold;`));
        return;
      }

      const lower = cleanToken.toLowerCase();
      const drawingCommands = ['line', 'rect', 'circle', 'box', 'tri', 'plot', 'write', 'ink', 'paper', 'brush', 'flood', 'wipe', 'paste', 'stamp', 'scroll', 'zoom', 'contrast', 'blur', 'repeat', 'spin', 'jump'];
      const controlFlow = ['if', 'when', 'unless', 'each', 'loop', 'later', 'once', 'def'];

      if (drawingCommands.includes(lower)) {
        pushSegment(token, applyStyle('color: #17a2b8; font-weight: bold;'));
        return;
      }

      if (controlFlow.includes(lower)) {
        pushSegment(token, applyStyle('color: #6f42c1; font-weight: bold;'));
        return;
      }

      if (lower === 'kidlisp') {
        pushSegment(token, applyStyle('color: #28a745; font-weight: bold;'));
        return;
      }

      pushSegment(token, baseStyle);
    };

    let cursor = 0;
    tokens.forEach((token) => {
      const idx = normalized.indexOf(token, cursor);
      if (idx === -1) {
        return;
      }
      if (idx > cursor) {
        pushSegment(normalized.slice(cursor, idx), baseStyle);
      }
      pushTokenSegments(token);
      cursor = idx + token.length;
    });

    if (cursor < normalized.length) {
      pushSegment(normalized.slice(cursor), baseStyle);
    }

    // Merge consecutive segments with identical styles to reduce console args
    const mergedSegments = [];
    for (const segment of segments) {
      const prev = mergedSegments[mergedSegments.length - 1];
      if (prev && prev.style === segment.style) {
        prev.text += segment.text;
      } else {
        mergedSegments.push({ ...segment });
      }
    }

    const textParts = ['\n'];
    const styles = [];

    mergedSegments.forEach(({ text, style }) => {
      if (!text) return;
      const escaped = text.replace(/%/g, '%%');
      textParts.push(`%c${escaped}`);
      styles.push(style);
    });

    textParts.push('%c');
    styles.push('');

    return {
      text: textParts.join(''),
      styles,
    };
  } catch (error) {
    console.warn('KidLisp syntax highlighting error:', error);
    return { text: `\n${normalized}`, styles: [] };
  }
}

// Function for logging kidlisp codes with day-based theming
// Set options.quiet = true to skip the verbose source code log (used in kidlisp.com editor)
export function logKidlispCode(source, code, isDarkMode, options = {}) {
  // Skip all logging in quiet mode (kidlisp.com editor context)
  if (options.quiet) return;
  
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