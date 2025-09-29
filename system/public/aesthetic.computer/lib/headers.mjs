import { getColorTokenHighlight } from "./color-highlighting.mjs";
import { tokenize } from "./kidlisp.mjs";

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