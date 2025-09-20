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

  // Title
  console.log(
    "%cAesthetic.Computer",
    titleStyle
  ); // Print a pretty title in the console.
  
  // Show TEIA mode in TEIA environments (check for window first)
  if (typeof window !== 'undefined' && window.acTEIA_MODE) {
    console.log(
      "%cTEIA",
      teiaStyle
    );
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

  // Log the source code in plain format
  console.log(source);

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