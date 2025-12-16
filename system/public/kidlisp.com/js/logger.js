// KidLisp.com Logging Module
// Beautiful multicolored headers + smart categorized logging
// Inspired by aesthetic.computer's headers.mjs

const LOG_LEVELS = {
  NONE: 0,
  ERROR: 1,
  WARN: 2,
  INFO: 3,
  DEBUG: 4,
  VERBOSE: 5
};

// Categories that can be individually toggled
const CATEGORIES = {
  boot: true,        // Boot sequence (important)
  splash: true,      // Splash screen animation
  platform: true,    // Platform switching
  code: true,        // Code loading/saving (important)
  editor: true,      // Monaco editor events
  auth: true,        // Auth0/login
  playback: false,   // Play/pause/stop controls
  iframe: false,     // Iframe communication
  message: false,    // PostMessage logs (very noisy)
  wallet: false,     // Tezos wallet
  socket: false,     // WebSocket session
  ui: false,         // UI interactions
  qr: false,         // QR code generation
  tabs: false,       // Tab switching
};

// Default log level
let currentLevel = LOG_LEVELS.INFO;

// Check localStorage for debug settings
try {
  const savedLevel = localStorage.getItem('kidlisp-log-level');
  if (savedLevel && LOG_LEVELS[savedLevel.toUpperCase()] !== undefined) {
    currentLevel = LOG_LEVELS[savedLevel.toUpperCase()];
  }
  
  const savedCategories = localStorage.getItem('kidlisp-log-categories');
  if (savedCategories) {
    const parsed = JSON.parse(savedCategories);
    Object.assign(CATEGORIES, parsed);
  }
} catch (e) {
  // Ignore localStorage errors
}

// Day-based color schemes (matching AC client style)
function getDayColorScheme() {
  const day = new Date().getDay();
  const schemes = {
    0: { // Sunday - Purple/Lavender
      title: ['#E8B4FF', '#D18EFF', '#B366FF', '#9544FF', '#7722FF', '#5500FF', '#7722FF', '#9544FF', '#B366FF', '#D18EFF', '#E8B4FF'],
      accent: '#9370db', tagline: '#b19cd9', primary: '#9370db', secondary: '#b19cd9'
    },
    1: { // Monday - Blue/Ocean
      title: ['#7DD3FC', '#60C5F8', '#43B7F4', '#26A9F0', '#099BEC', '#0284C7', '#099BEC', '#26A9F0', '#43B7F4', '#60C5F8', '#7DD3FC'],
      accent: '#4682b4', tagline: '#7dd3fc', primary: '#4682b4', secondary: '#7dd3fc'
    },
    2: { // Tuesday - Green/Forest
      title: ['#86EFAC', '#6AE79A', '#4EDF88', '#32D776', '#16CF64', '#059669', '#16CF64', '#32D776', '#4EDF88', '#6AE79A', '#86EFAC'],
      accent: '#228b22', tagline: '#6ee7b7', primary: '#228b22', secondary: '#6ee7b7'
    },
    3: { // Wednesday - Orange/Sunset
      title: ['#FED7AA', '#FDC78A', '#FCB76A', '#FBA74A', '#FA972A', '#F97316', '#FA972A', '#FBA74A', '#FCB76A', '#FDC78A', '#FED7AA'],
      accent: '#ff8c00', tagline: '#fed7aa', primary: '#ff8c00', secondary: '#fed7aa'
    },
    4: { // Thursday - Red/Ruby
      title: ['#FCA5A5', '#FB8585', '#FA6565', '#F94545', '#F82525', '#DC2626', '#F82525', '#F94545', '#FA6565', '#FB8585', '#FCA5A5'],
      accent: '#dc143c', tagline: '#fca5a5', primary: '#dc143c', secondary: '#fca5a5'
    },
    5: { // Friday - Pink/Rose
      title: ['#FBCFE8', '#F9B3DB', '#F797CE', '#F57BC1', '#F35FB4', '#EC4899', '#F35FB4', '#F57BC1', '#F797CE', '#F9B3DB', '#FBCFE8'],
      accent: '#ec4899', tagline: '#fbcfe8', primary: '#ec4899', secondary: '#fbcfe8'
    },
    6: { // Saturday - Teal/Cyan (AC signature)
      title: ['#5EEAD4', '#4ECDC4', '#3EBFB4', '#2EB1A4', '#1EA394', '#14B8A6', '#1EA394', '#2EB1A4', '#3EBFB4', '#4ECDC4', '#5EEAD4'],
      accent: '#14b8a6', tagline: '#5eead4', primary: '#4ecdc4', secondary: '#16a085'
    }
  };
  return schemes[day];
}

const dayColors = getDayColorScheme();

// Badge styles for different categories
const badges = {
  boot: ['🚀', dayColors.accent],
  splash: ['🎨', '#a855f7'],
  platform: ['🎮', '#f43f5e'],
  code: ['💾', '#10b981'],
  editor: ['📝', '#f59e0b'],
  auth: ['🔐', '#8b5cf6'],
  playback: ['▶️', '#22c55e'],
  iframe: ['🖼️', '#3b82f6'],
  message: ['📨', '#6366f1'],
  wallet: ['💰', '#14b8a6'],
  socket: ['🧦', '#06b6d4'],
  ui: ['🎛️', '#64748b'],
  qr: ['📱', '#0ea5e9'],
  tabs: ['📑', '#84cc16'],
};

// ═══════════════════════════════════════════════════════════════════════════════
// KidLisp Syntax Highlighting for Console
// ═══════════════════════════════════════════════════════════════════════════════

// KidLisp drawing and control keywords
const KIDLISP_KEYWORDS = {
  drawing: ['line', 'rect', 'circle', 'box', 'tri', 'plot', 'write', 'ink', 'paper', 'brush', 'flood', 'wipe', 'paste', 'stamp', 'scroll', 'zoom', 'contrast', 'blur', 'spin', 'jump', 'draw', 'wiggle', 'pan', 'grid', 'noise', 'shape'],
  control: ['if', 'when', 'unless', 'each', 'loop', 'later', 'once', 'def', 'repeat', 'tap', 'now', 'frame'],
  special: ['rainbow', 'zebra', 'fade', 'width', 'height', 'kidlisp'],
};

// Named colors for highlighting
const CSS_COLORS = {
  red: '#ef4444', blue: '#3b82f6', green: '#22c55e', yellow: '#eab308', orange: '#f97316',
  purple: '#a855f7', pink: '#ec4899', cyan: '#06b6d4', white: '#f8fafc', black: '#0f172a',
  gray: '#6b7280', grey: '#6b7280', magenta: '#d946ef', lime: '#84cc16', teal: '#14b8a6',
  navy: '#1e3a8a', maroon: '#7f1d1d', olive: '#84cc16', aqua: '#06b6d4', silver: '#cbd5e1',
  gold: '#fbbf24', coral: '#fb7185', salmon: '#fca5a5', violet: '#8b5cf6', indigo: '#6366f1',
  crimson: '#dc2626', turquoise: '#2dd4bf', chocolate: '#92400e', tan: '#d4a574',
};

// Simple tokenizer for KidLisp
function tokenizeKidLisp(code) {
  const tokens = [];
  let current = '';
  let inString = false;
  let inComment = false;
  
  for (let i = 0; i < code.length; i++) {
    const char = code[i];
    const next = code[i + 1];
    
    // Handle comments
    if (!inString && char === ';' && next === ';') {
      if (current) { tokens.push(current); current = ''; }
      let comment = '';
      while (i < code.length && code[i] !== '\n') {
        comment += code[i++];
      }
      tokens.push(comment);
      i--; // Back up for newline
      continue;
    }
    
    // Handle strings
    if (char === '"') {
      if (inString) {
        current += char;
        tokens.push(current);
        current = '';
        inString = false;
      } else {
        if (current) { tokens.push(current); current = ''; }
        current = char;
        inString = true;
      }
      continue;
    }
    
    if (inString) {
      current += char;
      continue;
    }
    
    // Handle parens and whitespace
    if (char === '(' || char === ')') {
      if (current) { tokens.push(current); current = ''; }
      tokens.push(char);
      continue;
    }
    
    if (/\s/.test(char)) {
      if (current) { tokens.push(current); current = ''; }
      tokens.push(char);
      continue;
    }
    
    current += char;
  }
  
  if (current) tokens.push(current);
  return tokens;
}

// Format KidLisp code for styled console output
function formatKidLispForConsole(code, indent = '   ') {
  if (!code || typeof code !== 'string') return { text: '', styles: [] };
  
  const tokens = tokenizeKidLisp(code);
  const baseStyle = 'font-family: monospace; font-size: 10px;';
  
  let text = indent;
  const styles = [];
  
  for (const token of tokens) {
    // Whitespace - preserve as-is
    if (/^\s+$/.test(token)) {
      text += token.replace(/\n/g, '\n' + indent);
      continue;
    }
    
    // Comments
    if (token.startsWith(';;')) {
      text += `%c${token}`;
      styles.push(`${baseStyle} color: #6b7280; font-style: italic;`);
      continue;
    }
    
    // Strings
    if (token.startsWith('"') && token.endsWith('"')) {
      text += `%c${token}`;
      styles.push(`${baseStyle} color: #f472b6;`); // Pink for strings
      continue;
    }
    
    // Numbers (including time like 0.5s)
    if (/^-?\d+(?:\.\d+)?s?$/.test(token)) {
      text += `%c${token}`;
      styles.push(`${baseStyle} color: #4ade80; font-weight: bold;`); // Green for numbers
      continue;
    }
    
    // Parens
    if (token === '(' || token === ')') {
      text += `%c${token}`;
      styles.push(`${baseStyle} color: #9ca3af;`);
      continue;
    }
    
    // $codes (shortcodes)
    if (token.startsWith('$')) {
      text += `%c$%c${token.slice(1)}`;
      styles.push(`${baseStyle} color: #fb923c; font-weight: bold;`); // Orange $
      styles.push(`${baseStyle} color: #fbbf24; font-weight: bold;`); // Yellow identifier
      continue;
    }
    
    // #codes (hashtags)
    if (token.startsWith('#')) {
      text += `%c#%c${token.slice(1)}`;
      styles.push(`${baseStyle} color: #818cf8; font-weight: bold;`); // Purple #
      styles.push(`${baseStyle} color: #a5b4fc; font-weight: bold;`); // Light purple identifier
      continue;
    }
    
    const lower = token.toLowerCase();
    
    // Drawing commands
    if (KIDLISP_KEYWORDS.drawing.includes(lower)) {
      text += `%c${token}`;
      styles.push(`${baseStyle} color: #22d3ee; font-weight: bold;`); // Cyan for drawing
      continue;
    }
    
    // Control flow
    if (KIDLISP_KEYWORDS.control.includes(lower)) {
      text += `%c${token}`;
      styles.push(`${baseStyle} color: #c084fc; font-weight: bold;`); // Purple for control
      continue;
    }
    
    // Special keywords
    if (KIDLISP_KEYWORDS.special.includes(lower)) {
      text += `%c${token}`;
      styles.push(`${baseStyle} color: #f472b6; font-weight: bold;`); // Pink for special
      continue;
    }
    
    // Named colors
    if (CSS_COLORS[lower]) {
      text += `%c${token}`;
      styles.push(`${baseStyle} color: ${CSS_COLORS[lower]}; font-weight: bold;`);
      continue;
    }
    
    // Default - gray text
    text += `%c${token}`;
    styles.push(`${baseStyle} color: #9ca3af;`);
  }
  
  return { text, styles };
}

// Create styled log functions
function createLogger(category) {
  const [emoji, color] = badges[category] || ['📌', '#888'];
  
  return {
    log: (...args) => {
      if (currentLevel < LOG_LEVELS.INFO || !CATEGORIES[category]) return;
      console.log(
        `%c${emoji} ${category}`,
        `background: ${color}; color: white; padding: 2px 6px; border-radius: 3px; font-size: 10px; font-weight: bold;`,
        ...args
      );
    },
    
    debug: (...args) => {
      if (currentLevel < LOG_LEVELS.DEBUG || !CATEGORIES[category]) return;
      console.log(
        `%c${emoji} ${category}`,
        `background: ${color}22; color: ${color}; padding: 2px 6px; border-radius: 3px; font-size: 10px; border: 1px solid ${color};`,
        ...args
      );
    },
    
    verbose: (...args) => {
      if (currentLevel < LOG_LEVELS.VERBOSE || !CATEGORIES[category]) return;
      console.log(
        `%c  ${emoji}`,
        `color: ${color}; font-size: 9px; opacity: 0.6;`,
        ...args
      );
    },
    
    warn: (...args) => {
      if (currentLevel < LOG_LEVELS.WARN) return;
      console.warn(
        `%c⚠️ ${category}`,
        `background: #f59e0b; color: white; padding: 2px 6px; border-radius: 3px; font-size: 10px; font-weight: bold;`,
        ...args
      );
    },
    
    error: (...args) => {
      if (currentLevel < LOG_LEVELS.ERROR) return;
      console.error(
        `%c❌ ${category}`,
        `background: #ef4444; color: white; padding: 2px 6px; border-radius: 3px; font-size: 10px; font-weight: bold;`,
        ...args
      );
    },
    
    success: (...args) => {
      if (currentLevel < LOG_LEVELS.INFO || !CATEGORIES[category]) return;
      console.log(
        `%c✅ ${category}`,
        `background: #22c55e; color: white; padding: 2px 6px; border-radius: 3px; font-size: 10px; font-weight: bold;`,
        ...args
      );
    },
    
    group: (label) => {
      if (currentLevel < LOG_LEVELS.DEBUG || !CATEGORIES[category]) return;
      console.groupCollapsed(
        `%c${emoji} ${category}%c ${label}`,
        `background: ${color}; color: white; padding: 2px 6px; border-radius: 3px; font-size: 10px;`,
        `color: ${color}; font-weight: normal; font-size: 11px;`
      );
    },
    
    groupEnd: () => {
      if (currentLevel < LOG_LEVELS.DEBUG || !CATEGORIES[category]) return;
      console.groupEnd();
    }
  };
}

// Create loggers for each category
export const log = {
  boot: createLogger('boot'),
  splash: createLogger('splash'),
  platform: createLogger('platform'),
  code: createLogger('code'),
  editor: createLogger('editor'),
  auth: createLogger('auth'),
  playback: createLogger('playback'),
  iframe: createLogger('iframe'),
  message: createLogger('message'),
  wallet: createLogger('wallet'),
  socket: createLogger('socket'),
  ui: createLogger('ui'),
  qr: createLogger('qr'),
  tabs: createLogger('tabs'),
};

// Beautiful multicolored header (call once on page load)
export function showHeader() {
  const colors = getDayColorScheme();
  const title = 'KidLisp.com';
  
  // Build the multicolored title - each character gets its own color
  let formatStr = '';
  let styles = [];
  
  title.split('').forEach((char, i) => {
    formatStr += `%c${char}`;
    const color = colors.title[i % colors.title.length];
    styles.push(`color: ${color}; font-weight: bold; font-size: 22px; font-family: 'Comic Sans MS', cursive, sans-serif; text-shadow: 1px 1px 2px rgba(0,0,0,0.3);`);
  });
  
  console.log(formatStr, ...styles);
  
  // Tagline
  console.log(
    '%c✨ friendly coding for everyone 🌈',
    `color: ${colors.tagline}; font-size: 11px; font-style: italic; padding-left: 4px;`
  );
  
  // Separator line
  console.log(
    '%c━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━',
    `color: ${colors.accent}44; font-size: 10px;`
  );
  
  // Day info & help hint
  const dayNames = ['Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'];
  const today = dayNames[new Date().getDay()];
  console.log(
    `%c🗓️ ${today}'s theme • %ckidlispDebug.help()%c for options`,
    'color: #666; font-size: 10px;',
    `color: ${colors.accent}; font-size: 10px; font-family: monospace;`,
    'color: #666; font-size: 10px;'
  );
  
  console.log(''); // blank line
}

// Log a platform switch with nice formatting
export function logPlatformSwitch(fromPlatform, toPlatform, platformInfo = {}) {
  if (currentLevel < LOG_LEVELS.INFO || !CATEGORIES.platform) return;
  
  const colors = getDayColorScheme();
  const icons = {
    'ac': '🎨',
    'ff1': '🎹',
    'ableton': '🎛️',
    'playdate': '🎮',
    'gameboy': '👾',
    'n64': '🕹️'
  };
  
  const fromIcon = icons[fromPlatform] || '❓';
  const toIcon = icons[toPlatform] || '❓';
  
  console.log(
    `%c🎮 platform%c ${fromIcon} ${fromPlatform || 'none'} → ${toIcon} ${toPlatform}`,
    `background: #f43f5e; color: white; padding: 2px 6px; border-radius: 3px; font-size: 10px; font-weight: bold;`,
    `color: ${colors.accent}; font-size: 11px; font-weight: bold; padding-left: 4px;`
  );
  
  if (platformInfo?.comingSoon) {
    console.log('%c   ⏳ Coming soon...', 'color: #f59e0b; font-size: 10px;');
  }
  if (platformInfo?.experimental) {
    console.log('%c   🧪 Experimental', 'color: #8b5cf6; font-size: 10px;');
  }
}

// Log code operations with syntax-highlighted preview
export function logCodeOperation(operation, code, shortCode) {
  if (currentLevel < LOG_LEVELS.INFO || !CATEGORIES.code) return;
  
  const colors = getDayColorScheme();
  const ops = {
    'load': ['📥', '#3b82f6', 'Loaded'],
    'save': ['💾', '#22c55e', 'Saved'],
    'run': ['▶️', '#22c55e', 'Running'],
    'create': ['✨', '#a855f7', 'Created'],
    'fetch': ['🌐', '#0ea5e9', 'Fetched'],
  };
  
  const [icon, color, label] = ops[operation] || ['📄', '#666', operation];
  
  console.log(
    `%c${icon} code%c ${label}${shortCode ? ` $${shortCode}` : ''}`,
    `background: ${color}; color: white; padding: 2px 6px; border-radius: 3px; font-size: 10px; font-weight: bold;`,
    `color: ${colors.accent}; font-size: 11px; padding-left: 4px;`
  );
  
  // Show syntax-highlighted preview (first line or first 60 chars)
  if (code) {
    const firstLine = code.split('\n')[0] || '';
    const preview = firstLine.length > 60 ? firstLine.slice(0, 60) : firstLine;
    const hasMore = code.includes('\n') || firstLine.length > 60;
    
    const highlighted = formatKidLispForConsole(preview + (hasMore ? '…' : ''));
    if (highlighted.text) {
      console.log(highlighted.text, ...highlighted.styles);
    }
  }
}

// Log editor initialization
export function logEditorInit(editorType, options = {}) {
  if (currentLevel < LOG_LEVELS.INFO || !CATEGORIES.editor) return;
  
  const colors = getDayColorScheme();
  
  console.log(
    `%c📝 editor%c ${editorType} initialized`,
    `background: #f59e0b; color: white; padding: 2px 6px; border-radius: 3px; font-size: 10px; font-weight: bold;`,
    `color: ${colors.accent}; font-size: 11px; padding-left: 4px;`
  );
  
  if (options.theme) {
    console.log(`%c   🎨 Theme: ${options.theme}`, 'color: #666; font-size: 10px;');
  }
  if (options.language) {
    console.log(`%c   📜 Language: ${options.language}`, 'color: #666; font-size: 10px;');
  }
}

// Log auth events
export function logAuth(event, user) {
  if (currentLevel < LOG_LEVELS.INFO || !CATEGORIES.auth) return;
  
  const events = {
    'login': ['🔓', '#22c55e', 'Logged in'],
    'logout': ['🔒', '#f59e0b', 'Logged out'],
    'restore': ['🔄', '#3b82f6', 'Session restored'],
    'error': ['❌', '#ef4444', 'Auth error'],
    'check': ['🔍', '#6366f1', 'Checking auth'],
  };
  
  const [icon, color, label] = events[event] || ['🔐', '#8b5cf6', event];
  
  console.log(
    `%c${icon} auth%c ${label}${user ? ` @${user}` : ''}`,
    `background: ${color}; color: white; padding: 2px 6px; border-radius: 3px; font-size: 10px; font-weight: bold;`,
    `color: #8b5cf6; font-size: 11px; font-weight: bold; padding-left: 4px;`
  );
}

// Log boot sequence steps
export function logBoot(step, details = '') {
  if (currentLevel < LOG_LEVELS.INFO || !CATEGORIES.boot) return;
  
  const colors = getDayColorScheme();
  const steps = {
    'start': ['🚀', 'Starting'],
    'config': ['⚙️', 'Config loaded'],
    'i18n': ['🌐', 'Translations loaded'],
    'auth': ['🔐', 'Auth initialized'],
    'editor': ['📝', 'Editor ready'],
    'iframe': ['🖼️', 'Iframe loaded'],
    'complete': ['✅', 'Boot complete'],
  };
  
  const [icon, label] = steps[step] || ['📌', step];
  
  console.log(
    `%c${icon} boot%c ${label}${details ? ` • ${details}` : ''}`,
    `background: ${colors.accent}; color: white; padding: 2px 6px; border-radius: 3px; font-size: 10px; font-weight: bold;`,
    `color: ${colors.primary}; font-size: 11px; padding-left: 4px;`
  );
}

// Global debug controls (attach to window for console access)
export const debug = {
  // Set log level
  level: (level) => {
    if (typeof level === 'string') {
      level = LOG_LEVELS[level.toUpperCase()];
    }
    if (level !== undefined) {
      currentLevel = level;
      const levelName = Object.keys(LOG_LEVELS).find(k => LOG_LEVELS[k] === level);
      localStorage.setItem('kidlisp-log-level', levelName);
      console.log(`%c📊 Log level: ${levelName}`, `color: ${dayColors.accent}; font-weight: bold;`);
    }
    return currentLevel;
  },
  
  // Quick presets
  quiet: () => { debug.level(LOG_LEVELS.WARN); console.log('%c🤫 Quiet mode - warnings & errors only', 'color: #666;'); },
  normal: () => { debug.level(LOG_LEVELS.INFO); console.log('%c📢 Normal mode', 'color: #666;'); },
  verbose: () => { debug.level(LOG_LEVELS.VERBOSE); console.log('%c📣 Verbose mode - everything!', 'color: #666;'); },
  
  // Toggle category
  toggle: (category) => {
    if (category && CATEGORIES[category] !== undefined) {
      CATEGORIES[category] = !CATEGORIES[category];
      localStorage.setItem('kidlisp-log-categories', JSON.stringify(CATEGORIES));
      const status = CATEGORIES[category] ? '✅ enabled' : '❌ disabled';
      const [emoji] = badges[category] || ['📌'];
      console.log(`%c${emoji} ${category}: ${status}`, `color: ${dayColors.accent};`);
    } else {
      console.log('%c📋 Available categories:', 'color: #888; font-weight: bold;');
      Object.entries(CATEGORIES).forEach(([k, v]) => {
        const [emoji] = badges[k] || ['📌'];
        console.log(`  ${emoji} ${k}: ${v ? '✅' : '❌'}`);
      });
    }
  },
  
  // Enable all categories
  all: () => {
    Object.keys(CATEGORIES).forEach(k => CATEGORIES[k] = true);
    localStorage.setItem('kidlisp-log-categories', JSON.stringify(CATEGORIES));
    debug.level(LOG_LEVELS.VERBOSE);
    console.log('%c🔊 All logging enabled!', `color: ${dayColors.accent}; font-weight: bold;`);
  },
  
  // Minimal logging (only important stuff)
  minimal: () => {
    Object.keys(CATEGORIES).forEach(k => CATEGORIES[k] = false);
    CATEGORIES.boot = true;
    CATEGORIES.code = true;
    CATEGORIES.auth = true;
    localStorage.setItem('kidlisp-log-categories', JSON.stringify(CATEGORIES));
    debug.level(LOG_LEVELS.INFO);
    console.log('%c🤫 Minimal logging - just the essentials', `color: ${dayColors.accent}; font-weight: bold;`);
  },
  
  // Show help
  help: () => {
    const colors = getDayColorScheme();
    console.log('%c🛠️ KidLisp Debug Commands', `color: ${colors.accent}; font-weight: bold; font-size: 13px;`);
    console.log('%c━━━━━━━━━━━━━━━━━━━━━━━━━━', `color: ${colors.accent}44;`);
    console.log('%ckidlispDebug.quiet()%c    - Warnings & errors only', 'color: #f59e0b; font-family: monospace;', 'color: #666;');
    console.log('%ckidlispDebug.normal()%c   - Standard logging', 'color: #22c55e; font-family: monospace;', 'color: #666;');
    console.log('%ckidlispDebug.verbose()%c  - Everything!', 'color: #3b82f6; font-family: monospace;', 'color: #666;');
    console.log('%ckidlispDebug.minimal()%c  - Just the essentials', 'color: #8b5cf6; font-family: monospace;', 'color: #666;');
    console.log('%ckidlispDebug.all()%c      - Enable all categories', 'color: #ec4899; font-family: monospace;', 'color: #666;');
    console.log('%ckidlispDebug.toggle(cat)%c- Toggle a category', 'color: #14b8a6; font-family: monospace;', 'color: #666;');
    console.log('%ckidlispDebug.toggle()%c   - Show all categories', 'color: #0ea5e9; font-family: monospace;', 'color: #666;');
    console.log('%ckidlispDebug.status()%c   - Show current settings', 'color: #f43f5e; font-family: monospace;', 'color: #666;');
  },
  
  // Show current settings
  status: () => {
    const colors = getDayColorScheme();
    const levelName = Object.keys(LOG_LEVELS).find(k => LOG_LEVELS[k] === currentLevel);
    console.log('%c📊 KidLisp Logger Status', `color: ${colors.accent}; font-weight: bold;`);
    console.log(`%c   Level: ${levelName}`, 'color: #666;');
    console.log('%c   Categories:', 'color: #666;');
    Object.entries(CATEGORIES).forEach(([k, v]) => {
      const [emoji] = badges[k] || ['📌'];
      console.log(`     ${emoji} ${k}: ${v ? '✅' : '❌'}`);
    });
  }
};

// Attach to window for console access
if (typeof window !== 'undefined') {
  window.kidlispDebug = debug;
  window.kidlispLog = log;
}
