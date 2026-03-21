// Logs
// Flags for tuning console logs by turning on or off groups in development
// (and production).

// Log levels: 0=NONE, 1=ERROR, 2=WARN, 3=INFO, 4=DEBUG, 5=VERBOSE
const LOG_LEVELS = { NONE: 0, ERROR: 1, WARN: 2, INFO: 3, DEBUG: 4, VERBOSE: 5 };

// Category colors - styled console output with background colors
const CATEGORY_STYLES = {
  boot:     { badge: '🥾', color: '#22c55e', label: 'boot' },
  disk:     { badge: '💿', color: '#8b5cf6', label: 'disk' },
  piece:    { badge: '🧩', color: '#f59e0b', label: 'piece' },
  wallet:   { badge: '🔷', color: '#3b82f6', label: 'wallet' },
  auth:     { badge: '🔐', color: '#ec4899', label: 'auth' },
  socket:   { badge: '🧦', color: '#06b6d4', label: 'socket' },
  audio:    { badge: '🔊', color: '#f97316', label: 'audio' },
  gpu:      { badge: '🎨', color: '#a855f7', label: 'gpu' },
  lisp:     { badge: '🌿', color: '#10b981', label: 'lisp' },
  store:    { badge: '💾', color: '#6366f1', label: 'store' },
  tape:     { badge: '🎬', color: '#ef4444', label: 'tape' },
  hid:      { badge: '🖱️', color: '#84cc16', label: 'hid' },
  net:      { badge: '🌐', color: '#0ea5e9', label: 'net' },
  qr:       { badge: '🔍', color: '#d946ef', label: 'qr' },
};

// Default category states - most off by default for clean console
const categoryLevels = {
  boot: LOG_LEVELS.NONE,      // Disabled - too noisy
  disk: LOG_LEVELS.NONE,      // Disabled
  piece: LOG_LEVELS.INFO,     // Enabled - piece loading is useful
  wallet: LOG_LEVELS.NONE,    // Disabled - too noisy
  auth: LOG_LEVELS.INFO,      // Enabled - login status is useful
  socket: LOG_LEVELS.NONE,    // Disabled - too noisy
  audio: LOG_LEVELS.NONE,     // Disabled
  gpu: LOG_LEVELS.NONE,       // Disabled - too noisy
  lisp: LOG_LEVELS.INFO,      // Enabled - KidLisp init is useful
  store: LOG_LEVELS.INFO,     // Enabled - code storage is useful
  tape: LOG_LEVELS.INFO,      // Enabled - recording status
  hid: LOG_LEVELS.NONE,       // Disabled
  net: LOG_LEVELS.NONE,       // Disabled
  qr: LOG_LEVELS.NONE,        // Disabled
};

// Create a styled logger for a category with background colors like KidLisp.com
function createCategoryLogger(category) {
  const style = CATEGORY_STYLES[category] || { badge: '📋', color: '#888', label: category };
  const getLevel = () => categoryLevels[category] ?? LOG_LEVELS.INFO;
  
  // Styled badge with background color
  const badgeStyle = `background: ${style.color}; color: white; padding: 2px 6px; border-radius: 3px; font-size: 10px; font-weight: bold;`;
  const debugStyle = `background: ${style.color}22; color: ${style.color}; padding: 2px 6px; border-radius: 3px; font-size: 10px; border: 1px solid ${style.color};`;
  const successStyle = `background: #22c55e; color: white; padding: 2px 6px; border-radius: 3px; font-size: 10px; font-weight: bold;`;
  const warnStyle = `background: #f59e0b; color: white; padding: 2px 6px; border-radius: 3px; font-size: 10px; font-weight: bold;`;
  const errorStyle = `background: #ef4444; color: white; padding: 2px 6px; border-radius: 3px; font-size: 10px; font-weight: bold;`;
  
  return {
    log: (...args) => {
      if (getLevel() >= LOG_LEVELS.INFO) {
        console.log(`%c${style.badge} ${style.label}`, badgeStyle, ...args);
      }
    },
    debug: (...args) => {
      if (getLevel() >= LOG_LEVELS.DEBUG) {
        console.log(`%c${style.badge} ${style.label}`, debugStyle, ...args);
      }
    },
    verbose: (...args) => {
      if (getLevel() >= LOG_LEVELS.VERBOSE) {
        console.log(`%c  ${style.badge}`, `color: ${style.color}; font-size: 9px; opacity: 0.6;`, ...args);
      }
    },
    warn: (...args) => {
      if (getLevel() >= LOG_LEVELS.WARN) {
        console.warn(`%c⚠️ ${style.label}`, warnStyle, ...args);
      }
    },
    error: (...args) => {
      if (getLevel() >= LOG_LEVELS.ERROR) {
        console.error(`%c❌ ${style.label}`, errorStyle, ...args);
      }
    },
    success: (...args) => {
      if (getLevel() >= LOG_LEVELS.INFO) {
        console.log(`%c✅ ${style.label}`, successStyle, ...args);
      }
    },
  };
}

// Export styled loggers
export const log = {
  boot: createCategoryLogger('boot'),
  disk: createCategoryLogger('disk'),
  piece: createCategoryLogger('piece'),
  wallet: createCategoryLogger('wallet'),
  auth: createCategoryLogger('auth'),
  socket: createCategoryLogger('socket'),
  audio: createCategoryLogger('audio'),
  gpu: createCategoryLogger('gpu'),
  lisp: createCategoryLogger('lisp'),
  store: createCategoryLogger('store'),
  tape: createCategoryLogger('tape'),
  hid: createCategoryLogger('hid'),
  net: createCategoryLogger('net'),
  qr: createCategoryLogger('qr'),
};

// Debug controls (available in browser console as window.acDebug)
export const debug = {
  quiet: () => { Object.keys(categoryLevels).forEach(k => categoryLevels[k] = LOG_LEVELS.ERROR); console.log('🔇 AC logs: quiet mode'); },
  normal: () => { Object.keys(categoryLevels).forEach(k => categoryLevels[k] = LOG_LEVELS.INFO); console.log('🔊 AC logs: normal mode'); },
  verbose: () => { Object.keys(categoryLevels).forEach(k => categoryLevels[k] = LOG_LEVELS.VERBOSE); console.log('📢 AC logs: verbose mode'); },
  set: (category, level) => {
    if (categoryLevels[category] !== undefined && LOG_LEVELS[level] !== undefined) {
      categoryLevels[category] = LOG_LEVELS[level];
      console.log(`📋 ${category}: ${level}`);
    }
  },
  status: () => console.table(Object.fromEntries(Object.entries(categoryLevels).map(([k, v]) => [k, Object.keys(LOG_LEVELS).find(l => LOG_LEVELS[l] === v)]))),
  help: () => console.log(`
🎮 AC Debug Commands:
  acDebug.quiet()     - Errors only
  acDebug.normal()    - Standard logging  
  acDebug.verbose()   - All logs
  acDebug.set(cat, level) - Set category level (NONE/ERROR/WARN/INFO/DEBUG/VERBOSE)
  acDebug.status()    - Show current levels
  
Categories: ${Object.keys(CATEGORY_STYLES).join(', ')}
`),
};

// Make debug controls available globally in browser
if (typeof window !== 'undefined') {
  window.acDebug = debug;
}

// Legacy export - boolean flags for backward compatibility
export const logs = {
  store: false,
  frame: false,
  loading: false,
  session: false,
  udp: false,
  download: false,
  audio: false,
  hid: false,
  painting: false,
  glaze: false,
  deps: false,
  messaging: false,
  chat: false,
  history: false,
  recorder: false,
};