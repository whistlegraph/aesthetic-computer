// Teia Mode, 25.09.22
// Shared module for TEIA mode detection across all modules

let TEIA_MODE = false; // Will be set during initialization

// Export function to access TEIA_MODE
export const getTeiaMode = () => TEIA_MODE;

// Export function to set TEIA_MODE (called from disk.mjs during init)
export const setTeiaMode = (value) => {
  TEIA_MODE = value;
};

// Export function to check if we're in TEIA mode with fallback detection
export const checkTeiaMode = () => {
  // Primary source is the explicitly set TEIA_MODE
  if (TEIA_MODE) return true;
  
  // Fallback detection methods for edge cases
  try {
    // Check window object if available (main thread)
    if (typeof window !== 'undefined' && window.acTEIA_MODE) return true;
    
    // Check globalThis if available
    if (typeof globalThis !== 'undefined' && globalThis.acTEIA_MODE) return true;
    
    // Check for Teia-specific patterns as last resort
    if (typeof window !== 'undefined' && window.location) {
      const urlHasTeia = window.location.hostname === 'localhost';
      const titleHasTeia = typeof document !== 'undefined' && 
                          document.title && 
                          document.title.startsWith('$');
      if (urlHasTeia && titleHasTeia) return true;
    }
  } catch (e) {
    // Ignore errors in worker context where window/document aren't available
  }
  
  return false;
};