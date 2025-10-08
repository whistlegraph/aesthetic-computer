// PACK Mode, 25.09.22
// Shared module for PACK mode detection across all modules

let PACK_MODE = false; // Will be set during initialization

// Export function to access PACK_MODE
export const getPackMode = () => PACK_MODE;

// Export function to set PACK_MODE (called from disk.mjs during init)
export const setPackMode = (value) => {
  PACK_MODE = value;
};

// Export function to check if we're in PACK mode with fallback detection
export const checkPackMode = () => {
  // Primary source is the explicitly set PACK_MODE
  if (PACK_MODE) return true;
  
  // Fallback detection methods for edge cases
  try {
    // Check window object if available (main thread)
    if (typeof window !== 'undefined' && window.acPACK_MODE) return true;
    
    // Check globalThis if available
    if (typeof globalThis !== 'undefined' && globalThis.acPACK_MODE) return true;
    
    // Check for PACK-specific patterns as last resort
    if (typeof window !== 'undefined' && window.location) {
      const urlHasPackIndicator = window.location.hostname === 'localhost';
      const titleHasPackIndicator = typeof document !== 'undefined' && 
                          document.title && 
                          document.title.startsWith('$');
      if (urlHasPackIndicator && titleHasPackIndicator) return true;
    }
  } catch (e) {
    // Ignore errors in worker context where window/document aren't available
  }
  
  return false;
};