/**
 * 🎨 Aesthetic Computer Color Schemes
 * 
 * Centralized color definitions derived from the VS Code themes.
 * Use these across the extension (Welcome Architecture, webviews, etc.)
 * 
 * Colors are organized by semantic purpose, not implementation.
 */

export const schemes = {
  dark: {
    // Core backgrounds
    background: '#181318',
    backgroundAlt: '#141214',
    backgroundDeep: '#101010',
    
    // Core foregrounds
    foreground: '#ffffffcc',
    foregroundBright: '#ffffff',
    foregroundDim: '#ffffff80',
    foregroundMuted: '#555555',
    
    // Accent colors (mauve/pink)
    accent: '#a87090',
    accentBright: '#ff69b4',
    accentDim: '#584058',
    
    // Status colors
    statusOnline: '#0f0',
    statusOffline: '#ff69b4',
    
    // Semantic colors
    success: '#70c070',
    warning: '#d0c070',
    error: '#e07070',
    info: '#70b0b0',
    
    // Category colors (for process visualization)
    categories: {
      editor: 0xb06bff,
      tui: 0xff69b4,
      bridge: 0x6bff9f,
      db: 0xffeb6b,
      proxy: 0x6b9fff,
      ai: 0xff9f6b,
      shell: 0x6bffff,
      dev: 0x6bff9f,
      ide: 0x6b9fff,
      lsp: 0x888888,
      kernel: 0x88ccff
    },
    
    // Three.js specific
    three: {
      sceneBackground: 0x000000,
      kernelOuter: 0x4488ff,
      kernelRing: 0x66aaff,
      kernelCore: 0x88ccff,
      connectionLine: 0x444444,
      connectionActive: 0xff69b4,
      deadProcess: 0x444444
    },
    
    // UI elements
    ui: {
      border: '#483848',
      shadow: 'rgba(0, 0, 0, 0.6)',
      overlay: 'rgba(0, 0, 0, 0.85)',
      devBanner: '#ff69b4',
      devBannerText: '#000000'
    }
  },
  
  light: {
    // Core backgrounds
    background: '#fcf7c5',
    backgroundAlt: '#f5f0c0',
    backgroundDeep: '#e8e3b0',
    
    // Core foregrounds
    foreground: '#281e5a',
    foregroundBright: '#281e5a',
    foregroundDim: '#281e5a80',
    foregroundMuted: '#806060',
    
    // Accent colors (blue/green)
    accent: '#387adf',
    accentBright: '#006400',
    accentDim: '#387adf60',
    
    // Status colors
    statusOnline: '#006400',
    statusOffline: '#387adf',
    
    // Semantic colors
    success: '#006400',
    warning: '#806000',
    error: '#c00000',
    info: '#008080',
    
    // Category colors (for process visualization - adjusted for light mode)
    categories: {
      editor: 0x8040d0,
      tui: 0xd04080,
      bridge: 0x208040,
      db: 0xa08000,
      proxy: 0x2060c0,
      ai: 0xc06020,
      shell: 0x008080,
      dev: 0x208040,
      ide: 0x2060c0,
      lsp: 0x606060,
      kernel: 0x387adf
    },
    
    // Three.js specific
    three: {
      sceneBackground: 0xfcf7c5,
      kernelOuter: 0x387adf,
      kernelRing: 0x006400,
      kernelCore: 0x387adf,
      connectionLine: 0xa8a080,
      connectionActive: 0x006400,
      deadProcess: 0xa8a080
    },
    
    // UI elements
    ui: {
      border: '#387adf',
      shadow: 'rgba(0, 0, 0, 0.2)',
      overlay: 'rgba(252, 247, 197, 0.95)',
      devBanner: '#006400',
      devBannerText: '#ffffff'
    }
  }
};

// Helper to get scheme by name
export function getScheme(name) {
  return schemes[name] || schemes.dark;
}

// Helper to detect VS Code theme from CSS variables (for webviews)
export function detectTheme() {
  if (typeof document !== 'undefined') {
    const body = document.body;
    const bgColor = getComputedStyle(body).getPropertyValue('--vscode-editor-background').trim();
    // Simple heuristic: if background is bright, use light theme
    if (bgColor) {
      const r = parseInt(bgColor.slice(1, 3), 16);
      const g = parseInt(bgColor.slice(3, 5), 16);
      const b = parseInt(bgColor.slice(5, 7), 16);
      const luminance = (0.299 * r + 0.587 * g + 0.114 * b) / 255;
      return luminance > 0.5 ? 'light' : 'dark';
    }
  }
  return 'dark';
}

// Export as global for non-module contexts (inline scripts)
if (typeof window !== 'undefined') {
  window.AestheticColorSchemes = { schemes, getScheme, detectTheme };
}

// Default export
export default schemes;
