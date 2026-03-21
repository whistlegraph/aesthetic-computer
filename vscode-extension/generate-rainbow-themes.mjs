import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const themesDir = path.join(__dirname, 'themes');

// Read base dark theme
const baseDarkPath = path.join(themesDir, 'aesthetic-dark-color-theme.json');
const baseDark = JSON.parse(fs.readFileSync(baseDarkPath, 'utf8'));

// Define saturated color palettes for each theme - JOLLY RANCHER STYLE!
const colorSchemes = {
  red: {
    name: 'Red 🔴',
    dark: {
      accent: '#ff6666',
      accentBright: '#ff8888',
      bg: '#cc0000',
      bgDark: '#aa0000',
      bgPanel: '#dd0000',
      bgSidebar: '#bb0000',
      fg: '#ffffff',
      fgDim: '#ffcccc',
      shadow: '#880000'
    },
    light: {
      accent: '#cc0000',
      accentBright: '#dd0000',
      bg: '#ff6666',
      bgDark: '#ff4444',
      bgPanel: '#ff8888',
      bgSidebar: '#ff5555',
      fg: '#ffffff',
      fgDim: '#ffe6e6',
      shadow: '#cc0000'
    }
  },
  orange: {
    name: 'Orange 🟠',
    dark: {
      accent: '#ffaa66',
      accentBright: '#ffbb88',
      bg: '#dd6600',
      bgDark: '#bb5500',
      bgPanel: '#ee7700',
      bgSidebar: '#cc6600',
      fg: '#ffffff',
      fgDim: '#ffd9b3',
      shadow: '#994400'
    },
    light: {
      accent: '#cc5500',
      accentBright: '#dd6600',
      bg: '#ff9944',
      bgDark: '#ff8833',
      bgPanel: '#ffaa66',
      bgSidebar: '#ff8844',
      fg: '#ffffff',
      fgDim: '#fff0e6',
      shadow: '#bb4400'
    }
  },
  yellow: {
    name: 'Yellow 🟡',
    dark: {
      accent: '#ffff66',
      accentBright: '#ffff88',
      bg: '#ccaa00',
      bgDark: '#aa8800',
      bgPanel: '#ddbb00',
      bgSidebar: '#bb9900',
      fg: '#ffffff',
      fgDim: '#ffffcc',
      shadow: '#886600'
    },
    light: {
      accent: '#aa8800',
      accentBright: '#ccaa00',
      bg: '#ffdd44',
      bgDark: '#ffcc22',
      bgPanel: '#ffee66',
      bgSidebar: '#ffcc33',
      fg: '#ffffff',
      fgDim: '#ffffee',
      shadow: '#aa8800'
    }
  },
  green: {
    name: 'Green 🟢',
    dark: {
      accent: '#66ff66',
      accentBright: '#88ff88',
      bg: '#00cc00',
      bgDark: '#00aa00',
      bgPanel: '#00dd00',
      bgSidebar: '#00bb00',
      fg: '#ffffff',
      fgDim: '#ccffcc',
      shadow: '#008800'
    },
    light: {
      accent: '#00aa00',
      accentBright: '#00cc00',
      bg: '#44ff44',
      bgDark: '#22ff22',
      bgPanel: '#66ff66',
      bgSidebar: '#33ff33',
      fg: '#ffffff',
      fgDim: '#e6ffe6',
      shadow: '#00aa00'
    }
  },
  blue: {
    name: 'Blue 🔵',
    dark: {
      accent: '#6699ff',
      accentBright: '#88aaff',
      bg: '#0066dd',
      bgDark: '#0055bb',
      bgPanel: '#0077ee',
      bgSidebar: '#0066cc',
      fg: '#ffffff',
      fgDim: '#cce6ff',
      shadow: '#004499'
    },
    light: {
      accent: '#0055bb',
      accentBright: '#0066dd',
      bg: '#3388ff',
      bgDark: '#2277ff',
      bgPanel: '#5599ff',
      bgSidebar: '#2288ff',
      fg: '#ffffff',
      fgDim: '#e6f2ff',
      shadow: '#0055bb'
    }
  },
  indigo: {
    name: 'Indigo 🟣',
    dark: {
      accent: '#8866ff',
      accentBright: '#9988ff',
      bg: '#5500dd',
      bgDark: '#4400bb',
      bgPanel: '#6600ee',
      bgSidebar: '#5500cc',
      fg: '#ffffff',
      fgDim: '#daccff',
      shadow: '#330099'
    },
    light: {
      accent: '#4400bb',
      accentBright: '#5500dd',
      bg: '#7744ff',
      bgDark: '#6633ff',
      bgPanel: '#8855ff',
      bgSidebar: '#6644ff',
      fg: '#ffffff',
      fgDim: '#ede6ff',
      shadow: '#4400bb'
    }
  },
  violet: {
    name: 'Violet 🔮',
    dark: {
      accent: '#cc66ff',
      accentBright: '#dd88ff',
      bg: '#aa00dd',
      bgDark: '#8800bb',
      bgPanel: '#bb00ee',
      bgSidebar: '#9900cc',
      fg: '#ffffff',
      fgDim: '#eeccff',
      shadow: '#660099'
    },
    light: {
      accent: '#8800bb',
      accentBright: '#aa00dd',
      bg: '#cc44ff',
      bgDark: '#bb22ff',
      bgPanel: '#dd66ff',
      bgSidebar: '#bb33ff',
      fg: '#ffffff',
      fgDim: '#f5e6ff',
      shadow: '#8800bb'
    }
  },
  pink: {
    name: 'Pink 🌸',
    dark: {
      accent: '#ff66cc',
      accentBright: '#ff88dd',
      bg: '#dd0088',
      bgDark: '#bb0077',
      bgPanel: '#ee0099',
      bgSidebar: '#cc0088',
      fg: '#ffffff',
      fgDim: '#ffccee',
      shadow: '#990066'
    },
    light: {
      accent: '#bb0077',
      accentBright: '#dd0088',
      bg: '#ff44aa',
      bgDark: '#ff3399',
      bgPanel: '#ff66bb',
      bgSidebar: '#ff3399',
      fg: '#ffffff',
      fgDim: '#ffe6f5',
      shadow: '#bb0077'
    }
  },
  pencil: {
    name: 'Pencil ✏️',
    dark: {
      accent: '#aaaaaa',
      accentBright: '#cccccc',
      bg: '#444444',
      bgDark: '#333333',
      bgPanel: '#555555',
      bgSidebar: '#3a3a3a',
      fg: '#ffffff',
      fgDim: '#cccccc',
      shadow: '#222222'
    },
    light: {
      accent: '#666666',
      accentBright: '#888888',
      bg: '#bbbbbb',
      bgDark: '#aaaaaa',
      bgPanel: '#cccccc',
      bgSidebar: '#b0b0b0',
      fg: '#ffffff',
      fgDim: '#f0f0f0',
      shadow: '#888888'
    }
  }
};

function createTheme(colorName, scheme, isDark) {
  const type = isDark ? 'dark' : 'light';
  const typeLabel = isDark ? 'Dark' : 'Light';
  const colors = isDark ? scheme.dark : scheme.light;
  
  return {
    "$schema": "vscode://schemas/color-theme",
    "name": `Aesthetic Computer: ${typeLabel} ${scheme.name}`,
    "type": type,
    "colors": {
      "editor.background": colors.bg,
      "editor.foreground": colors.fg,
      "editorCursor.foreground": colors.accentBright,
      "editorCursor.background": colors.bg,
      "editor.lineHighlightBackground": colors.accent + "18",
      "editor.selectionBackground": colors.accent + "40",
      "editor.selectionHighlightBackground": colors.accent + "25",
      "editor.wordHighlightBackground": colors.accent + "25",
      "editor.findMatchBackground": colors.accent + "50",
      "editor.findMatchHighlightBackground": colors.accent + "30",
      "editorLineNumber.foreground": colors.fgDim + "60",
      "editorLineNumber.activeForeground": colors.accent,
      "editorIndentGuide.background1": colors.fgDim + "30",
      "editorIndentGuide.activeBackground1": colors.fgDim + "60",
      "editorBracketMatch.background": colors.accent + "30",
      "editorBracketMatch.border": colors.accent,
      
      "activityBar.background": colors.bgDark,
      "activityBar.foreground": colors.fg,
      "activityBar.activeBorder": colors.accent,
      "activityBarBadge.background": colors.accent,
      "activityBarBadge.foreground": isDark ? "#ffffff" : colors.bg,
      
      "sideBar.background": colors.bgSidebar,
      "sideBar.foreground": colors.fg,
      "sideBarTitle.foreground": colors.fg,
      "sideBarSectionHeader.background": colors.bgDark,
      "sideBarSectionHeader.foreground": colors.fgDim,
      
      "list.activeSelectionBackground": colors.accent + "40",
      "list.activeSelectionForeground": colors.fg,
      "list.hoverBackground": colors.accent + "20",
      "list.focusBackground": colors.accent + "30",
      
      "statusBar.background": colors.accent,
      "statusBar.foreground": isDark ? "#ffffff" : colors.bg,
      "statusBar.debuggingBackground": "#ff6400",
      "statusBar.noFolderBackground": colors.bgPanel,
      "statusBarItem.hoverBackground": colors.accentBright + "40",
      
      "titleBar.activeBackground": colors.bgDark,
      "titleBar.activeForeground": colors.fg,
      "titleBar.inactiveBackground": colors.bgSidebar,
      "titleBar.inactiveForeground": colors.fgDim + "80",
      
      "tab.activeBackground": colors.bg,
      "tab.activeForeground": colors.fg,
      "tab.inactiveBackground": colors.bgSidebar,
      "tab.inactiveForeground": colors.fgDim + "80",
      "tab.activeBorderTop": colors.accent,
      "tab.border": colors.bgDark,
      "editorGroupHeader.tabsBackground": colors.bgDark,
      
      "panel.background": colors.bgSidebar,
      "panel.border": colors.accent + "60",
      "panelTitle.activeForeground": colors.fg,
      "panelTitle.activeBorder": colors.accent,
      "panelTitle.inactiveForeground": colors.fgDim + "80",
      
      "terminal.background": colors.bg,
      "terminal.foreground": colors.fg,
      "terminalCursor.foreground": colors.accentBright,
      "terminal.ansiBlack": colors.bgDark,
      "terminal.ansiRed": isDark ? "#ff6666" : "#cc0000",
      "terminal.ansiGreen": isDark ? "#66ff66" : "#00aa00",
      "terminal.ansiYellow": isDark ? "#ffff66" : "#ccaa00",
      "terminal.ansiBlue": isDark ? "#6699ff" : "#0066dd",
      "terminal.ansiMagenta": colors.accent,
      "terminal.ansiCyan": isDark ? "#66ffff" : "#00aaaa",
      "terminal.ansiWhite": colors.fg,
      "terminal.ansiBrightBlack": colors.fgDim,
      "terminal.ansiBrightRed": isDark ? "#ff9999" : "#ff3333",
      "terminal.ansiBrightGreen": isDark ? "#99ff99" : "#00dd00",
      "terminal.ansiBrightYellow": isDark ? "#ffff99" : "#ffdd00",
      "terminal.ansiBrightBlue": isDark ? "#99ccff" : "#3399ff",
      "terminal.ansiBrightMagenta": colors.accentBright,
      "terminal.ansiBrightCyan": isDark ? "#99ffff" : "#00dddd",
      "terminal.ansiBrightWhite": colors.fg,
      
      "input.background": colors.bgDark,
      "input.foreground": colors.fg,
      "input.border": colors.accent + "60",
      "input.placeholderForeground": colors.fgDim + "80",
      "inputOption.activeBorder": colors.accent,
      
      "dropdown.background": colors.bgDark,
      "dropdown.foreground": colors.fg,
      "dropdown.border": colors.accent + "60",
      
      "button.background": colors.accent,
      "button.foreground": isDark ? "#ffffff" : colors.bg,
      "button.hoverBackground": colors.accentBright,
      
      "badge.background": colors.accent,
      "badge.foreground": isDark ? "#ffffff" : colors.bg,
      
      "scrollbar.shadow": colors.shadow + "80",
      "scrollbarSlider.background": colors.accent + "30",
      "scrollbarSlider.hoverBackground": colors.accent + "50",
      "scrollbarSlider.activeBackground": colors.accent + "70",
      
      "minimap.background": colors.bgSidebar,
      "minimap.selectionHighlight": colors.accent + "40",
      
      "gitDecoration.modifiedResourceForeground": colors.accentBright,
      "gitDecoration.deletedResourceForeground": isDark ? "#ff6666" : "#cc0000",
      "gitDecoration.untrackedResourceForeground": isDark ? "#66ff66" : "#00aa00",
      "gitDecoration.addedResourceForeground": isDark ? "#66ff66" : "#00aa00",
      "gitDecoration.conflictingResourceForeground": isDark ? "#ff9944" : "#dd6600",
      
      "editorError.foreground": isDark ? "#ff6666" : "#cc0000",
      "editorWarning.foreground": isDark ? "#ffaa44" : "#dd6600",
      "editorInfo.foreground": colors.accentBright,
      
      "focusBorder": colors.accent,
      "foreground": colors.fg,
      "widget.shadow": colors.shadow + "80",
      "selection.background": colors.accent + "60"
    },
    "tokenColors": baseDark.tokenColors,
    "semanticHighlighting": true,
    "semanticTokenColors": baseDark.semanticTokenColors
  };
}

// Generate all themes
for (const [colorName, scheme] of Object.entries(colorSchemes)) {
  // Dark theme
  const darkTheme = createTheme(colorName, scheme, true);
  fs.writeFileSync(
    path.join(themesDir, `aesthetic-dark-${colorName}-color-theme.json`),
    JSON.stringify(darkTheme, null, 2)
  );
  console.log(`✓ Created dark ${colorName} theme`);
  
  // Light theme
  const lightTheme = createTheme(colorName, scheme, false);
  fs.writeFileSync(
    path.join(themesDir, `aesthetic-light-${colorName}-color-theme.json`),
    JSON.stringify(lightTheme, null, 2)
  );
  console.log(`✓ Created light ${colorName} theme`);
}

console.log('\n✨ Done! Generated all saturated rainbow themes.');
