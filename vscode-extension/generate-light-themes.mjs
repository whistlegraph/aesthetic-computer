import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const themesDir = path.join(__dirname, 'themes');

// Color mappings from dark to light theme
const colorMap = {
  // Backgrounds: dark -> light
  '#181010': '#fef8f0',  // editor bg (red tint)
  '#181018': '#f8f0fe',  // editor bg (violet tint)
  '#101810': '#f0fef8',  // editor bg (green tint)
  '#101018': '#f0f8fe',  // editor bg (blue tint)
  '#181810': '#fefef0',  // editor bg (yellow tint)
  '#101010': '#e8e0e0',  // darker panels
  '#140c0c': '#f5f0e8',  // sidebar
  '#0c140c': '#f0f5e8',  // sidebar (green)
  '#0c0c14': '#f0f0f5',  // sidebar (blue)
  '#14140c': '#f5f5e8',  // sidebar (yellow)
  '#301010': '#d0c0c0',  // status bar
  '#103010': '#c0d0c0',  // status bar (green)
  '#101030': '#c0c0d0',  // status bar (blue)
  '#303010': '#d0d0c0',  // status bar (yellow)
  
  // Foregrounds: white -> dark
  '#ffffff': '#281e5a',
  '#ffffffcc': '#281e5a',
  '#ffffff80': '#281e5a80',
  '#ffffff60': '#281e5a60',
  '#ffffff50': '#281e5a50',
  '#ffffff25': '#281e5a40',
  
  // Grays
  '#707070': '#606060',
  '#a0a0a0': '#505050',
  '#b0a0a8': '#281e5a',
  '#d0c0c8': '#281e5a',
  '#d0d0d0': '#404040',
  '#606060': '#a0a0a0',
  '#404060': '#c0c0e0',
  '#606080': '#a0a0c0',
  '#483848': '#d0c0d0',
};

// Function to lighten a hex color
function lightenColor(hex, accentHex) {
  // Remove # if present
  hex = hex.replace('#', '');
  accentHex = accentHex ? accentHex.replace('#', '') : null;
  
  // Parse hex to RGB
  let r = parseInt(hex.substring(0, 2), 16);
  let g = parseInt(hex.substring(2, 4), 16);
  let b = parseInt(hex.substring(4, 6), 16);
  let alpha = hex.length === 8 ? hex.substring(6, 8) : '';
  
  // Get accent color if provided
  let ar, ag, ab;
  if (accentHex && accentHex.length >= 6) {
    ar = parseInt(accentHex.substring(0, 2), 16);
    ag = parseInt(accentHex.substring(2, 4), 16);
    ab = parseInt(accentHex.substring(4, 6), 16);
  }
  
  // If it's a very dark color (bg), make it very light with accent tint
  if (r < 40 && g < 40 && b < 40) {
    if (ar !== undefined) {
      // Add slight accent tint to light background
      r = 250 + Math.floor((ar - 128) * 0.05);
      g = 250 + Math.floor((ag - 128) * 0.05);
      b = 250 + Math.floor((ab - 128) * 0.05);
    } else {
      r = 252; g = 247; b = 245;
    }
  }
  // If it's a light color (fg), make it dark
  else if (r > 180 && g > 180 && b > 180) {
    r = 40; g = 30; b = 90;
  }
  // If it's a medium color, invert brightness
  else {
    r = 255 - r;
    g = 255 - g;
    b = 255 - b;
  }
  
  // Clamp values
  r = Math.min(255, Math.max(0, r));
  g = Math.min(255, Math.max(0, g));
  b = Math.min(255, Math.max(0, b));
  
  return '#' + [r, g, b].map(x => x.toString(16).padStart(2, '0')).join('') + alpha;
}

// Function to transform a dark theme to light
function transformToLight(darkTheme, accentColor) {
  const lightTheme = JSON.parse(JSON.stringify(darkTheme));
  
  // Update theme metadata
  lightTheme.name = lightTheme.name.replace('Dark ', 'Light ');
  lightTheme.type = 'light';
  
  // Transform colors
  for (const [key, value] of Object.entries(lightTheme.colors)) {
    if (typeof value === 'string' && value.startsWith('#')) {
      // Check if we have a direct mapping
      if (colorMap[value]) {
        lightTheme.colors[key] = colorMap[value];
      }
      // If it's the accent color or contains it, keep it but adjust for light theme
      else if (value.toLowerCase().startsWith(accentColor.toLowerCase().substring(0, 4))) {
        // Keep accent colors but ensure they work on light background
        lightTheme.colors[key] = value;
      }
      // Otherwise, transform the color
      else {
        lightTheme.colors[key] = lightenColor(value, accentColor);
      }
    }
  }
  
  // Special adjustments for light theme
  lightTheme.colors['editor.foreground'] = '#281e5a';
  lightTheme.colors['sideBar.foreground'] = '#281e5a';
  lightTheme.colors['activityBar.foreground'] = '#281e5a';
  lightTheme.colors['titleBar.activeForeground'] = '#281e5a';
  lightTheme.colors['statusBar.foreground'] = '#ffffff';  // Keep white on colored status bar
  
  return lightTheme;
}

// Process each dark rainbow theme
const colors = ['red', 'orange', 'yellow', 'green', 'blue', 'indigo', 'violet', 'pink', 'pencil'];
const accentColors = {
  red: '#ff5555',
  orange: '#ff8833',
  yellow: '#ffdd44',
  green: '#55ff55',
  blue: '#5588ff',
  indigo: '#6655ff',
  violet: '#aa55ff',
  pink: '#ff88cc',
  pencil: '#888888'
};

for (const color of colors) {
  const darkFilePath = path.join(themesDir, `aesthetic-dark-${color}-color-theme.json`);
  const lightFilePath = path.join(themesDir, `aesthetic-light-${color}-color-theme.json`);
  
  try {
    const darkTheme = JSON.parse(fs.readFileSync(darkFilePath, 'utf8'));
    const lightTheme = transformToLight(darkTheme, accentColors[color]);
    
    fs.writeFileSync(lightFilePath, JSON.stringify(lightTheme, null, 2));
    console.log(`✓ Created ${color} light theme`);
  } catch (error) {
    console.error(`✗ Failed to create ${color} light theme:`, error.message);
  }
}

console.log('\nDone! Generated all light theme variations.');
