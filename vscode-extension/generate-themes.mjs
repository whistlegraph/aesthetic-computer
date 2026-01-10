
import fs from 'fs';
import path from 'path';

const themeDir = 'vscode-extension/themes';
const packageJsonPath = 'vscode-extension/package.json';
const templatePath = path.join(themeDir, 'aesthetic-dark-color-theme.json');

const template = fs.readFileSync(templatePath, 'utf8');

// Base colors in the template to replace
const BASE_ACCENT = 'a87090'; // Mauve
const BASE_BG = '181318'; // Dark Purple Background
const BASE_BG_ALT = '141214'; // Slightly Darker Background
const BASE_BG_DEEP = '101010'; // Deepest Black/Background
const BASE_STATUS = '302030'; // Status Bar Background
const BASE_FOCUS = '584058'; // Focus Border / Button Background

// Helper to replace all occurrences of a color with another
function replaceColor(content, oldHex, newHex) {
  const regex = new RegExp(oldHex, 'gi');
  return content.replace(regex, newHex);
}

// Target Palettes
// names: Red, Orange, Yellow, Green, Blue, Indigo, Violet, Pink
const palettes = [
  {
    name: 'Red',
    emoji: '🔴',
    accent: 'ff5555',
    background: '181010',
    backgroundAlt: '140c0c',
    status: '301010',
    focus: '582020'
  },
  {
    name: 'Orange',
    emoji: '🟠',
    accent: 'ffb86c',
    background: '181410',
    backgroundAlt: '14100c',
    status: '302010',
    focus: '584020'
  },
  {
    name: 'Yellow',
    emoji: '🟡',
    accent: 'f1fa8c',
    background: '181810',
    backgroundAlt: '14140c',
    status: '303010',
    focus: '585820'
  },
  {
    name: 'Green',
    emoji: '🟢',
    accent: '50fa7b',
    background: '101810',
    backgroundAlt: '0c140c',
    status: '103010',
    focus: '205820'
  },
  {
    name: 'Blue',
    emoji: '🔵',
    accent: '61afef',
    background: '101418',
    backgroundAlt: '0c1014',
    status: '102030',
    focus: '204058'
  },
  {
    name: 'Indigo',
    emoji: '🟣',
    accent: '6272a4',
    background: '121018',
    backgroundAlt: '0e0c14',
    status: '181030',
    focus: '302058'
  },
  {
    name: 'Violet',
    emoji: '🔮',
    accent: 'bd93f9',
    background: '161016',
    backgroundAlt: '120c12',
    status: '201020',
    focus: '402040'
  },
  {
    name: 'Pink',
    emoji: '🌸',
    accent: 'ff79c6',
    background: '181014',
    backgroundAlt: '140c10',
    status: '301020',
    focus: '582040'
  },
  {
    name: 'Pencil',
    emoji: '✏️',
    accent: 'e0e0e0',
    background: '181818',
    backgroundAlt: '141414',
    status: '303030',
    focus: '585858'
  }
];

const generatedThemes = [];

palettes.forEach(palette => {
    let themeContent = template;
    
    // Replace names
    themeContent = themeContent.replace("Aesthetic Computer: Dark 🌑", `Aesthetic Computer: ${palette.name} ${palette.emoji}`);
    
    // Replace Colors
    themeContent = replaceColor(themeContent, BASE_ACCENT, palette.accent);
    themeContent = replaceColor(themeContent, BASE_BG, palette.background);
    themeContent = replaceColor(themeContent, BASE_BG_ALT, palette.backgroundAlt);
    themeContent = replaceColor(themeContent, BASE_STATUS, palette.status);
    themeContent = replaceColor(themeContent, BASE_FOCUS, palette.focus);

    // Write new file
    const fileName = `aesthetic-${palette.name.toLowerCase()}-color-theme.json`;
    const filePath = path.join(themeDir, fileName);
    fs.writeFileSync(filePath, themeContent);
    console.log(`Generated ${fileName}`);

    generatedThemes.push({
        label: `Aesthetic Computer: ${palette.name} ${palette.emoji}`,
        uiTheme: "vs-dark",
        path: `./themes/${fileName}`
    });
});

// Update package.json
const packageJson = JSON.parse(fs.readFileSync(packageJsonPath, 'utf8'));

// Filter out existing generated themes to avoid duplicates on re-run
const staticThemes = packageJson.contributes.themes.filter(t => 
    t.label === "Aesthetic Computer: Dark 🌑" || 
    t.label === "Aesthetic Computer: Light 🌻"
);

packageJson.contributes.themes = [...staticThemes, ...generatedThemes];

fs.writeFileSync(packageJsonPath, JSON.stringify(packageJson, null, 2));
console.log('Updated package.json');
