# Language Translation Pieces Plan

## Goal
Create AC commands (`english`, `danish`, etc.) that translate ANY input language to the target language, displaying results fullscreen with a copy button.

## Architecture

### Approach
Use the existing `prompt:character` system pattern (like `brother.mjs`, `alphapoet.mjs`) which:
1. Exports `before` and `after` prompt strings to instruct the AI
2. Uses `system = "prompt:character"` to route through the AI conversation system
3. Displays the AI response with built-in copy functionality

### Pieces to Create
1. **english.mjs** - Translates any input to English
2. **danish.mjs** - Translates any input to Danish
3. (Easily extensible to: `spanish.mjs`, `french.mjs`, `german.mjs`, etc.)

### File Location
`system/public/aesthetic.computer/disks/english.mjs`
`system/public/aesthetic.computer/disks/danish.mjs`

## Implementation Details

### Prompt Structure for `english.mjs`
```javascript
const prompt = "enter text in any language to translate to english";
const before = `
  You are a language translator.
  - Translate the user's input text to English
  - If the text is already in English, clean it up grammatically
  - Preserve the tone and meaning of the original
  - Only output the translation, nothing else (no explanations, no quotation marks)
  - The user says:
`;
const after = `
  - Respond ONLY with the translation, no additional text
`;
const forgetful = true; // Each translation is independent

export const scheme = {
  text: [255, 255, 255],
  background: [20, 40, 80],
  prompt: [150, 200, 255],
  block: [100, 150, 200],
  highlight: [255, 255, 200],
  guideline: [100, 150, 200],
};

export { prompt, before, after, forgetful };
export const system = "prompt:character";
export const nohud = true;
```

### Prompt Structure for `danish.mjs`
```javascript
const prompt = "indtast tekst på et hvilket som helst sprog for at oversætte til dansk";
const before = `
  You are a language translator.
  - Translate the user's input text to Danish
  - If the text is already in Danish, clean it up grammatically
  - Preserve the tone and meaning of the original
  - Only output the translation, nothing else (no explanations, no quotation marks)
  - The user says:
`;
const after = `
  - Respond ONLY with the translation in Danish, no additional text
`;
```

## UI/UX
- **Fullscreen text display**: The `prompt:character` system already provides this
- **Copy button**: Built into the prompt system (already exists)
- **Clean aesthetic**: Use color schemes fitting each language's flag/culture

## Color Schemes
- **English**: Navy blue background (#142850), white text
- **Danish**: Red (#C60C30) and white scheme

## Usage Examples
```
aesthetic.computer/english:hola mundo
→ "hello world"

aesthetic.computer/danish:hello world
→ "hej verden"

aesthetic.computer/english:こんにちは
→ "hello"
```

## Implementation Steps
1. [x] Create plan (this document)
2. [x] Create `english.mjs` piece
3. [x] Create `danish.mjs` piece  
4. [x] Create `en.mjs` shortcut (aliases to `english`)
5. [x] Create `da.mjs` shortcut (aliases to `danish`)
6. [ ] Test with various languages

## Shortcuts
- `en` → `english` (using `alias()` pattern from `ff.mjs`)
- `da` → `danish`

## Files Created
- `system/public/aesthetic.computer/disks/english.mjs` - Full piece
- `system/public/aesthetic.computer/disks/danish.mjs` - Full piece
- `system/public/aesthetic.computer/disks/en.mjs` - Alias shortcut
- `system/public/aesthetic.computer/disks/da.mjs` - Alias shortcut

## Future Extensions
- Add more languages (`spanish.mjs`, `french.mjs`, `german.mjs`, `japanese.mjs`)
- Add a generic `translate.mjs` that takes target language as param: `translate:danish:hello`
- Add voice input/output via `speak` and `listen` APIs
