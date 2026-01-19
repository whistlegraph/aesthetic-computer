// translate.mjs - Shared translation module for language pieces
// Used by: english.mjs, danish.mjs, spanish.mjs, etc.

import { Conversation } from "../../lib/ask.mjs";

// Language configurations
export const languages = {
  english: {
    code: "en",
    name: "English",
    nativeName: "ENGLISH",
    flag: "🇬🇧",
    labelOriginal: "ORIGINAL:",
    labelTranslated: "ENGLISH:",
    copyButton: "COPY",
    waitingText: "Enter text to translate:",
    errorText: "Translation failed. Try again.",
    examples: ["english:hola mundo", "en:bonjour le monde"],
    colors: {
      bg: [20, 40, 80],
      title: [100, 150, 255],
      label: [150, 150, 150],
      original: [255, 200, 100],
      translated: [100, 255, 100],
      translating: [100, 200, 100],
      btnBg: [0, 200, 100],
      btnText: [0, 60, 30],
      waiting: [200, 200, 200],
      example: [150, 150, 150],
    },
  },
  danish: {
    code: "da",
    name: "Danish",
    nativeName: "DANSK",
    flag: "🇩🇰",
    labelOriginal: "ORIGINAL:",
    labelTranslated: "DANSK:",
    copyButton: "KOPIER",
    waitingText: "Indtast tekst at oversætte:",
    errorText: "Oversættelse mislykkedes. Prøv igen.",
    examples: ["danish:hello world", "da:bonjour le monde"],
    colors: {
      bg: [198, 12, 48],
      title: [255, 200, 200],
      label: [200, 150, 150],
      original: [255, 255, 200],
      translated: [255, 255, 255],
      translating: [255, 200, 200],
      btnBg: [255, 255, 255],
      btnText: [198, 12, 48],
      waiting: [255, 200, 200],
      example: [200, 150, 150],
    },
  },
  spanish: {
    code: "es",
    name: "Spanish",
    nativeName: "ESPAÑOL",
    flag: "🇪🇸",
    labelOriginal: "ORIGINAL:",
    labelTranslated: "ESPAÑOL:",
    copyButton: "COPIAR",
    waitingText: "Ingresa texto para traducir:",
    errorText: "La traducción falló. Inténtalo de nuevo.",
    examples: ["spanish:hello world", "es:bonjour le monde"],
    colors: {
      bg: [170, 21, 27],
      title: [255, 196, 0],
      label: [200, 150, 100],
      original: [255, 255, 200],
      translated: [255, 255, 255],
      translating: [255, 220, 150],
      btnBg: [255, 196, 0],
      btnText: [170, 21, 27],
      waiting: [255, 220, 150],
      example: [200, 150, 100],
    },
  },
};

// Shared state per instance
const instances = new Map();

function getState(lang) {
  if (!instances.has(lang)) {
    instances.set(lang, {
      original: "",
      translation: "",
      status: "waiting",
      copyBtn: null,
      conversation: null,
    });
  }
  return instances.get(lang);
}

function makeProgram(langConfig) {
  return {
    before: `You are a language translator. Translate the user's input text to ${langConfig.name}.
If the text is already in ${langConfig.name}, simply clean it up grammatically if needed.
Preserve the tone, style, and meaning of the original.
Only output the translation, nothing else - no explanations, no quotation marks, no prefixes.
The user says:`,
    after: `Respond ONLY with the translated ${langConfig.name} text, nothing more.`,
  };
}

export function boot(lang, { params, store, slug, needsPaint }) {
  const config = languages[lang];
  const state = getState(lang);
  
  state.original = params.join(" ");
  
  if (!state.original) {
    state.status = "waiting";
    return;
  }
  
  state.status = "translating";
  state.translation = "";
  
  state.conversation = new Conversation(store, slug, true);
  state.conversation.ask(
    { prompt: state.original, program: makeProgram(config), hint: "character" },
    function and(msg) {
      state.translation += msg;
      needsPaint();
    },
    function done() {
      state.status = "done";
      needsPaint();
    },
    function fail() {
      state.status = "error";
      state.translation = config.errorText;
      needsPaint();
    }
  );
}

// HUD reserved space (matches notepat.mjs)
const TOP_BAR_BOTTOM = 21;

export function paint(lang, { wipe, ink, screen, ui }) {
  const config = languages[lang];
  const state = getState(lang);
  const c = config.colors;
  
  wipe(...c.bg);
  
  const margin = 12;
  const lineHeight = 14;
  const hudReserved = TOP_BAR_BOTTOM;
  const bottomPadding = 8;
  
  let y = hudReserved + margin;
  
  // Title - flag and language name
  ink(...c.title).write(`${config.flag} ${config.nativeName}`, { x: margin, y });
  y += lineHeight * 2;
  
  if (state.status === "waiting") {
    ink(...c.waiting).write(config.waitingText, { x: margin, y });
    y += lineHeight;
    for (const ex of config.examples) {
      ink(...c.example).write(ex, { x: margin, y });
      y += lineHeight;
    }
    return;
  }
  
  // Original text label
  ink(...c.label).write(config.labelOriginal, { x: margin, y });
  y += lineHeight;
  
  // Original text (wrapped)
  ink(...c.original).write(state.original, { x: margin, y }, undefined, screen.width - margin * 2);
  const origLines = Math.ceil((state.original.length * 6) / (screen.width - margin * 2)) || 1;
  y += lineHeight * Math.max(origLines, 1) + lineHeight;
  
  // Translated text label
  ink(...c.label).write(config.labelTranslated, { x: margin, y });
  y += lineHeight;
  
  // Translation or status
  if (state.status === "translating") {
    ink(...c.translating).write(state.translation + "...", { x: margin, y }, undefined, screen.width - margin * 2);
  } else if (state.status === "done" || state.status === "error") {
    ink(...c.translated).write(state.translation, { x: margin, y }, undefined, screen.width - margin * 2);
    
    // Copy button - positioned at bottom with proper padding
    const btnText = state.copied ? "✓ Copied!" : (state.copyError ? "✗ Failed" : config.copyButton);
    const btnW = btnText.length * 6 + 20;
    const btnH = 28;
    const btnX = screen.width / 2 - btnW / 2;
    const btnY = screen.height - btnH - bottomPadding - margin;
    
    // Always create a fresh Button to ensure proper Box object
    state.copyBtn = new ui.Button({ x: btnX, y: btnY, w: btnW, h: btnH });
    
    // Button colors based on state
    let bgColor = c.btnBg;
    let textColor = c.btnText;
    
    if (state.copied) {
      bgColor = [100, 255, 100];
      textColor = [0, 80, 0];
    } else if (state.copyError) {
      bgColor = [255, 100, 100];
      textColor = [80, 0, 0];
    } else if (state.copyBtn.down) {
      bgColor = c.btnText;
      textColor = c.btnBg;
    }
    
    ink(...bgColor).box(state.copyBtn.box, "fill");
    ink(...textColor).write(btnText, { x: btnX + 10, y: btnY + 9 });
  }
}

export function act(lang, { event: e, send, sound, needsPaint }) {
  const state = getState(lang);
  
  // Handle clipboard result events
  if (e.is("clipboard:copy:copied")) {
    state.copied = true;
    state.copyError = false;
    needsPaint();
    // Auto-reset after showing "Copied!" for a moment
    setTimeout(() => {
      state.copied = false;
      needsPaint();
    }, 1200);
    return;
  }
  
  if (e.is("clipboard:copy:failed")) {
    state.copyError = true;
    needsPaint();
    setTimeout(() => {
      state.copyError = false;
      needsPaint();
    }, 2000);
    return;
  }
  
  // Button interaction
  state.copyBtn?.act(e, () => {
    if (state.translation && state.status === "done" && !state.copied) {
      // Use send to copy (like chat.mjs does)
      send({ type: "copy", content: state.translation });
      sound.synth({ tone: 800, duration: 0.08, volume: 0.5 });
    }
  });
  
  // Repaint on button hover/down state changes
  if (e.is("touch") || e.is("lift") || e.is("draw")) {
    needsPaint();
  }
}

export function meta(lang) {
  const config = languages[lang];
  return {
    title: config.name,
    desc: `${config.flag} Translate any language to ${config.name}`,
  };
}
