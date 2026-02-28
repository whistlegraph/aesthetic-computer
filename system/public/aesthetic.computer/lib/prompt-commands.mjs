// Prompt Commands Registry
// Centralized documentation for prompt commands used by chat.mjs modal and prompt.mjs

// Command descriptions for the link confirmation modal in chat
// These provide context about what each command does before execution
export const commandDescriptions = {
  // 🛒 Shop & Commerce
  shop: "Open the Aesthetic Computer shop",
  
  // 🎨 Painting Transform Commands
  flip: "Flip your painting vertically",
  flop: "Flip your painting horizontally", 
  right: "Rotate your painting 90° clockwise",
  left: "Rotate your painting 90° counter-clockwise",
  wipe: "Clear your painting canvas",
  
  // 📼 Recording Commands
  tape: "Start recording a tape",
  "tape:cut": "Stop recording and save tape",
  cut: "Stop recording and save tape",
  video: "Open video player/editor",
  
  // 🎄 Pipeline Commands
  merry: "Run a sequence of pieces",
  merryo: "Run a looping sequence of pieces",
  stop: "Stop current merry pipeline",
  "merry:stop": "Stop current merry pipeline",
  
  // 👤 Profile & Identity
  me: "View your profile",
  profile: "View your profile",
  handle: "Set or view your handle",
  login: "Log in to your account",
  logout: "Log out of your account",
  
  // 🔔 Notifications
  notifs: "Enable notifications",
  nonotifs: "Disable notifications",
  scream: "Send a scream notification",
  
  // 📷 Camera
  selfie: "Take a selfie",
  camera: "Open camera",
  camu: "Camera utilities",
  
  // 🖼️ Painting Management  
  print: "Print your painting",
  mint: "Mint your painting as NFT",
  painting: "Open painting viewer",
  "painting:start": "Start a new painting",
  "painting:done": "Finish current painting",
  done: "Finish current painting",
  "yes!": "Confirm and finish painting",
  download: "Download your painting",
  
  // 🎵 Sound & Music
  tone: "Play a tone",
  bgm: "Background music controls",
  sing: "Open singing interface",
  song: "Play/create a song",
  melody: "Create a melody",
  metronome: "Start metronome",
  sfx: "Sound effects",
  whistle: "Whistle sound",
  bleep: "Bleep sound",
  
  // 🎮 Interactive Pieces
  wand: "Magic wand tool",
  sparkle: "Sparkle effect",
  bubble: "Bubble effect",
  starfield: "Starfield visualization",
  
  // ✏️ Drawing Tools
  line: "Draw lines",
  oval: "Draw ovals",
  rect: "Draw rectangles",
  plot: "Plot points",
  shape: "Shape tool",
  smear: "Smear tool",
  brush: "Brush tool",
  stamp: "Stamp tool",
  handprint: "Handprint stamp",
  
  // 🔧 Utilities
  zoom: "Zoom in/out",
  paste: "Paste from clipboard",
  encode: "Encode data",
  decode: "Decode data",
  
  // 💬 Social
  chat: "Open chat",
  channel: "Join a channel",
  
  // 📱 Platform
  "at": "Open ATProto pages",
  
  // 🎪 Fun & Games
  "freaky-flowers": "Freaky flowers animation",
  "scawy-snake": "Scary snake game",
  gargoyle: "Gargoyle piece",
  liar: "Liar game",
  "no!": "No! reaction",
  no: "No reaction",
  
  // 👨‍👩‍👧‍👦 Relationships (AI companions)
  girlfriend: "AI girlfriend chat",
  boyfriend: "AI boyfriend chat",
  mom: "AI mom chat",
  dad: "AI dad chat",
  husband: "AI husband chat",
  wife: "AI wife chat",
  kid: "AI kid chat",
  brother: "AI brother chat",
  sister: "AI sister chat",
  
  // 🎓 Learning
  word: "Word of the day",
  bits: "Learn about bits",
  l5: "Open the L5 try page",
  l5learn: "Open the L5 try page",
  l5docs: "Open L5 compatibility docs",
  processing: "Open the Processing try page",
  processinglearn: "Open the Processing try page",
  processingdocs: "Open Processing compatibility docs",
  
  // 🔗 External
  whistlegraph: "Whistlegraph website",
  wg: "Whistlegraph (short)",
  wgr: "Whistlegraph radio",
  
  // 📻 Radio
  r8dio: "Play R8dio.dk live stream",
  r8Dio: "Play R8dio.dk live stream",
  radio: "Play R8dio.dk live stream",
  "r8dio:web": "Open R8dio.dk website",
  "r8Dio:web": "Open R8dio.dk website",
  "radio:web": "Open R8dio.dk website",
  
  // 🤖 Admin Commands
  patch: "Spawn a PR agent (admin only)",
};

// Get a description for a command, with fallback
export function getCommandDescription(command) {
  if (!command) return null;
  
  // Normalize the command (lowercase, trim)
  const normalized = command.toLowerCase().trim();
  
  // Direct lookup
  if (commandDescriptions[normalized]) {
    return commandDescriptions[normalized];
  }
  
  // Check for commands with parameters (e.g., "shop/item" -> "shop")
  const baseCommand = normalized.split(/[\s\/]/)[0];
  if (commandDescriptions[baseCommand]) {
    return commandDescriptions[baseCommand];
  }
  
  // Check for colon-prefixed variants (e.g., "tape:add" -> check "tape")
  const colonBase = normalized.split(":")[0];
  if (colonBase !== normalized && commandDescriptions[colonBase]) {
    return commandDescriptions[colonBase];
  }
  
  return null;
}

// Check if a command is a "prompt-only" command (doesn't load a piece)
// These commands execute immediately and return to prompt
export const promptOnlyCommands = new Set([
  "flip", "flop", "right", "left", "wipe",
  "cut", "tape:cut", "stop", "merry:stop",
  "notifs", "nonotifs", "scream",
  "download", "done", "yes!", "painting:done",
]);

export function isPromptOnlyCommand(command) {
  if (!command) return false;
  const normalized = command.toLowerCase().trim();
  return promptOnlyCommands.has(normalized);
}
