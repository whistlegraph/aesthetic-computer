// chat capabilities, 26.08.08
// What the AC chat server will accept, stated once.
//
// The 128-char cap used to live in four unlinked places — the `chat` piece, the
// MCP tool, a comment, and the server itself — so they drifted. The server is
// the only authority, and this is the server's copy. Everything else reads it:
//
//   session-server/chat-manager.mjs  enforces it, and hands it to every client
//                                    in the `connected` packet
//   .../disks/chat.mjs               takes its limit from that packet
//   /chat/capabilities               (session-server, per chat host)
//   /api/chat-capabilities           (lith, for bots that only know the apex)
//   slab/bin/chat-mcp.mjs            builds the chat_send tool contract from it
//
// If you change the cap or the syntax, change it here and nowhere else.

export const MAX_CHARS = 128;

// JS `String.length` — UTF-16 code units, not codepoints, graphemes, or bytes.
// A non-BMP emoji costs 2 and a flag costs 4, so "128 characters" and "128 of
// what a human would call characters" are different numbers. Every surface that
// advertises the cap has to say which one it means or people count graphemes
// and get a `too-long` they don't understand.
export const COUNTED_AS = "utf16-code-units";

// Channels that skip session-server/filter.mjs. `chat-clock` is the Danish
// laer-klokken room; the English wordlist mauls it.
const UNFILTERED = new Set(["chat-clock"]);

export function profanityFiltered(channel) {
  return !UNFILTERED.has(channel);
}

export function tooLong(text) {
  return String(text ?? "").length > MAX_CHARS;
}

// Message syntax — the tokens `lib/chat-highlighting.mjs` finds and makes
// tappable. Kept in that file's scan order so the two stay comparable.
export const TOKENS = [
  {
    token: "'…'",
    name: "prompt",
    example: "try 'starfield'",
    does:
      "Single-quoted text becomes a tappable jump to `prompt <text>`. This is the ONLY way to link a piece — a bare piece name is plain text. Quoted KidLisp source is syntax-highlighted and runs as KidLisp instead. Contractions (I'll, you'll) are not matched.",
  },
  { token: "@handle", name: "handle", example: "@jeffrey", does: "Opens that user's profile." },
  { token: "#code", name: "painting", example: "#Lv2", does: "Opens the painting with that code." },
  { token: "$code", name: "kidlisp", example: "$cow", does: "Opens the stored KidLisp piece." },
  { token: "*code", name: "clock", example: "*bell", does: "Opens that clock piece." },
  { token: "!code", name: "tape", example: "!x7q", does: "Opens that tape recording." },
  { token: "r8dio", name: "r8dio", example: "r8dio", does: "Bare word — starts the radio player. `@r8dio` stays a handle." },
  { token: "https://… | www.…", name: "url", example: "https://prompt.ac", does: "Opens in the browser. A URL that trips the sensitive-word list renders as [click to reveal link]." },
  { token: "youtube link", name: "youtube", example: "https://youtu.be/dQw4w9WgXcQ", does: "Watch/shorts/embed/youtu.be links get an inline player." },
  { token: "name@host.tld", name: "email", example: "me@aesthetic.computer", does: "Opens a mailto:." },
];

// Bare-word commands. There are no slash commands. These are intercepted by the
// chat piece BEFORE the socket send, so they are UI, not protocol — a bot that
// pushes "radio" through `chat:message` just posts the word "radio".
export const COMMANDS = [
  { command: "radio", does: "Toggle the radio player.", local: true },
  { command: "radio off | radio stop | hush | mute radio", does: "Stop the radio.", local: true },
  { command: "r8dio | bj", does: "Play that station.", local: true },
  { command: "fight @handle", does: "Challenge someone to a fight (login required).", local: true },
  { command: "fight accept | fight decline", does: "Answer a pending challenge.", local: true },
];

export function chatCapabilities(channel = "chat-system") {
  return {
    channel,
    maxChars: MAX_CHARS,
    countedAs: COUNTED_AS,
    profanityFiltered: profanityFiltered(channel),
    tokens: TOKENS,
    commands: COMMANDS,
  };
}

// A plain-text rendering of the above, for places that can only carry prose —
// an MCP tool description, a `help` reply. Generated so the numbers can't drift
// from the object.
export function capabilitiesBrief(caps = chatCapabilities()) {
  const tokens = caps.tokens.map((t) => `  ${t.token}  — ${t.does}`).join("\n");
  const commands = caps.commands.map((c) => `  ${c.command}  — ${c.does}`).join("\n");
  return [
    `Limit: ${caps.maxChars} characters, counted as ${caps.countedAs} (JS String.length — a non-BMP emoji costs 2, a flag 4). Over that, the server answers "too-long" and drops the message.`,
    `Profanity filter: ${caps.profanityFiltered ? "on" : "off"} for ${caps.channel}.`,
    ``,
    `Syntax (tokens become tappable links):`,
    tokens,
    ``,
    `Bare-word commands — typed in the chat UI only, no slash commands. Sending these as a message just posts the words:`,
    commands,
  ].join("\n");
}
