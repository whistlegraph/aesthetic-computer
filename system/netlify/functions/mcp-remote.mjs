// Remote MCP Server, 2026.02.13
// SSE-based MCP server for aesthetic.computer
// Enables Claude Pro/Team users to connect without local installation

import { respond } from "../../backend/http.mjs";

const SERVER_INFO = {
  name: "aesthetic-computer",
  version: "1.1.0",
};

const TOOLS = [
  {
    name: "publish_piece",
    description: "Publish a JavaScript piece to aesthetic.computer",
    inputSchema: {
      type: "object",
      properties: {
        source: {
          type: "string",
          description: "JavaScript piece source code (max 100,000 characters)",
        },
        name: {
          type: "string",
          description: "Optional name for the piece",
        },
      },
      required: ["source"],
    },
  },
  {
    name: "publish_kidlisp",
    description:
      "Publish KidLisp code to aesthetic.computer. KidLisp is a creative coding Lisp dialect for generative art.",
    inputSchema: {
      type: "object",
      properties: {
        source: {
          type: "string",
          description: "KidLisp source code (max 50,000 characters)",
        },
      },
      required: ["source"],
    },
  },
  {
    name: "publish_clock",
    description:
      "Publish a clock melody to aesthetic.computer. Returns a pronounceable short code.",
    inputSchema: {
      type: "object",
      properties: {
        source: {
          type: "string",
          description: "Clock melody string (max 10,000 characters)",
        },
      },
      required: ["source"],
    },
  },
  {
    name: "get_api_info",
    description:
      "Fetch the full API documentation from aesthetic.computer",
    inputSchema: {
      type: "object",
      properties: {},
    },
  },
  {
    name: "preview_kidlisp",
    description:
      "Validate KidLisp source code without publishing. Checks syntax (balanced parens, strings), identifies functions used, and flags common mistakes. Use this before publish_kidlisp to catch errors.",
    inputSchema: {
      type: "object",
      properties: {
        source: {
          type: "string",
          description: "KidLisp source code to validate",
        },
      },
      required: ["source"],
    },
  },
];

const RESOURCES = [
  {
    uri: "aesthetic-computer://piece-template",
    name: "Piece Template",
    description:
      "Starter template for a new aesthetic.computer JavaScript piece",
    mimeType: "text/plain",
  },
  {
    uri: "aesthetic-computer://kidlisp-reference",
    name: "KidLisp Language Reference",
    description:
      "Complete reference for KidLisp: 118 functions across 12 categories for generative art",
    mimeType: "text/plain",
  },
  {
    uri: "aesthetic-computer://piece-examples",
    name: "KidLisp Top Hit Examples",
    description:
      "Real KidLisp pieces from aesthetic.computer with 500+ views — proven patterns",
    mimeType: "text/plain",
  },
];

const PROMPTS = [
  {
    name: "create-piece",
    description: "Guided prompt for creating an aesthetic.computer JavaScript piece",
    arguments: [
      { name: "name", description: "Name of the piece", required: true },
      {
        name: "description",
        description: "What the piece should do",
        required: true,
      },
    ],
  },
  {
    name: "create-kidlisp",
    description:
      "Guide for creating a KidLisp piece — generative art on aesthetic.computer",
    arguments: [
      {
        name: "description",
        description: "What the piece should do or look like",
        required: true,
      },
    ],
  },
];

// ── KidLisp validator (shared logic with stdio server) ──

function validateKidLisp(source) {
  const errors = [];
  const warnings = [];
  const functionsUsed = new Set();
  let expressionCount = 0;
  let hasAnimation = false;
  let hasInteraction = false;
  let hasWipe = false;

  if (!source.trim()) {
    errors.push("Source code is empty");
    return {
      valid: false,
      errors,
      warnings,
      stats: {
        expressions: 0,
        functions_used: [],
        has_animation: false,
        has_interaction: false,
        has_wipe: false,
      },
    };
  }

  let depth = 0;
  let inString = false;
  let stringChar = "";
  let inComment = false;

  for (let i = 0; i < source.length; i++) {
    const ch = source[i];
    if (inComment) {
      if (ch === "\n") inComment = false;
      continue;
    }
    if (inString) {
      if (ch === stringChar && source[i - 1] !== "\\") inString = false;
      continue;
    }
    if (ch === ";") {
      inComment = true;
      continue;
    }
    if (ch === '"' || ch === "'") {
      inString = true;
      stringChar = ch;
      continue;
    }
    if (ch === "(") {
      depth++;
      expressionCount++;
    } else if (ch === ")") {
      depth--;
      if (depth < 0) errors.push(`Unmatched closing parenthesis at position ${i}`);
    }
  }

  if (depth > 0)
    errors.push(
      `${depth} unclosed parenthes${depth === 1 ? "is" : "es"} — missing closing )`,
    );
  if (inString) errors.push("Unterminated string literal");

  const funcPattern = /\(\s*([a-zA-Z_+\-*/%?][a-zA-Z0-9_]*)/g;
  let match;
  while ((match = funcPattern.exec(source)) !== null) {
    const fname = match[1];
    functionsUsed.add(fname);
    if (fname === "wipe") hasWipe = true;
    if (fname === "tap" || fname === "draw") hasInteraction = true;
  }

  const firstLine = source.trim().split("\n")[0].trim();
  const bareColors = [
    "black", "white", "red", "blue", "green", "purple", "navy", "brown",
    "salmon", "beige", "cyan",
  ];
  if (bareColors.some((c) => firstLine.startsWith(c))) hasWipe = true;

  if (/\d+\.?\d*s[.!]?/.test(source)) hasAnimation = true;
  if (
    functionsUsed.has("wiggle") ||
    functionsUsed.has("spin") ||
    functionsUsed.has("scroll") ||
    functionsUsed.has("zoom")
  )
    hasAnimation = true;

  if (!hasWipe)
    warnings.push(
      "No background color found — consider starting with (wipe color) or a bare color name",
    );

  if (/\(def\s+[a-zA-Z][a-zA-Z0-9]*-[a-zA-Z]/.test(source))
    errors.push(
      "Identifier contains a dash (-) which is parsed as subtraction. Use underscores: my_var not my-var",
    );

  return {
    valid: errors.length === 0,
    errors,
    warnings,
    stats: {
      expressions: expressionCount,
      functions_used: [...functionsUsed].sort(),
      has_animation: hasAnimation,
      has_interaction: hasInteraction,
      has_wipe: hasWipe,
    },
  };
}

// ── Handle MCP protocol messages ──

async function handleMCPMessage(message, authToken) {
  const { method, params } = message;

  switch (method) {
    case "initialize":
      return {
        protocolVersion: "2024-11-05",
        capabilities: {
          tools: {},
          resources: {},
          prompts: {},
        },
        serverInfo: SERVER_INFO,
      };

    case "tools/list":
      return { tools: TOOLS };

    case "tools/call":
      return await handleToolCall(params, authToken);

    case "resources/list":
      return { resources: RESOURCES };

    case "resources/read":
      return await handleResourceRead(params);

    case "prompts/list":
      return { prompts: PROMPTS };

    case "prompts/get":
      return await handlePromptGet(params);

    default:
      throw new Error(`Unknown method: ${method}`);
  }
}

// ── Tool handlers ──

async function handleToolCall(params, authToken) {
  const { name, arguments: args } = params;

  try {
    switch (name) {
      case "publish_piece":
        return await callPublishPiece(args, authToken);
      case "publish_kidlisp":
        return await callPublishKidLisp(args, authToken);
      case "publish_clock":
        return await callPublishClock(args, authToken);
      case "get_api_info":
        return await callGetApiInfo();
      case "preview_kidlisp":
        return callPreviewKidLisp(args);
      default:
        throw new Error(`Unknown tool: ${name}`);
    }
  } catch (error) {
    return {
      content: [{ type: "text", text: `Error: ${error.message}` }],
      isError: true,
    };
  }
}

async function callPublishPiece(args, authToken) {
  const headers = { "Content-Type": "application/json" };
  if (authToken) headers["Authorization"] = `Bearer ${authToken}`;

  const response = await fetch("https://aesthetic.computer/api/store-piece", {
    method: "POST",
    headers,
    body: JSON.stringify(args),
  });

  const result = await response.json();

  return {
    content: [
      {
        type: "text",
        text: JSON.stringify(
          {
            code: result.code,
            url: result.url,
            cached: result.cached,
          },
          null,
          2,
        ),
      },
    ],
  };
}

async function callPublishKidLisp(args, authToken) {
  const headers = { "Content-Type": "application/json" };
  if (authToken) headers["Authorization"] = `Bearer ${authToken}`;

  const response = await fetch(
    "https://aesthetic.computer/api/store-kidlisp",
    { method: "POST", headers, body: JSON.stringify(args) },
  );

  const result = await response.json();

  return {
    content: [
      {
        type: "text",
        text: JSON.stringify(
          {
            code: result.code,
            url: `https://aesthetic.computer/${result.code}`,
            cached: result.cached,
          },
          null,
          2,
        ),
      },
    ],
  };
}

async function callPublishClock(args, authToken) {
  const headers = { "Content-Type": "application/json" };
  if (authToken) headers["Authorization"] = `Bearer ${authToken}`;

  const response = await fetch("https://aesthetic.computer/api/store-clock", {
    method: "POST",
    headers,
    body: JSON.stringify(args),
  });

  const result = await response.json();

  return {
    content: [
      {
        type: "text",
        text: JSON.stringify(
          {
            code: result.code,
            url: `https://aesthetic.computer/clock~${result.code}`,
            cached: result.cached,
          },
          null,
          2,
        ),
      },
    ],
  };
}

async function callGetApiInfo() {
  const response = await fetch("https://aesthetic.computer/api", {
    headers: { Accept: "application/json" },
  });

  const docs = await response.json();

  return {
    content: [{ type: "text", text: JSON.stringify(docs, null, 2) }],
  };
}

function callPreviewKidLisp(args) {
  const result = validateKidLisp(args.source || "");
  return {
    content: [{ type: "text", text: JSON.stringify(result, null, 2) }],
  };
}

// ── Resource handlers ──

async function handleResourceRead(params) {
  const { uri } = params;

  if (uri === "aesthetic-computer://piece-template") {
    return {
      contents: [
        {
          uri,
          mimeType: "text/plain",
          text: `// aesthetic.computer piece template

export function boot($) {
  // Called once when the piece loads
  $.wipe("blue");
}

export function paint($) {
  // Called every frame (60fps)
  $.ink("white");
  $.circle($.screen.width / 2, $.screen.height / 2, 50);
}

export function act($) {
  // Handle user input events
  if ($.event.is("touch")) {
    console.log("Touch at:", $.event.x, $.event.y);
  }
}

export function sim($) {
  // Update state / physics
}`,
        },
      ],
    };
  }

  if (uri === "aesthetic-computer://kidlisp-reference") {
    return {
      contents: [
        {
          uri,
          mimeType: "text/plain",
          text: KIDLISP_REFERENCE,
        },
      ],
    };
  }

  if (uri === "aesthetic-computer://piece-examples") {
    return {
      contents: [
        {
          uri,
          mimeType: "text/plain",
          text: PIECE_EXAMPLES,
        },
      ],
    };
  }

  throw new Error(`Unknown resource: ${uri}`);
}

// ── Prompt handlers ──

async function handlePromptGet(params) {
  const { name, arguments: args } = params;

  if (name === "create-piece") {
    return {
      description: "Create an aesthetic.computer JavaScript piece",
      messages: [
        {
          role: "user",
          content: {
            type: "text",
            text: `Create a new aesthetic.computer piece called "${args.name}" that ${args.description}. Use the aesthetic-computer://piece-template resource as a starting point. After writing the piece, publish it using the publish_piece tool.`,
          },
        },
      ],
    };
  }

  if (name === "create-kidlisp") {
    return {
      description: "Create a KidLisp piece for aesthetic.computer",
      messages: [
        {
          role: "user",
          content: {
            type: "text",
            text: `Create a KidLisp piece for aesthetic.computer.

Description: ${args.description}

Read the aesthetic-computer://kidlisp-reference resource for the full language reference, and aesthetic-computer://piece-examples for real top-hit examples with 500+ views.

Key tips:
- Start with a background color (bare word like \`black\` or \`(wipe color)\`)
- Use \`(ink color)\` before drawing, \`(? a b c)\` for random, \`(... a b c)\` for cycling
- Combine transforms (scroll + zoom + blur) for feedback loops
- Use timing expressions: \`0.1s\`, \`1s...\`, \`2s!\`
- Keep it minimal — many top hits are under 5 lines

After writing the piece, publish it using the publish_kidlisp tool.`,
          },
        },
      ],
    };
  }

  throw new Error(`Unknown prompt: ${name}`);
}

// ── Inline content (matches stdio server) ──

const KIDLISP_REFERENCE = `# KidLisp Language Reference

KidLisp is a minimal Lisp dialect for creating generative art and interactive experiences on aesthetic.computer.
It uses S-expressions where the first element is a function name followed by arguments.

## Syntax Basics

\`\`\`lisp
; Comments start with semicolon
(function-name arg1 arg2 ...)

; Shorthand comma syntax (no parens needed for simple commands)
purple, ink, line, blur 5

; Variables & Functions
(def name value)
(later name param1 param2 body...)
\`\`\`

Shorthand: \`w\` = width, \`h\` = height, \`f\` = frame. \`?\` = random, \`...\` = cycle.
Identifiers: no dashes (parsed as subtraction). Use \`my_var\` not \`my-var\`.

## Drawing

| Function | Usage | Description |
|----------|-------|-------------|
| \`wipe\` | \`(wipe color)\` | Clear screen |
| \`ink\` | \`(ink color [alpha])\` | Set draw color |
| \`line\` | \`(line x1 y1 x2 y2)\` or \`(line)\` | Line (random if no args) |
| \`box\` | \`(box x y w h)\` | Rectangle |
| \`circle\` | \`(circle x y radius)\` | Circle |
| \`tri\` | \`(tri x1 y1 x2 y2 x3 y3)\` | Triangle |
| \`plot\` | \`(plot x y)\` | Single pixel |
| \`flood\` | \`(flood x y)\` | Flood fill |
| \`fill\`/\`outline\` | \`(fill)\` / \`(outline)\` | Toggle shape mode |

## Colors

- Named: \`red\`, \`blue\`, \`lime\`, \`navy\`, etc. (all CSS colors, no quotes needed)
- RGB: \`(ink 255 0 0)\` — With alpha: \`(ink red 128)\`
- Special: \`rainbow\`, \`zebra\`, \`erase\`
- Gradients: \`fade:red-blue-black\`, \`fade:zebra-rainbow-zebra:direction\`

## Math

\`+\`, \`-\`, \`*\`, \`/\`, \`%\`, \`sin\`, \`cos\`, \`random\`, \`wiggle\`, \`min\`, \`max\`, \`abs\`, \`sqrt\`, \`pow\`, \`floor\`, \`ceil\`, \`round\`

## System: \`width\`/\`w\`, \`height\`/\`h\`, \`frame\`/\`f\`, \`clock\`, \`pi\`

## Control Flow

| Function | Usage | Description |
|----------|-------|-------------|
| \`def\` | \`(def x 10)\` | Variable |
| \`later\` | \`(later fn params body)\` | Function |
| \`if\` | \`(if cond then else)\` | Conditional |
| \`once\` | \`(once expr)\` | Run once |
| \`repeat\`/\`bunch\` | \`(repeat n i expr)\` | Loop |
| \`?\`/\`choose\` | \`(? a b c)\` | Random pick |
| \`...\` | \`(... a b c)\` | Cycle over time |

## Timing

- \`1s\` — after 1 second
- \`2s...\` — cycle every 2 seconds
- \`0.5s!\` — once after 0.5s
- \`0.1s\` — every 0.1 seconds

## Transforms

| Function | Usage |
|----------|-------|
| \`scroll\` | \`(scroll dx dy)\` — translate with wrapping |
| \`zoom\` | \`(zoom factor)\` — scale from center |
| \`spin\` | \`(spin angle)\` — rotate |
| \`suck\` | \`(suck strength)\` — vortex |
| \`blur\` | \`(blur amount)\` — gaussian blur |
| \`contrast\` | \`(contrast amount)\` |
| \`sort\` | \`(sort)\` — sort pixels |
| \`bake\` | \`(bake)\` — commit to background |

## Media & Text

\`paste\`, \`stamp\`, \`write\`, \`tape\`, \`painting\`, \`steal\`, \`putback\`

## Audio

\`mic\`, \`amplitude\`, \`melody\`, \`overtone\`, \`noise\`, \`speaker\`

## 3D

\`cube\`, \`form\`, \`trans\`, \`cubespin\`, \`cubepos\`, \`cubescale\`, \`camrot\`, \`camspin\`

## Embedding

\`($codeId)\` or \`($codeId x y w h alpha)\` — embed another piece
\`(jump "piece")\` or \`(jump $id)\` — navigate

## Best Practices

1. Start with a background — bare color or \`(wipe color)\`
2. Use \`?\` for randomness, \`...\` for cycling
3. Combine transforms for feedback loops (scroll + zoom + blur)
4. Use \`once\` for setup, \`bake\` for layering
5. Keep it minimal — many top hits are under 5 lines
`;

const PIECE_EXAMPLES = `# KidLisp Top Hit Examples

Real pieces from aesthetic.computer with 500+ views.

## "bop" — 7,388 views
purple, ink, line, blur 5

Pattern: Bare color background, random ink, random line, blur. Builds dreamy lines.

## "pie" — 4,702 views
(fps 24)
(0.25s (wipe (... red yellow blue)))
(ink green)
(line 0 height/2 width height/2)
(ink red)
(line width/2 0 width/2 height)
(scroll frame frame)

Pattern: ... cycles colors. scroll frame frame = diagonal motion.

## "roz" — 3,251 views (@jeffrey)
fade:red-blue-black-blue-red
ink (? rainbow white 0) (1s... 24 64)
line w/2 0 w/2 h
(spin (2s... -1.125 1.125)) (zoom 1.1)
(0.5s (contrast 1.05))
(scroll (? -0.1 0 0.1) (? -0.1 0 0.1))
ink (? cyan yellow magenta) 8
circle w/2 h/2 (? 2 4 8)

Pattern: Fade gradient, cycling spin, random ink alpha, center circle. Transforms accumulate.

## "ceo" — 3,087 views
(1s (coat fade:black-red-rainbow-red-black:frame 64)) (0.3s (zoom 0.5)) (scroll 1)

Pattern: coat overlay with animated fade. Periodic zoom-out = tunnel. Scroll = drift.

## "39i" — 2,049 views (@jeffrey)
black
(0.1s (ink (? black white) 32) (circle ? ? 32))
(ink (? white black gray) (? 32 96))
flood ? ?
scroll (2s... 1 0 -1) (3s... -1 0 1)
(1.5s (zoom (? 0.5 4)))
(1s (blur 0.5))
(2s (contrast 2))
(0.3s (ink (? white black)) (repeat 30 point))
(1s... (spin (0.5s... -0.1 0.1))

Pattern: Random circles + floods + aggressive transforms. Multi-speed timers.

## "cow" — 2,320 views (@jeffrey)
($39i 0 0 w h 128)
($r2f 0 0 w h 128)
(contrast 1.5)

Pattern: Embedded composition — layers two pieces with alpha transparency.

## "4bb" — 1,611 views
black, ink (? yellow black) 48, line, scroll 1
bake, ink (? magenta erase) 64, line, scroll -1
bake, ink (? lime erase) 16, line, scroll 0 1
bake, ink (? cyan erase) 48, line, scroll 0 -1
burn, blur 8, contrast 1.25

Pattern: Multi-layer bake. Four scrolling directions. erase ink = transparency holes.

## "reeb" — 1,337 views
red, ink erase, line, bake

Pattern: Red surface eroded by transparent random lines. Minimal.

## "otoc" — 1,181 views
(once noise) (spin (0.5s... 1 -1)) (suck 1) (contrast 1.01)

Pattern: Noise + spin + suck vortex + slow contrast. Minimal code, maximum effect.

## "inz" — 1,293 views (@jeffrey)
(beige)
(ink (0.25s... 127 0 rainbow))
(write X 3 3)
(scroll 18 3)
(blur 0.1)
(1.25s (zoom (? 0.25 1.5)))

Pattern: Text animation with scroll/blur/zoom jumps.

## Common Patterns

1. Feedback loops — scroll + zoom + blur = self-evolving trails
2. Weighted randomness — (? value value value rare) makes common values likely
3. Multi-speed timers — different intervals for layered rhythm
4. Bare word shortcuts — black, line, ink work without parens
5. erase ink — creates transparency holes
6. Embedded composition — $codeId layers pieces with alpha
7. Fade gradients — fade:color1-color2-color3
8. Minimal code — many top hits are under 5 lines
`;

// ── Main handler ──

export async function handler(event, context) {
  console.log(`📡 Remote MCP request: ${event.httpMethod} ${event.path}`);

  const corsHeaders = {
    "Access-Control-Allow-Origin": "*",
    "Access-Control-Allow-Methods": "GET, POST, OPTIONS",
    "Access-Control-Allow-Headers": "Content-Type, Authorization",
  };

  if (event.httpMethod === "OPTIONS") {
    return { statusCode: 204, headers: corsHeaders, body: "" };
  }

  try {
    const authToken = event.headers.authorization?.replace("Bearer ", "");

    if (event.httpMethod === "GET") {
      const headers = {
        ...corsHeaders,
        "Content-Type": "text/event-stream",
        "Cache-Control": "no-cache",
        Connection: "keep-alive",
      };

      const initMessage = JSON.stringify({
        jsonrpc: "2.0",
        method: "server/info",
        params: SERVER_INFO,
      });

      return { statusCode: 200, headers, body: `data: ${initMessage}\n\n` };
    }

    if (event.httpMethod === "POST") {
      const message = JSON.parse(event.body || "{}");
      console.log(`🔧 MCP method: ${message.method}`);

      const result = await handleMCPMessage(message, authToken);
      const response = { jsonrpc: "2.0", id: message.id, result };

      return respond(200, response, corsHeaders);
    }

    return respond(405, { error: "Method not allowed" }, corsHeaders);
  } catch (error) {
    console.error("❌ Remote MCP error:", error);
    return respond(
      500,
      { jsonrpc: "2.0", error: { code: -32603, message: error.message } },
      corsHeaders,
    );
  }
}
