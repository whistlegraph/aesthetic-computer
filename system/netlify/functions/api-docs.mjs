// API Documentation
// Returns LLM-friendly documentation of public aesthetic.computer APIs

import { respond } from "../../backend/http.mjs";

export async function handler(event, context) {
  const apiDocs = {
    title: "aesthetic.computer Public API",
    version: "1.0",
    description: "Public APIs for publishing creative works anonymously to aesthetic.computer",
    baseURL: "https://aesthetic.computer",

    mcp: {
      title: "MCP Server",
      description: "Model Context Protocol server for AI assistants (Claude, GPT-4, etc.) to interact with aesthetic.computer APIs",
      package: "@aesthetic.computer/mcp",
      install: "npx @aesthetic.computer/mcp",
      repository: "https://github.com/whistlegraph/aesthetic-computer/tree/main/mcp-server",

      tools: [
        {
          name: "publish_piece",
          description: "Publish a JavaScript piece to aesthetic.computer",
          input: { source: "string", name: "string (optional)" },
          output: { code: "string", url: "string", cached: "boolean" }
        },
        {
          name: "publish_kidlisp",
          description: "Publish KidLisp code to aesthetic.computer",
          input: { source: "string" },
          output: { code: "string", url: "string", cached: "boolean" }
        },
        {
          name: "publish_clock",
          description: "Publish a clock melody to aesthetic.computer",
          input: { source: "string" },
          output: { code: "string", url: "string", cached: "boolean" }
        },
        {
          name: "get_api_info",
          description: "Fetch the full API documentation",
          input: {},
          output: "API documentation object"
        }
      ],

      resources: [
        {
          uri: "aesthetic-computer://piece-template",
          description: "Starter template for a new aesthetic.computer piece with all lifecycle functions"
        },
        {
          uri: "aesthetic-computer://kidlisp-reference",
          description: "Quick reference guide for KidLisp syntax and common functions"
        }
      ],

      prompts: [
        {
          name: "create-piece",
          description: "Guided prompt for creating an aesthetic.computer piece",
          arguments: ["name (required)", "description (required)"]
        }
      ],

      configuration: {
        "Claude Desktop": `{
  "mcpServers": {
    "aesthetic-computer": {
      "command": "npx",
      "args": ["-y", "@aesthetic.computer/mcp"],
      "env": {
        "AC_TOKEN": "optional-bearer-token"
      }
    }
  }
}`,
        "Claude Code": `{
  "mcpServers": {
    "aesthetic-computer": {
      "command": "npx",
      "args": ["-y", "@aesthetic-computer/mcp"]
    }
  }
}`,
        "Cursor": `{
  "mcpServers": {
    "aesthetic-computer": {
      "command": "npx",
      "args": ["-y", "@aesthetic-computer/mcp"]
    }
  }
}`
      }
    },

    endpoints: [
      {
        name: "Store KidLisp Code",
        method: "POST",
        path: "/api/store-kidlisp",
        description: "Publish KidLisp code anonymously and get a short URL for sharing",
        authentication: "Optional (Bearer token for authenticated users)",
        requestBody: {
          contentType: "application/json",
          schema: {
            source: {
              type: "string",
              required: true,
              description: "KidLisp source code (max 50,000 characters)",
              example: "(wipe blue)\n(ink red)\n(box 10 10 50 50)"
            }
          }
        },
        responseBody: {
          schema: {
            code: {
              type: "string",
              description: "Short code for accessing the piece (e.g. 'abc123')"
            },
            cached: {
              type: "boolean",
              description: "True if code already existed (deduplication)"
            }
          }
        },
        examples: [
          {
            title: "Publish a KidLisp Piece",
            description: "Create a simple animated piece with KidLisp",
            curl: `curl -X POST https://aesthetic.computer/api/store-kidlisp \\
  -H "Content-Type: application/json" \\
  -d '{
    "source": "(wipe blue)\\n(ink yellow)\\n(circle (/ w 2) (/ h 2) 100)"
  }'`,
            javascript: `const response = await fetch("https://aesthetic.computer/api/store-kidlisp", {
  method: "POST",
  headers: { "Content-Type": "application/json" },
  body: JSON.stringify({
    source: "(wipe blue)\\n(ink yellow)\\n(circle (/ w 2) (/ h 2) 100)"
  })
});

const { code, cached } = await response.json();
console.log(\`View at: https://aesthetic.computer/\${code}\`);`,
            python: `import requests

response = requests.post(
    "https://aesthetic.computer/api/store-kidlisp",
    json={
        "source": "(wipe blue)\\n(ink yellow)\\n(circle (/ w 2) (/ h 2) 100)"
    }
)

data = response.json()
print(f"View at: https://aesthetic.computer/{data['code']}")`,
            response: {
              status: 201,
              body: {
                code: "xyz789",
                cached: false
              }
            }
          }
        ]
      },

      {
        name: "Store Clock Melody",
        method: "POST",
        path: "/api/store-clock",
        description: "Publish a clock melody string and get a pronounceable short code",
        authentication: "Optional (Bearer token for authenticated users)",
        requestBody: {
          contentType: "application/json",
          schema: {
            source: {
              type: "string",
              required: true,
              description: "Clock melody string (max 10,000 characters)",
              example: "c4 d4 e4 f4 g4"
            },
            melody: {
              type: "string",
              required: false,
              description: "Legacy field name (use 'source' instead)"
            }
          }
        },
        responseBody: {
          schema: {
            code: {
              type: "string",
              description: "Pronounceable short code (e.g. 'bako', 'milu')"
            },
            cached: {
              type: "boolean",
              description: "True if melody already existed (deduplication)"
            }
          }
        },
        examples: [
          {
            title: "Publish a Clock Melody",
            description: "Store a musical sequence for the clock piece",
            curl: `curl -X POST https://aesthetic.computer/api/store-clock \\
  -H "Content-Type: application/json" \\
  -d '{
    "source": "c4 e4 g4 c5 g4 e4 c4"
  }'`,
            javascript: `const response = await fetch("https://aesthetic.computer/api/store-clock", {
  method: "POST",
  headers: { "Content-Type": "application/json" },
  body: JSON.stringify({
    source: "c4 e4 g4 c5 g4 e4 c4"
  })
});

const { code, cached } = await response.json();
console.log(\`Listen at: https://aesthetic.computer/clock~\${code}\`);`,
            python: `import requests

response = requests.post(
    "https://aesthetic.computer/api/store-clock",
    json={
        "source": "c4 e4 g4 c5 g4 e4 c4"
    }
)

data = response.json()
print(f"Listen at: https://aesthetic.computer/clock~{data['code']}")`,
            response: {
              status: 201,
              body: {
                code: "bako",
                cached: false
              }
            }
          }
        ]
      },

      {
        name: "Store JavaScript Piece",
        method: "POST",
        path: "/api/store-piece",
        description: "Publish a JavaScript piece (.mjs) anonymously by providing source code as a string. No S3 credentials needed - the server handles storage automatically.",
        authentication: "Optional (Bearer token for authenticated users)",
        requestBody: {
          contentType: "application/json",
          schema: {
            source: {
              type: "string",
              required: true,
              description: "JavaScript piece source code (max 100,000 characters). Must contain at least one lifecycle function export.",
              example: "export function boot($) { $.wipe('blue'); }\nexport function paint($) { $.ink('red'); $.box(10, 10, 50, 50); }"
            },
            name: {
              type: "string",
              required: false,
              description: "Optional name for the piece (used for code generation)"
            }
          }
        },
        responseBody: {
          schema: {
            code: {
              type: "string",
              description: "Short code for accessing the piece (e.g. 'drift', 'wave')"
            },
            cached: {
              type: "boolean",
              description: "True if code already existed (deduplication)"
            },
            url: {
              type: "string",
              description: "Full URL to view the piece"
            }
          }
        },
        examples: [
          {
            title: "Publish a Simple Piece",
            description: "Create a piece with basic drawing",
            curl: `curl -X POST https://aesthetic.computer/api/store-piece \\
  -H "Content-Type: application/json" \\
  -d '{
    "source": "export function boot($) {\\n  $.wipe(\"blue\");\\n}\\n\\nexport function paint($) {\\n  $.ink(\"red\");\\n  $.box(10, 10, 50, 50);\\n}",
    "name": "red-box"
  }'`,
            javascript: `const source = \`export function boot($) {
  $.wipe("blue");
}

export function paint($) {
  $.ink("red");
  $.box(10, 10, 50, 50);
}\`;

const response = await fetch("https://aesthetic.computer/api/store-piece", {
  method: "POST",
  headers: { "Content-Type": "application/json" },
  body: JSON.stringify({
    source,
    name: "red-box"
  })
});

const { code, url, cached } = await response.json();
console.log(\`View at: \${url}\`);`,
            python: `import requests

source = """export function boot($) {
  $.wipe("blue");
}

export function paint($) {
  $.ink("red");
  $.box(10, 10, 50, 50);
}"""

response = requests.post(
    "https://aesthetic.computer/api/store-piece",
    json={
        "source": source,
        "name": "red-box"
    }
)

data = response.json()
print(f"View at: {data['url']}")`,
            response: {
              status: 201,
              body: {
                code: "red-box",
                cached: false,
                url: "https://aesthetic.computer/red-box"
              }
            }
          },
          {
            title: "Publish Interactive Piece",
            description: "Create a piece with user interaction",
            curl: `curl -X POST https://aesthetic.computer/api/store-piece \\
  -H "Content-Type: "application/json" \\
  -d '{
    "source": "let x = 0;\\n\\nexport function boot($) {\\n  x = $.screen.width / 2;\\n}\\n\\nexport function paint($) {\\n  $.wipe(\"black\");\\n  $.ink(\"yellow\");\\n  $.circle(x, $.screen.height / 2, 20);\\n}\\n\\nexport function act($) {\\n  if ($.event.is(\"touch\")) x = $.event.x;\\n}"
  }'`,
            javascript: `const source = \`let x = 0;

export function boot($) {
  x = $.screen.width / 2;
}

export function paint($) {
  $.wipe("black");
  $.ink("yellow");
  $.circle(x, $.screen.height / 2, 20);
}

export function act($) {
  if ($.event.is("touch")) x = $.event.x;
}\`;

const response = await fetch("https://aesthetic.computer/api/store-piece", {
  method: "POST",
  headers: { "Content-Type": "application/json" },
  body: JSON.stringify({ source })
});

const { code, url } = await response.json();
console.log(\`View at: \${url}\`);`,
            python: `import requests

source = """let x = 0;

export function boot($) {
  x = $.screen.width / 2;
}

export function paint($) {
  $.wipe("black");
  $.ink("yellow");
  $.circle(x, $.screen.height / 2, 20);
}

export function act($) {
  if ($.event.is("touch")) x = $.event.x;
}"""

response = requests.post(
    "https://aesthetic.computer/api/store-piece",
    json={"source": source}
)

data = response.json()
print(f"View at: {data['url']}")`,
            response: {
              status: 201,
              body: {
                code: "touch",
                cached: false,
                url: "https://aesthetic.computer/touch"
              }
            }
          }
        ]
      },

      {
        name: "Track Media (Publish Artwork)",
        method: "POST",
        path: "/api/track-media",
        description: "Publish a painting (PNG), JavaScript piece (MJS), or recording tape (ZIP) anonymously. Note: Files must be uploaded to S3/storage before calling this endpoint.",
        authentication: "Optional (Bearer token for authenticated users)",
        requestBody: {
          contentType: "application/json",
          schema: {
            slug: {
              type: "string",
              required: true,
              description: "S3/storage path where the file was uploaded"
            },
            ext: {
              type: "string",
              required: true,
              enum: ["png", "mjs", "zip"],
              description: "File extension: 'png' for paintings, 'mjs' for JavaScript pieces, 'zip' for tapes"
            },
            metadata: {
              type: "object",
              required: false,
              description: "Optional metadata (for tapes: totalDuration in seconds, max 30s)",
              properties: {
                totalDuration: {
                  type: "number",
                  description: "Duration in seconds (tapes only, max 30)"
                }
              }
            }
          }
        },
        responseBody: {
          schema: {
            code: {
              type: "string",
              description: "Short code for accessing the media"
            }
          }
        },
        examples: [
          {
            title: "Publish a JavaScript Piece",
            description: "After uploading .mjs file to S3, register it in the database",
            curl: `curl -X POST https://aesthetic.computer/api/track-media \\
  -H "Content-Type: application/json" \\
  -d '{
    "slug": "2026/02/12/my-piece.mjs",
    "ext": "mjs"
  }'`,
            javascript: `// Step 1: Upload your .mjs file to S3 (requires credentials)
// Step 2: Register the uploaded file
const response = await fetch("https://aesthetic.computer/api/track-media", {
  method: "POST",
  headers: { "Content-Type": "application/json" },
  body: JSON.stringify({
    slug: "2026/02/12/my-piece.mjs",
    ext: "mjs"
  })
});

const { code } = await response.json();
console.log(\`View at: https://aesthetic.computer/\${code}\`);`,
            python: `import requests

# After uploading your .mjs file to S3
response = requests.post(
    "https://aesthetic.computer/api/track-media",
    json={
        "slug": "2026/02/12/my-piece.mjs",
        "ext": "mjs"
    }
)

data = response.json()
print(f"View at: https://aesthetic.computer/{data['code']}")`,
            response: {
              status: 200,
              body: {
                code: "abc456"
              }
            }
          },
          {
            title: "Publish a Painting (PNG)",
            description: "Register a painting image after uploading to S3",
            curl: `curl -X POST https://aesthetic.computer/api/track-media \\
  -H "Content-Type: application/json" \\
  -d '{
    "slug": "2026/02/12/my-painting.png",
    "ext": "png"
  }'`,
            javascript: `const response = await fetch("https://aesthetic.computer/api/track-media", {
  method: "POST",
  headers: { "Content-Type": "application/json" },
  body: JSON.stringify({
    slug: "2026/02/12/my-painting.png",
    ext: "png"
  })
});

const { code } = await response.json();
console.log(\`View at: https://aesthetic.computer/\${code}\`);`,
            python: `import requests

response = requests.post(
    "https://aesthetic.computer/api/track-media",
    json={
        "slug": "2026/02/12/my-painting.png",
        "ext": "png"
    }
)

data = response.json()
print(f"View at: https://aesthetic.computer/{data['code']}")`,
            response: {
              status: 200,
              body: {
                code: "def789"
              }
            }
          },
          {
            title: "Publish a Recording Tape (ZIP)",
            description: "Register a recording after uploading ZIP to S3",
            curl: `curl -X POST https://aesthetic.computer/api/track-media \\
  -H "Content-Type: application/json" \\
  -d '{
    "slug": "2026/02/12/my-recording.zip",
    "ext": "zip",
    "metadata": {
      "totalDuration": 15.5
    }
  }'`,
            javascript: `const response = await fetch("https://aesthetic.computer/api/track-media", {
  method: "POST",
  headers: { "Content-Type": "application/json" },
  body: JSON.stringify({
    slug: "2026/02/12/my-recording.zip",
    ext: "zip",
    metadata: {
      totalDuration: 15.5  // seconds (max 30)
    }
  })
});

const { code } = await response.json();
console.log(\`Watch at: https://aesthetic.computer/\${code}\`);`,
            python: `import requests

response = requests.post(
    "https://aesthetic.computer/api/track-media",
    json={
        "slug": "2026/02/12/my-recording.zip",
        "ext": "zip",
        "metadata": {
            "totalDuration": 15.5  # seconds (max 30)
        }
    }
)

data = response.json()
print(f"Watch at: https://aesthetic.computer/{data['code']}")`,
            response: {
              status: 200,
              body: {
                code: "ghi012"
              }
            }
          }
        ]
      }
    ],

    notes: [
      "✨ All endpoints support anonymous (guest) publishing without authentication",
      "🔑 To associate uploads with your account, include a Bearer token in the Authorization header",
      "🎨 KidLisp is a creative coding language - visit https://kidlisp.com for documentation",
      "💾 /api/store-piece handles storage automatically - no S3 credentials needed",
      "📦 For /api/track-media: Files must be uploaded to S3/storage first (contact admins for credentials)",
      "📏 Maximum source code lengths: KidLisp 50,000 chars, JavaScript pieces 100,000 chars",
      "⏱️ Maximum clock melody length: 10,000 characters",
      "🎬 Maximum tape duration: 30 seconds",
      "♻️ Duplicate content is automatically deduplicated (same content returns same code)",
      "🔧 JavaScript pieces must export at least one lifecycle function: boot, paint, sim, or act"
    ],

    relatedResources: [
      {
        name: "KidLisp Documentation",
        url: "https://kidlisp.com"
      },
      {
        name: "aesthetic.computer Main Site",
        url: "https://aesthetic.computer"
      }
    ]
  };

  // Content negotiation: HTML for browsers, JSON for APIs/LLMs
  const acceptHeader = event.headers?.accept || "";
  const format = event.queryStringParameters?.format;

  // Explicit format parameter takes precedence
  const wantsHTML = format === "html" ||
                    (!format && acceptHeader.includes("text/html"));

  if (wantsHTML) {
    // Serve HTML documentation for browsers
    const html = generateHTML(apiDocs);
    return respond(200, html, {
      "Content-Type": "text/html",
      "Access-Control-Allow-Origin": "*"
    });
  }

  // Default: Return as pretty-printed JSON for LLMs/APIs
  return respond(200, apiDocs, {
    "Content-Type": "application/json",
    "Access-Control-Allow-Origin": "*"
  });
}

function generateHTML(docs) {
  return `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>${docs.title}</title>
  <link rel="icon" href="https://aesthetic.computer/favicon.ico" type="image/x-icon">
  <style>
    * { box-sizing: border-box; }
    body {
      font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif;
      background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
      color: #1a1a1a;
      margin: 0;
      padding: 0;
      line-height: 1.7;
      -webkit-font-smoothing: antialiased;
    }
    .container {
      max-width: 900px;
      margin: 0 auto;
      padding: 20px;
    }
    .header {
      background: rgba(255, 255, 255, 0.98);
      backdrop-filter: blur(10px);
      border-radius: 16px;
      padding: 40px;
      margin-bottom: 30px;
      box-shadow: 0 20px 60px rgba(0, 0, 0, 0.3);
      text-align: center;
    }
    .logo {
      font-size: 48px;
      margin-bottom: 20px;
      filter: drop-shadow(0 4px 8px rgba(0,0,0,0.1));
    }
    h1 {
      margin: 0 0 10px 0;
      font-size: 2.5em;
      font-weight: 800;
      background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
      -webkit-background-clip: text;
      -webkit-text-fill-color: transparent;
      background-clip: text;
    }
    .tagline {
      font-size: 1.1em;
      color: #666;
      margin-bottom: 20px;
    }
    .version {
      display: inline-block;
      background: #f0f0f0;
      padding: 4px 12px;
      border-radius: 20px;
      font-size: 0.85em;
      color: #666;
      margin-bottom: 15px;
    }
    .quick-links {
      display: flex;
      gap: 10px;
      justify-content: center;
      flex-wrap: wrap;
    }
    .btn {
      display: inline-block;
      padding: 10px 20px;
      background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
      color: white;
      text-decoration: none;
      border-radius: 8px;
      font-weight: 600;
      transition: transform 0.2s, box-shadow 0.2s;
    }
    .btn:hover {
      transform: translateY(-2px);
      box-shadow: 0 8px 20px rgba(102, 126, 234, 0.4);
    }
    .btn-secondary {
      background: white;
      color: #667eea;
      border: 2px solid #667eea;
    }
    .section {
      background: rgba(255, 255, 255, 0.98);
      backdrop-filter: blur(10px);
      border-radius: 16px;
      padding: 30px;
      margin-bottom: 20px;
      box-shadow: 0 10px 40px rgba(0, 0, 0, 0.2);
    }
    h2 {
      margin-top: 0;
      font-size: 1.8em;
      font-weight: 700;
      color: #2d3748;
      border-bottom: 3px solid #667eea;
      padding-bottom: 10px;
    }
    h3 {
      font-size: 1.3em;
      font-weight: 600;
      color: #4a5568;
      margin-top: 1.5em;
    }
    h4 {
      font-size: 1.1em;
      font-weight: 500;
      color: #718096;
      font-style: italic;
    }
    code {
      font-family: "SF Mono", Monaco, "Cascadia Code", "Roboto Mono", Consolas, "Courier New", monospace;
      background: #f7fafc;
      padding: 2px 6px;
      border-radius: 4px;
      font-size: 0.9em;
      color: #d73a49;
      font-weight: 600;
    }
    pre {
      background: #1a202c;
      color: #e2e8f0;
      padding: 20px;
      border-radius: 8px;
      overflow-x: auto;
      font-size: 0.9em;
      line-height: 1.5;
      box-shadow: inset 0 2px 8px rgba(0, 0, 0, 0.3);
    }
    pre code {
      background: transparent;
      padding: 0;
      color: #e2e8f0;
      font-weight: normal;
    }
    .method {
      display: inline-block;
      background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
      color: white;
      padding: 4px 12px;
      border-radius: 6px;
      font-weight: 700;
      font-size: 0.85em;
      margin-right: 8px;
      text-transform: uppercase;
      letter-spacing: 0.5px;
    }
    .path {
      font-family: "SF Mono", Monaco, monospace;
      font-weight: 600;
      color: #667eea;
    }
    .tabs {
      display: flex;
      gap: 4px;
      margin: 15px 0 10px 0;
      border-bottom: 2px solid #e2e8f0;
    }
    .tab {
      padding: 10px 20px;
      background: transparent;
      border: none;
      cursor: pointer;
      font-size: 0.95em;
      font-weight: 600;
      color: #718096;
      border-bottom: 3px solid transparent;
      transition: all 0.2s;
    }
    .tab:hover { color: #667eea; }
    .tab.active {
      color: #667eea;
      border-bottom-color: #667eea;
    }
    .tab-content { display: none; margin-top: 10px; }
    .tab-content.active { display: block; }
    .notes {
      background: linear-gradient(135deg, #fef5e7 0%, #fdebd0 100%);
      border-left: 4px solid #f39c12;
      padding: 20px;
      border-radius: 8px;
      margin: 20px 0;
    }
    .notes h3 { margin-top: 0; color: #e67e22; }
    .notes ul { margin: 10px 0 0 20px; }
    .notes li { margin-bottom: 8px; }
    .footer {
      text-align: center;
      margin-top: 40px;
      padding: 30px 0;
      color: rgba(255, 255, 255, 0.9);
    }
    .footer a {
      color: white;
      text-decoration: none;
      font-weight: 600;
      padding: 8px 16px;
      background: rgba(255, 255, 255, 0.2);
      border-radius: 6px;
      margin: 0 5px;
      transition: background 0.2s;
    }
    .footer a:hover { background: rgba(255, 255, 255, 0.3); }
    a { color: #667eea; text-decoration: none; font-weight: 600; }
    a:hover { text-decoration: underline; }
    .endpoint-card {
      border-left: 4px solid #667eea;
      padding-left: 20px;
      margin-bottom: 40px;
    }
    @media (max-width: 768px) {
      .header { padding: 30px 20px; }
      h1 { font-size: 2em; }
      .section { padding: 20px; }
      .tabs { flex-wrap: wrap; }
    }
  </style>
</head>
<body>
  <div class="container">
    <div class="header">
      <div class="logo">🎨💻✨</div>
      <h1>${docs.title}</h1>
      <p class="tagline">${docs.description}</p>
      <div class="version">v${docs.version}</div>
      <div class="quick-links">
        <a href="?format=json" class="btn btn-secondary">📄 View as JSON</a>
        <a href="https://aesthetic.computer" class="btn">🏠 Go to Site</a>
        <a href="https://github.com/whistlegraph/aesthetic-computer" class="btn">⚡ GitHub</a>
      </div>
    </div>

    <div class="section">
      <h2>🤖 MCP Server</h2>
      <p><strong>${docs.mcp.title}:</strong> ${docs.mcp.description}</p>
      <p><strong>📦 Package:</strong> <code>${docs.mcp.package}</code></p>
      <p><strong>⚡ Install:</strong> <code>${docs.mcp.install}</code></p>
      <p><a href="${docs.mcp.repository}">📂 View on GitHub →</a></p>

  <h3>Tools</h3>
  ${docs.mcp.tools.map(tool => `
    <p><strong><code>${tool.name}</code></strong> — ${tool.description}</p>
    <p>Input: <code>${JSON.stringify(tool.input)}</code></p>
    <p>Output: <code>${typeof tool.output === 'string' ? tool.output : JSON.stringify(tool.output)}</code></p>
  `).join('')}

  <h3>Resources</h3>
  ${docs.mcp.resources.map(resource => `
    <p><strong><code>${resource.uri}</code></strong> — ${resource.description}</p>
  `).join('')}

  <h3>Prompts</h3>
  ${docs.mcp.prompts.map(prompt => `
    <p><strong><code>${prompt.name}</code></strong> — ${prompt.description}</p>
    <p>Arguments: ${prompt.arguments.join(', ')}</p>
  `).join('')}

      <h3>⚙️ Configuration Examples</h3>
      ${Object.entries(docs.mcp.configuration).map(([client, config]) => `
        <h4>${client}</h4>
        <pre><code>${escapeHTML(config)}</code></pre>
      `).join('')}
    </div>

    <div class="section">
      <h2>📡 HTTP Endpoints</h2>

      ${docs.endpoints.map((endpoint, idx) => `
        <div class="endpoint-card">
          <h3><span class="method">${endpoint.method}</span> ${endpoint.name}</h3>
          <p><code class="path">${docs.baseURL}${endpoint.path}</code></p>
          <p>${endpoint.description}</p>

      ${endpoint.examples.map((example, exIdx) => `
        <h3>${example.title}</h3>
        <p>${example.description}</p>

        <div class="tabs" id="tabs-${idx}-${exIdx}">
          <button class="tab active" onclick="showTab(${idx}, ${exIdx}, 'curl')">cURL</button>
          <button class="tab" onclick="showTab(${idx}, ${exIdx}, 'js')">JavaScript</button>
          <button class="tab" onclick="showTab(${idx}, ${exIdx}, 'py')">Python</button>
        </div>

        <div class="tab-content active" id="content-${idx}-${exIdx}-curl">
          <pre>${escapeHTML(example.curl)}</pre>
        </div>
        <div class="tab-content" id="content-${idx}-${exIdx}-js">
          <pre>${escapeHTML(example.javascript)}</pre>
        </div>
        <div class="tab-content" id="content-${idx}-${exIdx}-py">
          <pre>${escapeHTML(example.python)}</pre>
        </div>

          <h4>📬 Response:</h4>
          <pre><code>${JSON.stringify(example.response.body, null, 2)}</code></pre>
        `).join('')}
        </div>
      `).join('')}
    </div>

    <div class="notes">
      <h3>📝 Important Notes</h3>
      <ul>
        ${docs.notes.map(note => `<li>${note}</li>`).join('')}
      </ul>
    </div>

    <div class="footer">
      <p>
        ${docs.relatedResources.map(r =>
          `<a href="${r.url}" target="_blank">${r.name}</a>`
        ).join(' ')}
      </p>
      <p style="margin-top: 20px; opacity: 0.8;">
        Made with 💜 by <a href="https://aesthetic.computer" style="background: transparent;">aesthetic.computer</a>
      </p>
    </div>
  </div>

  <script>
    function showTab(endpointIdx, exampleIdx, lang) {
      const tabsContainer = document.getElementById(\`tabs-\${endpointIdx}-\${exampleIdx}\`);
      const tabs = tabsContainer.querySelectorAll('.tab');
      const contents = ['curl', 'js', 'py'];

      tabs.forEach((tab, i) => {
        tab.classList.remove('active');
        const contentId = \`content-\${endpointIdx}-\${exampleIdx}-\${contents[i]}\`;
        const content = document.getElementById(contentId);
        if (content) content.classList.remove('active');
      });

      const activeTab = Array.from(tabs).find(t =>
        t.textContent.toLowerCase().includes(lang === 'js' ? 'javascript' : lang)
      );
      if (activeTab) activeTab.classList.add('active');

      const activeContent = document.getElementById(\`content-\${endpointIdx}-\${exampleIdx}-\${lang}\`);
      if (activeContent) activeContent.classList.add('active');
    }
  </script>
</body>
</html>`;
}

function escapeHTML(str) {
  return str
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;')
    .replace(/'/g, '&#039;');
}
