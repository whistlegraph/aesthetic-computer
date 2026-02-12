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
      package: "@aesthetic-computer/mcp",
      install: "npx @aesthetic-computer/mcp",
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
      "args": ["-y", "@aesthetic-computer/mcp"],
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
  <style>
    body {
      font-family: sans-serif;
      background-color: rgb(235, 235, 235);
      -webkit-text-size-adjust: none;
      max-width: 800px;
      margin: 0 auto;
      padding: 20px;
      line-height: 1.6;
    }
    h1 { margin-bottom: 0.25em; }
    h2 { margin-top: 2em; margin-bottom: 0.5em; }
    h3 { margin-top: 1.5em; margin-bottom: 0.5em; font-weight: normal; }
    h4 { margin-top: 1em; margin-bottom: 0.5em; font-weight: normal; font-style: italic; }
    code, pre {
      font-family: monospace;
      font-weight: bold;
    }
    pre {
      background: white;
      padding: 1em;
      overflow-x: auto;
      border-left: 3px solid black;
      font-weight: normal;
    }
    .method {
      display: inline-block;
      background: black;
      color: white;
      padding: 0.15em 0.5em;
      font-weight: bold;
      font-size: 0.85em;
      margin-right: 0.5em;
    }
    .path {
      font-family: monospace;
      font-weight: bold;
    }
    .tabs {
      display: flex;
      gap: 0.25em;
      margin: 0.5em 0;
    }
    .tab {
      padding: 0.5em 1em;
      background: white;
      border: 1px solid black;
      cursor: pointer;
      font-size: 0.9em;
      font-weight: bold;
    }
    .tab.active { background: black; color: white; }
    .tab-content { display: none; }
    .tab-content.active { display: block; }
    .notes {
      background: white;
      padding: 1em;
      margin: 2em 0;
      border-left: 3px solid black;
    }
    .notes ul { margin-left: 1.5em; }
    .footer {
      text-align: center;
      margin-top: 3em;
      padding: 2em 0;
    }
    .footer a { color: black; }
    a { color: black; text-decoration: underline; }
    .version { font-style: italic; opacity: 0.7; }
  </style>
</head>
<body>
  <h1>${docs.title}</h1>
  <p class="version">Version ${docs.version}</p>
  <p>${docs.description}</p>
  <p><a href="?format=json">View as JSON</a></p>
  <hr>

  <h2>🤖 MCP Server</h2>
  <p><strong>${docs.mcp.title}:</strong> ${docs.mcp.description}</p>
  <p><strong>Package:</strong> <code>${docs.mcp.package}</code></p>
  <p><strong>Install:</strong> <code>${docs.mcp.install}</code></p>
  <p><a href="${docs.mcp.repository}">View on GitHub</a></p>

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

  <h3>Configuration Examples</h3>
  ${Object.entries(docs.mcp.configuration).map(([client, config]) => `
    <h4>${client}</h4>
    <pre>${escapeHTML(config)}</pre>
  `).join('')}

  <hr>

  <h2>📡 HTTP Endpoints</h2>

  ${docs.endpoints.map((endpoint, idx) => `
    <h2><span class="method">${endpoint.method}</span> ${endpoint.name}</h2>
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

        <h4>Response:</h4>
        <pre>${JSON.stringify(example.response.body, null, 2)}</pre>
      `).join('')}
  `).join('')}

  <hr>

  <div class="notes">
    <h3>Important Notes</h3>
    <ul>
      ${docs.notes.map(note => `<li>${note}</li>`).join('')}
    </ul>
  </div>

  <div class="footer">
    <p>
      ${docs.relatedResources.map(r =>
        `<a href="${r.url}" target="_blank">${r.name}</a>`
      ).join(' · ')}
    </p>
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
