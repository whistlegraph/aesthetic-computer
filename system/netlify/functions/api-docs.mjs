// API Documentation
// Returns LLM-friendly documentation of public aesthetic.computer APIs

import { respond } from "../../backend/http.mjs";

export async function handler(event, context) {
  const apiDocs = {
    title: "aesthetic.computer Public API",
    version: "1.0",
    description: "Public APIs for publishing creative works anonymously to aesthetic.computer",
    baseURL: "https://aesthetic.computer",

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
      "📦 For /api/track-media: Files must be uploaded to S3/storage first (contact admins for credentials)",
      "📏 Maximum KidLisp source code length: 50,000 characters",
      "⏱️ Maximum clock melody length: 10,000 characters",
      "🎬 Maximum tape duration: 30 seconds",
      "♻️ Duplicate content is automatically deduplicated (same content returns same code)"
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
    * { margin: 0; padding: 0; box-sizing: border-box; }
    body {
      font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
      line-height: 1.6;
      color: #333;
      max-width: 1200px;
      margin: 0 auto;
      padding: 2rem;
      background: #f5f5f5;
    }
    header {
      background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
      color: white;
      padding: 2rem;
      border-radius: 8px;
      margin-bottom: 2rem;
    }
    h1 { font-size: 2.5rem; margin-bottom: 0.5rem; }
    .version { opacity: 0.9; font-size: 0.9rem; }
    .description { margin-top: 1rem; font-size: 1.1rem; }
    .endpoint {
      background: white;
      border-radius: 8px;
      padding: 2rem;
      margin-bottom: 2rem;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    }
    .endpoint h2 {
      color: #667eea;
      margin-bottom: 0.5rem;
      display: flex;
      align-items: center;
      gap: 1rem;
    }
    .method {
      background: #667eea;
      color: white;
      padding: 0.25rem 0.75rem;
      border-radius: 4px;
      font-size: 0.9rem;
      font-weight: bold;
    }
    .path {
      font-family: 'Courier New', monospace;
      background: #f0f0f0;
      padding: 0.5rem 1rem;
      border-radius: 4px;
      margin: 1rem 0;
      font-size: 0.95rem;
    }
    .example {
      background: #f8f8f8;
      border-left: 4px solid #667eea;
      padding: 1rem;
      margin: 1rem 0;
      border-radius: 4px;
    }
    .example h4 { color: #667eea; margin-bottom: 0.5rem; }
    pre {
      background: #2d2d2d;
      color: #f8f8f2;
      padding: 1rem;
      border-radius: 4px;
      overflow-x: auto;
      margin: 0.5rem 0;
      font-size: 0.85rem;
    }
    .tabs {
      display: flex;
      gap: 0.5rem;
      margin-bottom: 0.5rem;
    }
    .tab {
      padding: 0.5rem 1rem;
      background: #e0e0e0;
      border: none;
      border-radius: 4px 4px 0 0;
      cursor: pointer;
      font-size: 0.9rem;
    }
    .tab.active { background: #2d2d2d; color: white; }
    .tab-content { display: none; }
    .tab-content.active { display: block; }
    .notes {
      background: #fff3cd;
      border-left: 4px solid #ffc107;
      padding: 1rem;
      border-radius: 4px;
      margin: 2rem 0;
    }
    .notes h3 { color: #856404; margin-bottom: 0.5rem; }
    .notes ul { margin-left: 1.5rem; }
    .footer {
      text-align: center;
      margin-top: 3rem;
      padding: 2rem;
      color: #666;
    }
    .footer a { color: #667eea; text-decoration: none; }
    .footer a:hover { text-decoration: underline; }
    .json-link {
      display: inline-block;
      margin-top: 1rem;
      padding: 0.5rem 1rem;
      background: rgba(255,255,255,0.2);
      border-radius: 4px;
      color: white;
      text-decoration: none;
      font-size: 0.9rem;
    }
    .json-link:hover { background: rgba(255,255,255,0.3); }
  </style>
</head>
<body>
  <header>
    <h1>${docs.title}</h1>
    <div class="version">Version ${docs.version}</div>
    <div class="description">${docs.description}</div>
    <a href="?format=json" class="json-link">📄 View as JSON</a>
  </header>

  ${docs.endpoints.map((endpoint, idx) => `
    <div class="endpoint">
      <h2>
        <span class="method">${endpoint.method}</span>
        ${endpoint.name}
      </h2>
      <div class="path">${docs.baseURL}${endpoint.path}</div>
      <p>${endpoint.description}</p>

      ${endpoint.examples.map((example, exIdx) => `
        <div class="example">
          <h4>${example.title}</h4>
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

          <h4 style="margin-top: 1rem;">Response:</h4>
          <pre>${JSON.stringify(example.response.body, null, 2)}</pre>
        </div>
      `).join('')}
    </div>
  `).join('')}

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
