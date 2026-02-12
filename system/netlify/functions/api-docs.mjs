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

  // Return as pretty-printed JSON for readability
  return respond(200, apiDocs, {
    "Content-Type": "application/json",
    "Access-Control-Allow-Origin": "*"
  });
}
