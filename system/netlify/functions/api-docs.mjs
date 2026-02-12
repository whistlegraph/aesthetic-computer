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
            url: {
              type: "string",
              description: "Full URL to view the piece (e.g. 'https://aesthetic.computer/abc123')"
            }
          }
        },
        examples: [
          {
            title: "Anonymous KidLisp Piece",
            request: {
              method: "POST",
              url: "https://aesthetic.computer/api/store-kidlisp",
              headers: {
                "Content-Type": "application/json"
              },
              body: {
                source: "(wipe blue)\n(ink yellow)\n(circle (/ w 2) (/ h 2) 100)"
              }
            },
            response: {
              status: 200,
              body: {
                code: "xyz789",
                url: "https://aesthetic.computer/xyz789"
              }
            }
          }
        ]
      },

      {
        name: "Track Media (Publish Artwork)",
        method: "POST",
        path: "/api/track-media",
        description: "Publish a painting (PNG), JavaScript piece (MJS), or recording tape (ZIP) anonymously",
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
            },
            url: {
              type: "string",
              description: "Full URL to view the media"
            }
          }
        },
        examples: [
          {
            title: "Publish a JavaScript Piece",
            request: {
              method: "POST",
              url: "https://aesthetic.computer/api/track-media",
              headers: {
                "Content-Type": "application/json"
              },
              body: {
                slug: "2025/02/12/my-piece.mjs",
                ext: "mjs"
              }
            },
            response: {
              status: 200,
              body: {
                code: "abc456",
                url: "https://aesthetic.computer/abc456"
              }
            }
          },
          {
            title: "Publish a Painting (PNG)",
            request: {
              method: "POST",
              url: "https://aesthetic.computer/api/track-media",
              headers: {
                "Content-Type": "application/json"
              },
              body: {
                slug: "2025/02/12/my-painting.png",
                ext: "png"
              }
            },
            response: {
              status: 200,
              body: {
                code: "def789",
                url: "https://aesthetic.computer/def789"
              }
            }
          }
        ]
      }
    ],

    notes: [
      "All endpoints support anonymous (guest) uploads without authentication",
      "To associate uploads with your account, include a Bearer token in the Authorization header",
      "KidLisp is a creative coding language - visit https://kidlisp.com for documentation",
      "Files must be uploaded to S3/storage before calling /api/track-media (contact admins for upload credentials)",
      "Maximum KidLisp source code length is 50,000 characters",
      "Maximum tape duration is 30 seconds"
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
