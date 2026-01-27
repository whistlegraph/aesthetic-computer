// 🔗 Open Graph Preview Fetcher
// Fetches og:title and og:image from a URL for link previews in chat

export const config = { path: "/api/og-preview" };

// Simple in-memory cache with TTL (1 hour)
const cache = new Map();
const CACHE_TTL = 60 * 60 * 1000; // 1 hour

function getCached(url) {
  const entry = cache.get(url);
  if (!entry) return null;
  if (Date.now() - entry.timestamp > CACHE_TTL) {
    cache.delete(url);
    return null;
  }
  return entry.data;
}

function setCache(url, data) {
  // Limit cache size to prevent memory issues
  if (cache.size > 1000) {
    // Delete oldest entries
    const entries = [...cache.entries()].sort((a, b) => a[1].timestamp - b[1].timestamp);
    for (let i = 0; i < 100; i++) {
      cache.delete(entries[i][0]);
    }
  }
  cache.set(url, { data, timestamp: Date.now() });
}

export default async function handler(req) {
  // Handle CORS preflight
  if (req.method === "OPTIONS") {
    return new Response(null, {
      status: 204,
      headers: {
        "Access-Control-Allow-Origin": "*",
        "Access-Control-Allow-Methods": "GET, OPTIONS",
        "Access-Control-Allow-Headers": "Content-Type",
      },
    });
  }

  // Only allow GET requests
  if (req.method !== "GET") {
    return new Response(JSON.stringify({ error: "Method not allowed" }), {
      status: 405,
      headers: { "Content-Type": "application/json" },
    });
  }

  const url = new URL(req.url);
  const targetUrl = url.searchParams.get("url");

  if (!targetUrl) {
    return new Response(JSON.stringify({ error: "Missing 'url' parameter" }), {
      status: 400,
      headers: {
        "Content-Type": "application/json",
        "Access-Control-Allow-Origin": "*",
      },
    });
  }

  // Validate URL
  let parsedUrl;
  try {
    parsedUrl = new URL(targetUrl);
    if (!["http:", "https:"].includes(parsedUrl.protocol)) {
      throw new Error("Invalid protocol");
    }
  } catch {
    return new Response(JSON.stringify({ error: "Invalid URL" }), {
      status: 400,
      headers: {
        "Content-Type": "application/json",
        "Access-Control-Allow-Origin": "*",
      },
    });
  }

  // Check cache
  const cached = getCached(targetUrl);
  if (cached) {
    return new Response(JSON.stringify(cached), {
      status: 200,
      headers: {
        "Content-Type": "application/json",
        "Access-Control-Allow-Origin": "*",
        "Cache-Control": "public, max-age=3600",
      },
    });
  }

  try {
    // Fetch the page with a reasonable timeout
    const controller = new AbortController();
    const timeout = setTimeout(() => controller.abort(), 5000);

    const response = await fetch(targetUrl, {
      signal: controller.signal,
      headers: {
        "User-Agent": "Mozilla/5.0 (compatible; AestheticComputer/1.0; +https://aesthetic.computer)",
        "Accept": "text/html,application/xhtml+xml",
      },
    });

    clearTimeout(timeout);

    if (!response.ok) {
      return new Response(JSON.stringify({ error: "Failed to fetch URL", status: response.status }), {
        status: 502,
        headers: {
          "Content-Type": "application/json",
          "Access-Control-Allow-Origin": "*",
        },
      });
    }

    // Only process HTML content
    const contentType = response.headers.get("content-type") || "";
    if (!contentType.includes("text/html")) {
      const result = { 
        url: targetUrl, 
        title: null, 
        image: null, 
        description: null,
        siteName: null,
        favicon: null,
      };
      setCache(targetUrl, result);
      return new Response(JSON.stringify(result), {
        status: 200,
        headers: {
          "Content-Type": "application/json",
          "Access-Control-Allow-Origin": "*",
          "Cache-Control": "public, max-age=3600",
        },
      });
    }

    // Read only the first 50KB to find meta tags (they're usually in <head>)
    const reader = response.body.getReader();
    const decoder = new TextDecoder();
    let html = "";
    let bytesRead = 0;
    const maxBytes = 50 * 1024;

    while (bytesRead < maxBytes) {
      const { done, value } = await reader.read();
      if (done) break;
      html += decoder.decode(value, { stream: true });
      bytesRead += value.length;
      
      // Stop early if we've found </head>
      if (html.includes("</head>")) break;
    }
    
    reader.cancel();

    // Parse Open Graph and other meta tags
    const result = parseMetaTags(html, targetUrl);
    setCache(targetUrl, result);

    return new Response(JSON.stringify(result), {
      status: 200,
      headers: {
        "Content-Type": "application/json",
        "Access-Control-Allow-Origin": "*",
        "Cache-Control": "public, max-age=3600",
      },
    });
  } catch (err) {
    const errorMessage = err.name === "AbortError" ? "Request timed out" : err.message;
    return new Response(JSON.stringify({ error: errorMessage }), {
      status: 500,
      headers: {
        "Content-Type": "application/json",
        "Access-Control-Allow-Origin": "*",
      },
    });
  }
}

// Parse meta tags from HTML to extract OG data
function parseMetaTags(html, baseUrl) {
  const result = {
    url: baseUrl,
    title: null,
    image: null,
    description: null,
    siteName: null,
    favicon: null,
  };

  // Helper to extract meta content
  const getMetaContent = (nameOrProperty) => {
    // Match both name="" and property="" attributes
    const patterns = [
      new RegExp(`<meta[^>]*property=["']${nameOrProperty}["'][^>]*content=["']([^"']+)["']`, "i"),
      new RegExp(`<meta[^>]*content=["']([^"']+)["'][^>]*property=["']${nameOrProperty}["']`, "i"),
      new RegExp(`<meta[^>]*name=["']${nameOrProperty}["'][^>]*content=["']([^"']+)["']`, "i"),
      new RegExp(`<meta[^>]*content=["']([^"']+)["'][^>]*name=["']${nameOrProperty}["']`, "i"),
    ];
    
    for (const pattern of patterns) {
      const match = html.match(pattern);
      if (match) return decodeHtmlEntities(match[1]);
    }
    return null;
  };

  // Try og:title first, then twitter:title, then <title>
  result.title = getMetaContent("og:title") 
    || getMetaContent("twitter:title")
    || extractTitle(html);

  // Try og:image first, then twitter:image
  result.image = getMetaContent("og:image") 
    || getMetaContent("twitter:image");
  
  // Make image URL absolute
  if (result.image && !result.image.startsWith("http")) {
    try {
      result.image = new URL(result.image, baseUrl).href;
    } catch {
      result.image = null;
    }
  }

  // Description
  result.description = getMetaContent("og:description")
    || getMetaContent("twitter:description")
    || getMetaContent("description");

  // Site name
  result.siteName = getMetaContent("og:site_name");

  // Favicon - try various common patterns
  const faviconPatterns = [
    /<link[^>]*rel=["'](?:shortcut )?icon["'][^>]*href=["']([^"']+)["']/i,
    /<link[^>]*href=["']([^"']+)["'][^>]*rel=["'](?:shortcut )?icon["']/i,
    /<link[^>]*rel=["']apple-touch-icon["'][^>]*href=["']([^"']+)["']/i,
  ];

  for (const pattern of faviconPatterns) {
    const match = html.match(pattern);
    if (match) {
      let favicon = match[1];
      if (!favicon.startsWith("http")) {
        try {
          favicon = new URL(favicon, baseUrl).href;
        } catch {
          continue;
        }
      }
      result.favicon = favicon;
      break;
    }
  }

  // Default favicon fallback
  if (!result.favicon) {
    try {
      result.favicon = new URL("/favicon.ico", baseUrl).href;
    } catch {
      // Ignore
    }
  }

  return result;
}

// Extract <title> content
function extractTitle(html) {
  const match = html.match(/<title[^>]*>([^<]+)<\/title>/i);
  return match ? decodeHtmlEntities(match[1].trim()) : null;
}

// Decode common HTML entities
function decodeHtmlEntities(str) {
  return str
    .replace(/&amp;/g, "&")
    .replace(/&lt;/g, "<")
    .replace(/&gt;/g, ">")
    .replace(/&quot;/g, '"')
    .replace(/&#39;/g, "'")
    .replace(/&#x27;/g, "'")
    .replace(/&#x2F;/g, "/")
    .replace(/&nbsp;/g, " ");
}
