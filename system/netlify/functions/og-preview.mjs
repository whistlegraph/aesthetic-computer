// 🔗 Open Graph Preview Fetcher
// Fetches og:title and og:image from a URL for link previews in chat

export const config = { path: "/api/og-preview" };

// Detect dev mode - Netlify Dev sets NETLIFY_DEV=true
const isDev = process.env.NETLIFY_DEV === "true" || process.env.CONTEXT === "dev";

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

  // In dev mode, Netlify Dev intercepts all outbound HTTP from functions,
  // which breaks curl/fetch. Return placeholder data instead.
  if (isDev) {
    console.log(`[og-preview] Dev mode: returning placeholder for ${targetUrl}`);
    const hostname = parsedUrl.hostname;
    const devData = {
      url: targetUrl,
      title: hostname,
      image: null,
      description: `Link to ${hostname}`,
      siteName: hostname,
      favicon: null,
      dev: true, // Flag so client knows this is dev placeholder
    };
    // Don't cache dev placeholders
    return new Response(JSON.stringify(devData), {
      status: 200,
      headers: {
        "Content-Type": "application/json",
        "Access-Control-Allow-Origin": "*",
      },
    });
  }

  try {
    // Use fetch to get the page HTML
    const controller = new AbortController();
    const timeout = setTimeout(() => controller.abort(), 8000);
    
    const response = await fetch(targetUrl, {
      headers: {
        'User-Agent': 'Mozilla/5.0 (compatible; AestheticComputer/1.0)',
        'Accept': 'text/html',
      },
      signal: controller.signal,
      redirect: 'follow',
    });
    
    clearTimeout(timeout);
    
    if (!response.ok) {
      return new Response(JSON.stringify({ error: `HTTP ${response.status}` }), {
        status: 502,
        headers: {
          "Content-Type": "application/json",
          "Access-Control-Allow-Origin": "*",
        },
      });
    }
    
    const html = await response.text();

    // Parse Open Graph and other meta tags
    const resultData = parseMetaTags(html.slice(0, 50 * 1024), targetUrl);
    setCache(targetUrl, resultData);

    return new Response(JSON.stringify(resultData), {
      status: 200,
      headers: {
        "Content-Type": "application/json",
        "Access-Control-Allow-Origin": "*",
        "Cache-Control": "public, max-age=3600",
      },
    });
  } catch (err) {
    console.error(`[og-preview] Error fetching ${targetUrl}:`, err.message, err.code || '');
    const errorMessage = err.name === "AbortError" ? "Request timed out" : err.message;
    return new Response(JSON.stringify({ error: errorMessage, code: err.code }), {
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
