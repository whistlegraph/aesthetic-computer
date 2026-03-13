// keeps-social.js — SSR meta tag injection for social crawlers on keeps.kidlisp.com/$code permalinks

const CRAWLER_RE = /twitterbot|facebookexternalhit|linkedinbot|slackbot|discordbot|telegrambot|whatsapp|applebot/i;
const OBJKT_GRAPHQL = 'https://data.objkt.com/v3/graphql';

export default async function handleRequest(request, context) {
  const url = new URL(request.url);
  const host = request.headers.get('host') || '';

  // Only handle keeps.kidlisp.com
  if (!host.includes('keeps.kidlisp.com')) return context.next();

  const seg = url.pathname.replace(/^\/+/, '').split('/')[0];

  // Only handle $code permalink paths
  if (!seg.startsWith('$') || seg.length < 2) return context.next();

  // Only intercept social crawlers — normal users get the SPA
  const ua = request.headers.get('user-agent') || '';
  if (!CRAWLER_RE.test(ua)) return context.next();

  const code = seg.slice(1);

  try {
    // Fetch token data and resolve the preview image URL in parallel
    const [tokenData, ogImage] = await Promise.all([
      fetchTokenData(code),
      resolveImageUrl(`https://oven.aesthetic.computer/preview/1200x630/$${code}.png`),
    ]);

    // Get the upstream HTML response
    const response = await context.next();
    let html = await response.text();

    // Build meta tag values
    const title = `$${code}`;
    const description = buildDescription(tokenData);
    const permalink = `https://keeps.kidlisp.com/$${code}`;

    // Replace the static OG/Twitter meta tags with dynamic ones
    html = html.replace(
      /<meta property="og:url"[^>]*\/>/,
      `<meta property="og:url" content="${permalink}" />`
    );
    html = html.replace(
      /<meta property="og:title"[^>]*\/>/,
      `<meta property="og:title" content="${escapeAttr(title)}" />`
    );
    html = html.replace(
      /<meta property="og:description"[^>]*\/>/,
      `<meta property="og:description" content="${escapeAttr(description)}" />`
    );
    html = html.replace(
      /<meta property="og:image" content="[^"]*"[^>]*\/>/,
      `<meta property="og:image" content="${ogImage}" />`
    );
    html = html.replace(
      /<meta name="twitter:title"[^>]*\/>/,
      `<meta name="twitter:title" content="${escapeAttr(title)}" />`
    );
    html = html.replace(
      /<meta name="twitter:description"[^>]*\/>/,
      `<meta name="twitter:description" content="${escapeAttr(description)}" />`
    );
    html = html.replace(
      /<meta name="twitter:image" content="[^"]*"[^>]*\/>/,
      `<meta name="twitter:image" content="${ogImage}" />`
    );

    return new Response(html, {
      status: 200,
      headers: {
        'Content-Type': 'text/html; charset=utf-8',
        'Cache-Control': 'public, max-age=3600',
      },
    });
  } catch (err) {
    console.error('[keeps-social] error:', err);
    // Fall through to normal SPA on error
    return context.next();
  }
}

async function fetchTokenData(code) {
  // Use the production contract (v11)
  const contract = 'KT1Q1irsjSZ7EfUN4qHzAB2t7xLBPsAWYwBB';

  const query = `query {
    token(where: {
      fa_contract: { _eq: "${contract}" }
      name: { _eq: "$${code}" }
    }) {
      token_id
      name
      thumbnail_uri
    }
    listing_active(where: {
      fa_contract: { _eq: "${contract}" }
      token: { name: { _eq: "$${code}" } }
    } order_by: { price_xtz: asc } limit: 1) {
      price_xtz
      seller_address
    }
  }`;

  const res = await fetch(OBJKT_GRAPHQL, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ query }),
  });

  if (!res.ok) return null;
  const json = await res.json();
  const data = json?.data || {};
  const tokens = data?.token || [];
  const listings = data?.listing_active || [];

  if (tokens.length === 0) return null;

  return {
    token: tokens[0],
    listing: listings[0] || null,
  };
}

function buildDescription(tokenData) {
  if (!tokenData) return 'KidLisp generative art preserved on Tezos.';

  const { listing } = tokenData;
  if (listing) {
    const xtz = (Number(listing.price_xtz) / 1_000_000).toFixed(2);
    return `For Sale — ${xtz} XTZ | KidLisp generative art on Tezos`;
  }
  return 'KidLisp generative art preserved on Tezos.';
}

function escapeAttr(str) {
  return str.replace(/&/g, '&amp;').replace(/"/g, '&quot;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
}

// Resolve a URL through any redirects to get the final direct URL.
// Twitterbot does not reliably follow 302 redirects on image URLs,
// so we resolve to the direct CDN URL for the meta tag.
async function resolveImageUrl(url) {
  try {
    const res = await fetch(url, { method: 'HEAD', redirect: 'follow' });
    if (res.ok && res.url) return res.url;
  } catch (e) {
    console.error('[keeps-social] image resolve error:', e);
  }
  return url; // Fall back to the original URL
}
