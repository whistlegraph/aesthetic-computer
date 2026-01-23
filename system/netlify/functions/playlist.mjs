// api/playlist, 2025.06.27.12.00
// Dynamic playlists of AC pieces conforming to the Feral File DP-1 spec.

export async function handler(event, context) {
  if (event.httpMethod !== "GET") {
    return {
      statusCode: 405,
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ error: "Method not allowed" }),
    };
  }

  // Parse query params
  const params = new URLSearchParams(event.rawQuery || '');
  const limit = parseInt(params.get('limit')) || 10;
  const duration = parseInt(params.get('duration')) || 60; // Default 60s per item
  const density = parseInt(params.get('density')) || 8;
  const type = params.get('type') || 'kidlisp'; // kidlisp, static, or custom list name

  // 🔧 Environment detection
  const dev = process.env.NETLIFY_DEV || process.env.CONTEXT === "dev";
  const baseUrl = dev ? "https://local.aesthetic.computer" : "https://aesthetic.computer";
  const deviceUrl = dev ? "https://local.aesthetic.computer/device.kidlisp.com" : "https://device.kidlisp.com";

  // 🗞️ FF DP-1: https://docs.google.com/document/d/1fEB44JTP2_M7ZiP5MR03FKcjNnnH4vTJ5_y-0foE08Q/edit?usp=sharing
  //   📦 Top-Level (3.1): dpVersion, id, created, defaults?, signature?
  //   🎮 Display (4): scaling, background, margin, autoplay?, loop?, interaction?
  //   📜 Items[] (3.2): id, title, source, duration?, license?, ref?, override?
  //   🔄 Repro (5): engineVersion?, seed?, assetsSHA256?, frameHash?
  //   📚 Provenance (6): type, contract?, dependencies?
  //   🔐 Security (7.1): Ed25519 signature
  //   🚀 Transport (8): HTTP, IPFS, offline support 

  let items = [];

  if (type === 'kidlisp') {
    // Fetch top KidLisp hits from /api/tv
    try {
      const tvUrl = `${baseUrl}/api/tv?types=kidlisp&sort=hits&limit=${limit}`;
      const response = await fetch(tvUrl);
      const data = await response.json();
      
      if (data.media && data.media.kidlisp) {
        items = data.media.kidlisp
          .sort((a, b) => (b.hits || 0) - (a.hits || 0))
          .slice(0, limit)
          .map((item, index) => ({
            id: `kidlisp-${item.code}`,
            title: `$${item.code}`,
            source: `${deviceUrl}/${item.code}?density=${density}&playlist=true&duration=${duration}`,
            duration: duration,
            license: "open",
            provenance: {
              type: "offChainURI",
              uri: `${baseUrl}/$${item.code}`
            }
          }));
      }
    } catch (e) {
      console.error('Failed to fetch kidlisp hits:', e);
    }
  }

  // Fallback to static list if dynamic fetch fails or type is 'static'
  if (items.length === 0 || type === 'static') {
    items = [
      {
        id: "kidlisp-ceo",
        title: "$ceo",
        source: `${deviceUrl}/ceo?density=${density}&playlist=true&duration=${duration}`,
        duration: duration,
        license: "open",
      },
      {
        id: "kidlisp-roz",
        title: "$roz",
        source: `${deviceUrl}/roz?density=${density}&playlist=true&duration=${duration}`,
        duration: duration,
        license: "open",
      },
      {
        id: "kidlisp-bels",
        title: "$bels",
        source: `${deviceUrl}/bels?density=${density}&playlist=true&duration=${duration}`,
        duration: duration,
        license: "open",
      },
    ];
  }

  // 📜 Build DP-1 playlist
  const playlistData = {
    "dpVersion": "1.0.0",
    "id": `aesthetic-computer-${type}-playlist`,
    "created": new Date().toISOString(),
    "defaults": {
      "display": {
        "scaling": "fit",
        "background": "#000000",
        "margin": "0%"
      },
      "license": "open",
      "duration": duration
    },
    "items": items,
    "signature": `aesthetic.computer-${new Date().toISOString().slice(0, 10).replace(/-/g, '.')}`
  };

  return {
    statusCode: 200,
    headers: { 
      "Content-Type": "application/json",
      "Access-Control-Allow-Origin": "*",
      "Cache-Control": "public, max-age=300"
    },
    body: JSON.stringify(playlistData, null, 2),
  };
};