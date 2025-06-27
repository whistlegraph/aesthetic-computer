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

  // 🗞️ FF DP-1: https://docs.google.com/document/d/1fEB44JTP2_M7ZiP5MR03FKcjNnnH4vTJ5_y-0foE08Q/edit?usp=sharing
  //   📦 Top-Level (3.1): dpVersion, id, created, defaults?, signature?
  //   🎮 Display (4): scaling, background, margin, autoplay?, loop?, interaction?
  //   📜 Items[] (3.2): id, title, source, duration?, license?, ref?, override?
  //   🔄 Repro (5): engineVersion?, seed?, assetsSHA256?, frameHash?
  //   📚 Provenance (6): type, contract?, dependencies?
  //   🔐 Security (7.1): Ed25519 signature
  //   🚀 Transport (8): HTTP, IPFS, offline support 

  // 📜 Meta
  const playlistData = {
    "dpVersion": "1.0.0", // 🏷️ DP-1 spec
    "id": "aesthetic-computer-playlist-001", // 🆔 Unique ID
    "created": "2025-06-27T12:00:00Z", // ⏰ Created
    "defaults": { // 📦 Inherited
      "display": { // 🎮 Display
        "scaling": "fit", // 📏 fit|fill|stretch
        "background": "#000000", // 🎨 Hex or "transparent"
        "margin": "5%" // 📐 Even margin
      },
      "license": "open", // ⚖️ open|token|sub
      "duration": 300 // ⏱️ Default sec
    },
    // 🖼️ Media
    "items": [
      {
        "id": "starfield-001", // 🆔 Unique ID
        "title": "Starfield", // 📝 Item name
        "source": "https://prompt.ac/starfield", // 🌐 Artwork URL
        "duration": 30, // ⏱️ Duration sec
        "license": "open", // ⚖️ License mode
        // "ref": "ipfs://bafybeigd…/manifest.json", // 📄 External (opt)
        // "override": { "duration": 180 }, // 🔄 Override (opt)
        // "display": { // 🖥️ Display (opt)
        //   "scaling": "fill",
        //   "autoplay": true,
        //   "loop": false
        // },
        // "repro": { // 🔄 Repro (opt)
        //   "engineVersion": { "chromium": "123.0.6312.58" },
        //   "seed": "0x84a39ef5…",
        //   "assetsSHA256": ["473…", "9be…"]
        // },
        // "provenance": { // 📚 Provenance (opt)
        //   "type": "onChain",
        //   "contract": {
        //     "chain": "evm",
        //     "standard": "erc721",
        //     "address": "0x61d45475fe81ef46bdd8093b5c73efee03167e0",
        //     "tokenId": "42"
        //   }
        // }
      },
      {
        "id": "wipe-red-002",
        "title": "Wipe Red",
        "source": "https://prompt.ac/(wipe_red)",
        "duration": 5,
        "license": "open"
      },
      {
        "id": "wipe-rainbow-003",
        "title": "Wipe Rainbow",
        "source": "https://prompt.ac/(0.25s_wipe_rainbow)",
        "duration": 1,
        "license": "open"
      },
      {
        "id": "noise-004",
        "title": "Noise",
        "source": "https://prompt.ac/(noise)",
        "duration": 10,
        "license": "open"
      },
      {
        "id": "repeat-line-005",
        "title": "Repeat Line",
        "source": "https://prompt.ac/(1s_(ink_(..._red_blue))_(repeat_2_line))",
        "duration": 15,
        "license": "open"
      }
    ],
    "signature": "ed25519:0x…" // ✍️ Signature (opt)
  };

  return {
    statusCode: 200,
    headers: { 
      "Content-Type": "application/json",
      "Access-Control-Allow-Origin": "*",
      "Cache-Control": "public, max-age=300" // 5 minute cache
    },
    body: JSON.stringify(playlistData, null, 2),
  };
};