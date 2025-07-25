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

  // 🔧 Environment detection
  const dev = process.env.NETLIFY_DEV || process.env.CONTEXT === "dev";
  const baseUrl = dev ? "https://local.aesthetic.computer" : "https://prompt.ac";

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
        "scaling": "fit", // 📏 fit|fill|stretchapi/playlist
        "background": "#000000", // 🎨 Hex or "transparent"
        "margin": "5%" // 📐 Even margin
      },
      "license": "open", // ⚖️ open|token|sub
      "duration": 300 // ⏱️ Default sec
    },
    // 🖼️ Media
    "items": [
      // {
      //   "id": "zzzwap", // 🆔 Unique ID
      //   "title": "zzzwap", // 📝 Item name
      //   "source": `${baseUrl}/zzzwap?density=10&duration=512&nolabel`, // 🌐 Artwork URL
      //   "duration": 512, // ⏱️ Duration sec
      //   "license": "open", // ⚖️ License mode
      // },
      {
        "id": "(wipe_purple)", // 🆔 Unique ID
        "title": "(wipe purple)", // 📝 Item name
        "source": `${baseUrl}/(wipe_brown)?density=10&duration=18&nolabel`, // 🌐 Artwork URL
        "duration": 18, // ⏱️ Duration sec
        "license": "open", // ⚖️ License mode
      },
      {
        "id": "(wipe_purple)", // 🆔 Unique ID
        "title": "(wipe purple)", // 📝 Item name
        "source": `${baseUrl}/(wipe_purple)?density=10&duration=18&nolabel`, // 🌐 Artwork URL
        "duration": 18, // ⏱️ Duration sec
        "license": "open", // ⚖️ License mode
      },
      // {
      //   "id": "$bels", // 🆔 Unique ID
      //   "title": "$bels", // 📝 Item name
      //   "source": `${baseUrl}/$bels?density=10&duration=15`, // 🌐 Artwork URL
      //   "duration": 15, // ⏱️ Duration sec
      //   "license": "open", // ⚖️ License mode
      // },
      // {
      //   "id": "$wes", // 🆔 Unique ID
      //   "title": "$wes", // 📝 Item name
      //   "source": `${baseUrl}/$wes?density=10&duration=15`, // 🌐 Artwork URL
      //   "duration": 15, // ⏱️ Duration sec
      //   "license": "open", // ⚖️ License mode
      // },
      // {
      //   "id": "$bial", // 🆔 Unique ID
      //   "title": "$bial", // 📝 Item name
      //   "source": `${baseUrl}/$bial?density=10&duration=15`, // 🌐 Artwork URL
      //   "duration": 15, // ⏱️ Duration sec
      //   "license": "open", // ⚖️ License mode
      // },
      // {
      //   "id": "starfield-001", // 🆔 Unique ID
      //   "title": "Starfield", // 📝 Item name
      //   "source": `${baseUrl}/3-kidlisp-tests?density=10`, // 🌐 Artwork URL
      //   "duration": 30, // ⏱️ Duration sec
      //   "license": "open", // ⚖️ License mode
      // }
    ],
    "signature": "aesthetic.computer-25.07.24.22.13" // ✍️ Signature (opt)
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