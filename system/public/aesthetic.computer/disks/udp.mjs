// UDP, 2023.11.29.14.43.51.154
// A Simple UDP test piece.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [] Get basic UDP messages passing. 
#endregion */

let server;

// 🥾 Boot
function boot({ wipe, net: { udp } }) {
  wipe("blue");

  server = udp(function receive(content) {
    console.log("UDP Received:", content);
  });
}

function paint({ wipe }) {
  // wipe("blue");
  return false;
}

// 🎪 Act
function act({ event: e }) {
  if (e.is("touch")) server.send("touch", { x: e.x, y: e.y });
}

// 📰 Meta
function meta() {
  return {
    title: "UDP",
    desc: "A simple UDP test piece.",
  };
}

export { boot, paint, act, meta };

// 📚 Library
//   (Useful functions used throughout the piece)
