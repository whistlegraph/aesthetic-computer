// Deck, 2023.8.14.12.22.18
// A demo slide deck that explains aesthetic.computer.

/* #region 📚 README 
  See also: `/notes/funding-questions.txt`.
#endregion */

/* #region 🏁 TODO 
  - [] Minors need to start in a base location and then show up automagically. 
  - [] Add sound and other doodads.
  + Done
  - [x] Add word-wrapped text / break-width option to "write".
#endregion */

const statements = [
  {
    major: "aesthetic.computer is transparent",
    minors: [
      "its intentions r clear ;)",
      "you can see how it works",
      "the software is visible",
    ],
  },
  {
    major: "its functionality is emerging",
    minors: [
      "line",
      "rect",
      "oval",
      "paint",
      "shape",
      "bleep",
      "encode",
      "decode",
      "metronome",
      "mom",
      "dad",
      "boyfriend",
      "girlfriend",
      "smear",
      "camera",
      "baktok",
      "liar",
      "whistle",
    ],
    // TODO: Over N pieces and counting.
  },
  {
    major: "when ur using it ur making it",
    // TODO: Pull in a random list of all user handles programatically.
    minors: ["@jeffrey", "@georgica", "@ida", "@mxsage"],
    // TODO: Show a count of users with handles.
  },
  { major: "other software is prescriptive" },
  { major: "it simplifies ur identity" },
  { major: "and operates in secret" },
  {
    major: "we need funding",
    minors: [
      "$100",
      "10 eth",
      "50000 tez",
      "5 million bucks",
      "$0.99",
      "cap table etc.",
    ],
  },
  {
    major: "to be bigger + better",
    minors: [
      "roblox",
      "ig",
      "snapchat things",
      "apples",
      "google",
      "tiktok",
      "adobe",
      "microcraft",
      "kid pix",
      "tumblr",
    ],
  },
  {
    major: "for artists of all ages :)",
    minors: ["painters", "poets", "coders", "composers"],
  },
  // TODO: Add a button here that says ^ "Try it".
];
let si = 0;
let needsWipe = null;

// 🥾 Boot
function boot({ wipe, ink, line }) {
  // Runs once at the start.
  wipe(0);

  // Grab all the handles.
  fetch("/handle?for=all")
    .then((res) => {
      if (!res.ok) throw new Error(`HTTP error! Status: ${res.status}`);
      return res.json();
    })
    .then((data) => (statements[2].minors = data.handles))
    .catch((error) => console.error("😫 Error fetching handles:", error));
}

// 🎨 Paint
function paint({ ink, wipe, write, num, help, leaving, screen }) {
  const s = statements[si];

  if (si === -1) {
    // wipe(0);
    return;
  }

  if (needsWipe !== null) {
    wipe(needsWipe);
    needsWipe = null;
  }

  // let slideNo = si;
  let majorColor = undefined;
  let minorColor = num.randIntRange(32, 64);

  if (si <= 2) {
    majorColor = help.choose("red", "yellow", "blue");
  } else if (si > 2 && si < 6) {
    // slideNo = 6;
    majorColor = help.choose("yellow", "white");
  } else if (si === 6) {
    // slideNo = "$";
    majorColor = help.choose("lime", "white");
    minorColor = [0, num.randIntRange(100, 200), 0];
  } else if (si === 7) {
    majorColor = help.choose("white", "cyan");
    minorColor = [0, 0, num.randIntRange(100, 200)];
  } else if (si === 8) {
    majorColor = help.choose("pink", "red");
    minorColor = [num.randIntRange(100, 200), 0, num.randIntRange(100, 200)];
  }

  ink(majorColor).write(
    `${s.major}`,
    { center: "xy" },
    undefined,
    screen.width - 48
  );

  if (s.minors?.length > 0) ink(minorColor).write(help.any(s.minors));

  const mid = screen.height - 1;
  ink(help.choose("white", "black", "red")).box(
    0,
    mid,
    screen.width * (si / (statements.length - 1)),
    mid
  );

  // ink("red").line(0, screen.height / 2, screen.width, screen.height / 2);
}

// 🎪 Act
function act({ event: e, jump }) {
  if (e.is("touch")) {
    si = (si + 1) % statements.length;

    if (si < 3) {
      needsWipe = "black";
    } else if (si < 6) {
      needsWipe = "red";
    } else if (si < 7) {
      needsWipe = "green";
    } else if (si < 8) {
      needsWipe = "blue";
    } else {
      needsWipe = "purple";
    }

    if (si === 0) {
      // si = -1;
      // jump("prompt");
    }
  }
}

// 🧮 Sim
// function sim() {
//  // Runs once per logic frame. (120fps locked.)
// }

// 🥁 Beat
// function beat() {
//   // Runs once per metronomic BPM.
// }

// 👋 Leave
// function leave() {
//  // Runs once before the piece is unloaded.
// }

// 📰 Meta
function meta() {
  return {
    title: "Deck",
    desc: "A demo slide deck that explains aesthetic.computer.",
  };
}

// 🖼️ Preview
// function preview({ ink, wipe }) {
// Render a custom thumbnail image.
// }

// 🪷 Icon
// function icon() {
// Render an application icon, aka favicon.
// }

export { boot, act, paint, meta };

// 📚 Library
//   (Useful functions used throughout the piece)
