// Test Suite No. 1 in A.C. Major
// Score for Prix Ars Electronica 2026

export const meta = {
  title: "Test Suite No. 1 in A.C. Major",
  voices: ["female:18"],
};

export const score = [
  {
    movement: "I. The Prompt",
    card: { text: "Test one.\nThe prompt.", duration: 3000, fade: 500 },
    actions: [
      { action: "speak", text: "Test one. The prompt.", voice: "female:18" },
      { action: "wait-for", event: "speech:completed" },
      { action: "wait", ms: 500 },
      { action: "type", text: "notepat", speed: 80 },
      { action: "wait", ms: 300 },
      { action: "key", key: "Enter" },
      { action: "wait", ms: 3000 },
      { action: "keys", sequence: [
        { key: "d", hold: 250 },
        { key: "a", hold: 250 },
        { key: "f", hold: 250 },
        { key: "d", hold: 250 },
        { key: "a", hold: 250 },
        { key: "f", hold: 250 },
      ]},
      { action: "wait", ms: 2000 },
      { action: "key", key: "Escape" },
      { action: "wait", ms: 500 },
    ],
  },
  {
    movement: "II. Pieces",
    card: { text: "Test two.\nThree hundred and\nfifty four pieces.", duration: 3000, fade: 500 },
    actions: [
      { action: "speak", text: "Test two. Three hundred and fifty four pieces.", voice: "female:18" },
      { action: "wait-for", event: "speech:completed" },
      { action: "wait", ms: 500 },
      { action: "type", text: "line", speed: 60 },
      { action: "wait", ms: 200 },
      { action: "key", key: "Enter" },
      { action: "wait", ms: 2000 },
      { action: "jump", to: "prompt" },
      { action: "wait", ms: 500 },
      { action: "type", text: "plot", speed: 60 },
      { action: "wait", ms: 200 },
      { action: "key", key: "Enter" },
      { action: "wait", ms: 2000 },
      { action: "jump", to: "prompt" },
      { action: "wait", ms: 500 },
    ],
  },
  {
    movement: "III. KidLisp",
    card: { text: "Test three.\nA language a child\ncan read.", duration: 3000, fade: 500 },
    actions: [
      { action: "speak", text: "Test three. A language a child can read.", voice: "female:18" },
      { action: "wait-for", event: "speech:completed" },
      { action: "wait", ms: 3000 },
    ],
  },
  {
    movement: "IV. Social",
    card: { text: "Test four.\nSocial infrastructure.", duration: 3000, fade: 500 },
    actions: [
      { action: "speak", text: "Test four. Social infrastructure.", voice: "female:18" },
      { action: "wait-for", event: "speech:completed" },
      { action: "wait", ms: 3000 },
    ],
  },
  {
    movement: "V. Flow",
    card: { text: "Test five.\nFlow state.", duration: 3000, fade: 500 },
    actions: [
      { action: "speak", text: "Test five. Flow state.", voice: "female:18" },
      { action: "wait-for", event: "speech:completed" },
      { action: "wait", ms: 3000 },
    ],
  },
  {
    movement: "VI. Coda",
    card: { text: "All tests passed.", duration: 4000, fade: 1000 },
    actions: [
      { action: "speak", text: "All tests passed.", voice: "female:18" },
      { action: "wait-for", event: "speech:completed" },
      { action: "wait", ms: 2000 },
    ],
  },
];
