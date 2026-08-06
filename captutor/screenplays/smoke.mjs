// smoke — captutor's self-test, and the shape every real screenplay copies.
//
// It drives fixtures/smoke.html: open a recipe, fill a socket, fuse, see the
// output. The fourth beat deliberately overruns its narration (the fixture
// stalls 3.2s on "Fuse"), because that is the case the measured-offset sync
// exists to survive — if this renders in sync, an AI generation that takes 40s
// will too.
//
// Run:
//   node captutor.mjs render smoke
// with a CDP Chrome open on the fixture (see README).

export default {
  slug: "smoke",
  voice: "jeffrey",
  window: "Chrome",           // what `reel` films — a window, not the desktop
  match: "smoke.html",        // which CDP page to drive
  fps: 60,
  signIn: false,               // fixture: never send Iris a login code
  billable: false,              // fixture: no Fuser credits are involved

  beats: [
    {
      say: "This is the recipe gallery. Every card here is a published flow, ready to run.",
      do: async ({ point }) => {
        await point("[data-testid=recipe-card]");
      },
    },
    {
      say: "Open one, and it asks for whatever the author left exposed.",
      do: async ({ click }) => {
        await click("[data-testid=recipe-card]");
      },
    },
    {
      say: "This recipe takes a single line of text.",
      do: async ({ type }) => {
        await type("[data-testid=socket-text]", "a poster for the fuser studio");
      },
    },
    {
      // The overrun beat. The line lasts ~2s; the fixture takes 3.2s. captutor
      // stamps the NEXT beat where it truly starts, so nothing drifts.
      say: "Press Fuse, and the flow runs.",
      do: async ({ click, cdp }) => {
        await click("[data-testid=fuse]");
        await cdp.waitFor("document.querySelector('[data-testid=shelf]').classList.contains('open')");
      },
    },
    {
      say: "The result lands in the output shelf.",
      do: async ({ point }) => {
        await point("[data-testid=shelf]");
      },
    },
  ],
};
