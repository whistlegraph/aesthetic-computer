// intro-to-the-prompt — episode 1: what the aesthetic.computer prompt is,
// how a piece runs, a first taste of KidLisp, and the way back home.
//
// Voice notes: AC is an instrument, not an app. Short lines, present tense,
// no feature-tour cadence. Every sentence should survive being spoken over
// a phone-shaped screen.
//
//   node aesthetic-captutor.mjs render intro-to-the-prompt --format vertical

export default {
  slug: "intro-to-the-prompt",
  voice: "jeffrey",
  url: "https://aesthetic.computer",
  boot: 6000, // let the boot canvas and prompt settle before the first frame

  beats: [
    {
      say: "This is aesthetic computer. It boots straight into a prompt — a blinking cursor that's waiting for you to play it.",
      do: async ({ tap, sleep }) => {
        await sleep(800);
        await tap(0.5, 0.5); // wake the caret
      },
    },
    {
      say: "Every piece has a short name you can memorize. Type line, and press enter.",
      do: async ({ type, press, sleep }) => {
        await sleep(600);
        await type("line");
        await sleep(400);
        await press("Enter");
      },
    },
    {
      say: "Now the whole screen is the piece. Drag to draw — it does exactly what it says.",
      do: async ({ drag, sleep }) => {
        await sleep(900);
        await drag(0.22, 0.30, 0.78, 0.52, { ms: 700 });
        await sleep(250);
        await drag(0.70, 0.28, 0.30, 0.68, { ms: 700 });
        await sleep(250);
        await drag(0.25, 0.75, 0.80, 0.80, { ms: 600 });
      },
    },
    {
      say: "Escape always brings you home to the prompt. One key, from anywhere.",
      do: async ({ press, sleep }) => {
        await sleep(500);
        await press("Escape");
      },
    },
    {
      say: "The prompt also speaks a little language called KidLisp. Open a parenthesis, and you're writing code that runs every frame.",
      do: async ({ type, press, sleep }) => {
        await sleep(400);
        await type("(wipe navy) (ink lime) (line)", { cps: 10 });
        await sleep(400);
        await press("Enter");
      },
    },
    {
      say: "One line, and the machine is painting for you.",
      do: async ({ sleep }) => {
        await sleep(500); // hold on the generative piece; the picture is the point
      },
    },
    {
      say: "Escape again, and try notepat — the screen becomes a pocket instrument, and your keyboard plays it.",
      do: async ({ press, type, sleep }) => {
        await press("Escape");
        await sleep(700);
        await type("notepat");
        await sleep(300);
        await press("Enter");
        await sleep(1600);
        for (const note of ["a", "s", "d", "f", "d", "s", "a"]) {
          await press(note);
          await sleep(340);
        }
      },
    },
    {
      say: "When you find something worth keeping, ask the prompt to share it. You get a QR code anyone can scan.",
      do: async ({ press, type, sleep }) => {
        await press("Escape");
        await sleep(700);
        await type("share notepat");
        await sleep(300);
        await press("Enter");
      },
    },
    {
      say: "That's the prompt. Type a name, play, escape, repeat — aesthetic computer.",
      do: async ({ press, sleep }) => {
        await sleep(2200); // let the QR breathe before heading home
        await press("Escape");
      },
    },
  ],
};
