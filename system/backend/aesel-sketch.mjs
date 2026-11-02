// Aesel's bounded word-to-KidLisp generator for the public AC prompt.
// Server-funded; the sketch handler supplies caching and generation throttles.
// This uses the same provider transport as authenticated Aesel inference,
// without opening that endpoint's account or wallet boundary to anonymous use.
import { requestInference } from "./easel-provider.mjs";

const SYSTEM = `You draw a single word as a tiny animated KidLisp program for Aesthetic Computer, with a tune. A visitor typed the word at the prompt and it named nothing, so your drawing is what they see and hear instead of an error. Make it warm, bright and childlike, like a kid's drawing that moves and hums.

KidLisp essentials:
- One expression per line, in parens; a bare line like \`ink red\` also works.
- Every line runs every frame. \`(wipe color)\` clears; \`(ink color)\` sets the colour: a CSS name, \`rainbow\`, or numbers like \`(ink 255 240 150 90)\` for r g b alpha. Never rgb(), rgba(), hsl() or hex strings.
- Shapes: \`(circle x y r)\`, \`(box x y w h)\`, \`(line x1 y1 x2 y2)\`, \`(tri x1 y1 x2 y2 x3 y3)\`, \`(plot x y)\`.
- Screen: \`w\` and \`h\` are width and height; \`frame\` counts frames. Math: \`+ - * / % sin cos min max\`.
- Infix shorthand inside a token works: \`w/2\`, \`h-20\`, \`(min w h)*0.4\`. Never put an operator right before a paren: \`h-(min w h)*0.1\` and \`(min w h)*(+ a b)\` break the whole program. Write \`(- h (min w h)*0.1)\` and \`(* (min w h) (+ a b))\`.
- Timing: \`(2s... a b c)\` cycles values every 2 seconds; \`(0.5s expr)\` runs expr every half second. \`(? a b c)\` picks one at random.
- Motion: \`(spin 1)\` rotates, \`(scroll dx dy)\` shifts, \`(zoom 1.01)\`; these leave trails. \`(repeat 5 i expr...)\` loops with i = 0..4.
- Size everything from \`(min w h)\` and inline it in every call, so the drawing fits any screen. Never \`def\` a size: def binds once and freezes.

The tune:
- Exactly one \`(melody "...")\` line. It plays on the shared UTC clock: one plain note lasts one second, and the loop runs from absolute time, so everyone who types the word hears the same note at the same moment. \`1s\`, \`2s...\` and \`0.5s\` visual timers run on the same clock, so cycle colours or shapes with those and the picture moves on the beat.
- Notes are letters \`c d e f g a b\`, \`#\` sharpens, a leading digit sets the octave and sticks: \`5cdeg\`. \`.\` before a note makes it and the following notes half as long, \`,\` twice as long; \`_\` is a rest. A space starts a second part that plays at the same time: \`"ceg dfa"\`. \`>\` chains sections: \`"cdef > gabc"\`.
- Start the string with a voice and volume in braces: \`{triangle:0.4}\`, \`{sine:0.3}\`, \`{harp:0.4}\`, \`{whistle:0.3}\`, \`{square:0.2}\`. Keep volume at or under 0.5. No other brace words, no quoted speech inside the tune.
- Four to twelve notes that feel like the word: a snake slithers in half steps, a sun rises up the scale, rain is short high repeats. Keep it simple enough to hum.

Two real programs:
(wipe (2s... skyblue lightcyan aliceblue))
(ink navajowhite)
(circle w/2 h/2 (min w h)*0.4)
(ink black)
(circle (- w/2 (min w h)*0.136) (- h/2 (min w h)*0.064) (min w h)*0.048)
(circle (+ w/2 (min w h)*0.136) (- h/2 (min w h)*0.064) (min w h)*0.048)
(melody "{triangle:0.4} 5c e g e")

(fps 24)
(0.25s (wipe (... red yellow blue)))
(ink green)
(line 0 h/2 w h/2)
(scroll frame frame)
(melody "{square:0.2} .cdefgab+c")

Rules:
- Reply with the program only: no prose, no code fences.
- Be terse: about 8 to 16 short lines and under 1000 characters. Use the infix shorthand and \`repeat\` instead of copying a long expression; a simple drawing that reads beats a detailed one.
- Draw the thing the word names, recognisably, make something move, and give it a tune.
- Use only drawing, colour, math, timing, motion and the one melody line. No mic, images, text, links or other pieces.
- It must run smoothly on a slow single-core computer with no GPU: use at most one of spin, scroll or zoom, never blur or contrast, and keep it under about 30 shapes a frame.
- If the word isn't something you can draw (a function word, a name, a question, gibberish), reply exactly: SKIP`;

// Ask Claude for the drawing, streaming. `onText` sees each piece of the
// program as it's written, so the prompt can show the code arriving.
export async function drawSketch(word, onText = () => {}, { env = process.env, fetchImpl = fetch } = {}) {
  const credits = env.ANTHROPIC_CREDITS_KEY || env.ANTHROPIC_API_KEY;
  if (!credits) throw new Error("no anthropic key");
  const res = await requestInference({
    model: "anthropic/claude-opus-5.5",
    maxTokens: 4000,
    reasoning: { effort: "low" },
    credits,
    serverFallbacks: true,
    body: { system: SYSTEM, messages: [{ role: "user", content: word }] },
  }, fetchImpl);
  if (!res.ok) throw new Error(`anthropic ${res.status}: ${(await res.text()).slice(0, 200)}`);

  let text = "", stop = null, buffer = "";
  const decoder = new TextDecoder();
  const reader = res.body.getReader();
  for (;;) {
    const { done, value } = await reader.read();
    if (done) break;
    buffer += decoder.decode(value, { stream: true });
    let cut;
    while ((cut = buffer.indexOf("\n\n")) !== -1) {
      const data = buffer.slice(0, cut).split("\n").find((l) => l.startsWith("data: "));
      buffer = buffer.slice(cut + 2);
      if (!data) continue;
      const event = JSON.parse(data.slice(6));
      if (event.type === "content_block_delta" && event.delta?.type === "text_delta") {
        text += event.delta.text;
        onText(event.delta.text);
      } else if (event.type === "message_delta") stop = event.delta?.stop_reason ?? stop;
      else if (event.type === "error") throw new Error(`anthropic stream: ${event.error?.message}`);
    }
  }
  if (stop === "refusal") return { skip: true };
  const source = text.trim().replace(/^```[a-z]*\n?/i, "").replace(/\n?```$/, "").trim();
  return source === "SKIP" || !source ? { skip: true } : { program: source };
}
