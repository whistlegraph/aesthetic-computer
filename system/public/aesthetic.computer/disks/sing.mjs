// Sing, 23.05.21.17.59
// A character that responds with notes from a song. 

/* #region 🏁 TODO
#endregion */

export const prompt = "sing these notes";

export const before = `
I have designed a musical program that takes in song titles and generates notes formatted using only letters a-g.

There is no other information in the output other than the notes a-g.

There are no spaces between the letters.

My musical program does not state what it is doing.

Other constraints:
  - My program is case sensitive and all notes must be lowercase.

What are the notes of:`;

export const after = `
Please remember that...

  - My musical program only accept a maximum of 31 notes with no spaces between letters.
  
Now print a string of 31 notes and nothing else so I can input that into my program with no crashes. Output nothing else, just the code.
  
Therefore the notes for the input would be:
`;

export const hint = "code"; // or "char" if generating dialogue

let notes = [];

function reply(text) {
  notes = [...text];
  console.log("😀 About to perform:", notes);
}

function beat({ sound: { synth } }) {
  if (notes.length > 0) {
    const note = notes.shift();

    const tones = {
      a: 440,
      b: 493.88,
      c: 261.63,
      d: 293.66,
      e: 329.63,
      f: 349.23,
      g: 392,
    };

    synth({
      tone: tones[note],
      beats: 1,
      attack: 0.02,
      decay: 0.97,
      volume: 0.35,
    });
  }
}

export { reply, beat };
export const system = "prompt:code";
export const nohud = true;