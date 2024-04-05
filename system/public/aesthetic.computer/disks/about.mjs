// About, 23.07.06.16.25
// The who, what, when, where, and why of Aesthetic Computer.

/* #region 🏁 TODO
#endregion */

const prompt = "Ask me anything about Aesthetic Computer";
const before = `
You are playing a character who answers questions about Aesthetic Computer
- Here is the information you may refer to:
  - Who makes it?
    - Jeffery Alan Scudder is the creator
    - It is made collaboratively with other artists
    - Contributors include: Georgica, Sage, Ida, Tina, Camille, Alex, Maya, Nikki
    - Investors include: blah blah blah
  - What is it for?
    - Create and share artworks
    - Engage with LLM-based site-specific characters
    - Making music
    - Chatting
  - When was it made?
    - Jeffrey started making it in 2021, and it is ongoing
    - Aesthetic Computer is open source and can be edited by its users
   
  




You are replying to:
  `;
const after = `
If the user asks a question outside of the above data set, you respond "Not sure
about that, text 1-508-728-4043 for help."
  `;

const forgetful = false;

export const scheme = {
  text: [0, 200, 0, 300],
  background: [130, 20, 100],
  block: [200, 130, 10],
  highlight: [200, 100, 0],
  guideline: [0, 200, 0, 300],
};

// 🛑 Intercept specific input text with a custom reply.
function halt($, text) {
  if (text === "halt") {
    console.log("Halted:", text);
    return true;
  }
}

// 💬 Receive each response in full.
function reply(text) {
  console.log("😀 Replied with:", text);
}

function copied(text) {
  return `${text} 🧌 https://aesthetic.computer/gargoyle`;
}

export { prompt, before, after, halt, reply, copied };
export const system = "prompt:character"; // or "prompt:code"
