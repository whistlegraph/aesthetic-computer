import assert from "node:assert/strict";
import test from "node:test";
import { filmProblems, frameAt, valueAt } from "../src/film.mjs";
import { builtInProblems, outputType } from "../src/harness-factory.mjs";

const film = {
  seconds: 4, fps: 12, width: 320, height: 240,
  svg: '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 320 240"><rect width="320" height="240" fill="#102040"/><circle id="sun" cx="60" cy="200" r="24" fill="#ffcc00"/><g id="boat" transform="translate(0 0)"><text id="line" x="20" y="40">one</text></g></svg>',
  tracks: [
    { id: "sun", attr: "cy", from: 200, to: 100, start: 0, end: 4, ease: "linear" },
    { id: "sun", attr: "fill", from: "#000000", to: "#ffffff", start: 0, end: 4, ease: "linear" },
    { id: "boat", attr: "transform", from: "translate(0 0)", to: "translate(200 10)", start: 1, end: 3, ease: "linear" },
    { id: "line", attr: "text", steps: ["one", "two & <three>"], start: 0, end: 4 },
    { id: "sun", attr: "opacity", from: 0, to: 1, start: 0, end: 2, ease: "linear" },
  ],
};

test("a film is checked before it is drawn", () => {
  assert.deepEqual(filmProblems(film), []);
  assert.match(filmProblems({ ...film, seconds: 90 })[0], /seconds/);
  assert.match(filmProblems({ ...film, tracks: [] })[0], /nothing would move/);
  assert.match(filmProblems({ ...film, tracks: [{ id: "moon", attr: "cy", from: 0, to: 1, start: 0, end: 1 }] })[0], /id="moon"/);
  assert.match(filmProblems({ ...film, svg: "a sunset" })[0], /svg/);
  assert.match(filmProblems({ ...film, score: { bpm: 96, notes: [] } }).join(" "), /score:/);
});

test("a track's value moves between its two times and holds outside them", () => {
  const [cy, fill, boat, line] = film.tracks;
  assert.equal(valueAt(cy, 2), 150);
  assert.equal(valueAt(cy, 9), 100);
  assert.equal(valueAt(fill, 2), "#808080");
  assert.equal(valueAt(boat, 0), "translate(0 0)");
  assert.equal(valueAt(boat, 2), "translate(100 5)");
  assert.equal(valueAt(line, 1), "one");
  assert.equal(valueAt(line, 3), "two & <three>");
});

test("a frame is the svg with each value written in", () => {
  const frame = frameAt(film, 2);
  assert.match(frame, /<circle id="sun" cx="60" cy="150" r="24" fill="#808080" opacity="1"\/>/);
  assert.match(frame, /<g id="boat" transform="translate\(100 5\)">/);
  assert.match(frame, />two &amp; &lt;three><\/text>/);
  assert.match(frameAt(film, 0), />one<\/text>/);
});

test("a film and a p5 sketch are output goals with their own checks", () => {
  assert.equal(outputType("mp4"), "video/mp4");
  assert.equal(outputType("p5"), "text/x-processing");
  assert.deepEqual(builtInProblems("video/mp4", JSON.stringify(film)), []);
  assert.match(builtInProblems("video/mp4", "{")[0], /not valid JSON/);
  assert.deepEqual(builtInProblems("text/x-processing", "function setup() { createCanvas(9, 9); }\nfunction draw() {}"), []);
  assert.match(builtInProblems("text/x-processing", "export function paint() {}").join(" "), /global mode/);
  assert.match(builtInProblems("text/x-processing", "function setup() {").join(" "), /does not parse/);
});
