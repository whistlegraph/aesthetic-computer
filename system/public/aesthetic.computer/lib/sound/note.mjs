const { max } = Math;

const notes = {
  // C-Major w/ a starting H
  b3: 246.94,
  c4: 261.63,
  d4: 293.66,
  e4: 329.63,
  f4: 349.23,
  g4: 392.0,
  a4: 440.0,
  b4: 493.88,
  c5: 523.25,
  // Scale for room.
  a2: 110.0,
  b2: 123.47,
  c3: 130.81,
  d3: 146.83,
  e3: 163.81,
  f3: 174.61,
  g3: 196.0,
};

// Check to see if the note exists or default to the input.
export function noteOrFreq(tone) {
  return max(0, notes[tone] || tone);
}
