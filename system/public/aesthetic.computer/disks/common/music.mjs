// Design
export const colors = {
  notes: {
    a: [10, 115, 0],
    b: [136, 255, 0],
    c: [115, 155, 0],
    d: [255, 163, 10],
    e: [115, 5, 0],
    f: [255, 0, 230],
    g: [100, 0, 115],
    h: [0, 5, 120],
    i: [0, 5, 250],
    j: [5, 120, 255],
    k: [10, 115, 115],
    l: [10, 250, 190],
  },
};

// Data
export const noteList = "abcdefghijkl";

export const noteFrequencies = {
  a: 261.63, // [255, 0, 0] // Red
  b: 277.18,
  c: 293.66,
  d: 311.13,
  e: 329.63,
  f: 349.23,
  g: 369.99,
  h: 392.1, // TODO: Why does this need .1? (It's silent otherwise.)
  i: 415.3,
  j: 440.1, // TODO: Why does this need .1? (It's silent otherwise.)
  k: 466.16,
  l: 493.88,
};
