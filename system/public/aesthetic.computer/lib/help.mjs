import * as num from "./num.mjs";

// Randomly returns one of the arguments.
export function choose() {
  return arguments[num.randInt(arguments.length - 1)];
}

// Set every property of an object to a certain value.
export function every(obj, value) {
  Object.keys(obj).forEach((k) => (obj[k] = value));
}

// Returns a random value from an object.
export function any(obj) {
  const keys = Object.keys(obj);
  return obj[keys[(keys.length * Math.random()) << 0]];
}

// Returns a random key from an object.
export function anyKey(obj) {
  const keys = Object.keys(obj);
  return keys[num.randInt(keys.length - 1)];
}

// Shuffles an array: https://stackoverflow.com/a/2450976
export function shuffleInPlace(array) {
  let currentIndex = array.length,
    randomIndex;

  // While there remain elements to shuffle.
  while (currentIndex != 0) {
    // Pick a remaining element.
    randomIndex = Math.floor(Math.random() * currentIndex);
    currentIndex--;

    // And swap it with the current element.
    [array[currentIndex], array[randomIndex]] = [
      array[randomIndex],
      array[currentIndex],
    ];
  }
  return array;
}

// Run a function on every value in an object.
// Ex. each(obj, (value, key) => console.log(value, key));
export function each(obj, fn) {
  Object.entries(obj).forEach(([key, obj]) => fn(obj, key));
}

// Run a function `n` times, passing in `i` on each iteration
// and returning an array of the results.
export function repeat(n, fn) {
  const reps = [];
  for (let i = 0; i < n; i += 1) reps.push(fn(i));
  return reps;
}

// Find a key value pair match inside an object of objects, 0 levels deep.
export function findKeyAndValue(obj, k, v) {
  return obj[Object.keys(obj).find((key) => obj[key][k] === v)];
}