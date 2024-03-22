// 📚 Helpers

/*
export function times(n, fun) {
  const accum = Array(Math.max(0, n));
  for (let i = 0; i < n; i += 1) accum[i] = fun();
  return accum;
}
*/

// Generate a sealed object with named keys set to undefined.
export function apiObject() {
  const obj = {};
  for (const key of arguments) obj[key] = undefined;
  return Object.seal(obj);
}

export function extension(filename) {
  // https://stackoverflow.com/a/680982
  return /(?:\.([^.]+))?$/.exec(filename)[1];
}

// Returns true if the object is not an Array.
export function notArray(obj) {
  return !Array.isArray(obj);
}

// Wraps anything other than undefined & null that isn't an array with `[any]`.
// Undefined or null yields: `[]`.
export function wrapNotArray(any) {
  if (any !== undefined && any !== null && notArray(any)) return [any];
  else if (Array.isArray(any)) return any;
  else return [];
}

// Returns content remaining after the last "/" of a string.
// Used for URL path resolution.
export function pathEnd(path) {
  return path.substring(path.lastIndexOf("/") + 1);
}

// Default template string behavior: https://stackoverflow.com/a/64298689/8146077
export function defaultTemplateStringProcessor(strings, ...vars) {
  let result = "";
  strings.forEach((str, i) => {
    result += `${str}${i === strings.length - 1 ? "" : vars[i]}`;
  });
  return result;
}

export function utf8ToBase64(str) {
  let bytes = new TextEncoder().encode(str);
  let binary = String.fromCharCode(...bytes);
  return btoa(binary);
}

export function uint8ArrayToBase64(buffer) {
  let binary = "";
  let bytes = new Uint8Array(buffer);
  let len = bytes.byteLength;
  for (let i = 0; i < len; i++) binary += String.fromCharCode(bytes[i]);
  return btoa(binary);
}

export function base64ToUint8Array(base64) {
  let binaryString = atob(base64);
  let len = binaryString.length;
  let bytes = new Uint8Array(len);
  for (let i = 0; i < len; i++) {
    bytes[i] = binaryString.charCodeAt(i);
  }
  return bytes;
}