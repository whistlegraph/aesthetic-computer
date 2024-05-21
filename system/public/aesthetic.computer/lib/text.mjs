// Text, 23.01.03.15.03 
// Helpers and utility functions for manipulating strings and text. 

function capitalize(word) {
  return word.charAt(0).toUpperCase() + word.slice(1);
}

// 🙆 👍 Validates a user handle. ✅
// (Used on client and server)

// 🧑‍🚒
// TODO: Why have client validation actually?
// 💦 Or why not just only check for profanities here?
// TODO: Give a reason for an invalid handle.
// Reasons: length, naughty.
// They cannot contain profanities! 

// Handles must be within 1 and 16 characters.
// They can use a-z and 0-9, underscores and periods.
// They cannot begin or end with underscores or periods.
function validateHandle(handle) {
  if (!/^[a-z0-9]+([._][a-z0-9]+)*$/i.test(handle) || handle.length > 16) {
    return false;
  }
  return true;
}

export { capitalize, validateHandle }