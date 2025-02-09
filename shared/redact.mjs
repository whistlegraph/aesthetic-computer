// redact, 25.02.08.01.45
// Used for redacting user information like chat messages and masking it
// before rendering and/or sending to clients.

// Modify a message object into a 'redacted' state.
export function redact(msg) {
  console.log("💌 Redacting:", msg);
  msg.redactedText = msg.text;
  msg.text = msg.text.replace(/\S/g, "_"); // Replace non-space chars with
  //                                          underscores.
  // msg.from = "redacted";
}

// Revert a message out of a 'redacted' state.
export function unredact(msg) {
  console.log("💌 Unredacting:", msg);
  msg.text = msg.redactedText || msg.text;
  // msg.from = "redacted";
}