// Shared chat/laklok entry syntax. The actual invitation is always delivered
// through the authenticated, targeted fight coordinator.

export function parseFightChallenge(text) {
  const match = String(text || "").trim().match(/^fight\s+@([a-z0-9][a-z0-9_-]{0,31})$/i);
  return match ? { target: `@${match[1].toLowerCase()}` } : null;
}

export function parseFightCommand(text) {
  const challenge = parseFightChallenge(text);
  if (challenge) return { action: "challenge", ...challenge };
  const action = String(text || "").trim().match(/^fight\s+(accept|decline)$/i)?.[1];
  return action ? { action: action.toLowerCase() } : null;
}
