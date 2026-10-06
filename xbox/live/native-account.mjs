// The native host keeps credentials; this bridge only reads public account state.
export function createNativeAccountBridge({ read, act, validate, now = Date.now }) {
  let next = 0, state = { status: 'signed-out' }, fighter = null;
  return {
    refresh() {
      const at = now();
      if (at >= next) {
        next = at + 500;
        try {
          state = read() || { status: 'unavailable' };
          fighter = null;
          const saved = state.savedFighter;
          if (state.status === 'signed-in' && saved &&
              saved.handle?.toLowerCase() === '@' + String(state.handle).replace(/^@/, '').toLowerCase() &&
              Number.isFinite(saved.validUntil) && saved.validUntil > at) {
            fighter = { appearance: validate(saved.fighter), handle: saved.handle,
              validUntil: Math.min(saved.validUntil, at + 30000) };
          }
        } catch { state = { status: 'error', error: 'Account unavailable. Try again.' }; fighter = null; }
      }
      if (fighter && fighter.validUntil <= at) fighter = null;
      return { state, fighter };
    },
    action(name) {
      if (!['login', 'logout', 'cancel'].includes(name)) return;
      next = 0; fighter = null;
      act(name);
    },
  };
}
globalThis.__oskiewarCreateNativeAccount = createNativeAccountBridge;
