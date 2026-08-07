// Public-release allowlist. An episode can be rendered and staged privately
// without appearing here. Add it only after both narrators approve the master.
export const RELEASED = {};

export const released = (slug) => Boolean(RELEASED[slug]);

