// Keep drafts per Auth0 identity. The engine's account record stays in memory;
// credential persistence and renewal belong to the Auth0 SDK alone.
export function accountStorage(backing, subject) {
  if (!subject) throw new Error("An account is required to open saved pieces.");
  const prefix = `aesel.web.v1:${encodeURIComponent(subject)}:`;
  let account = null;
  return {
    get(key) {
      if (key !== "session") return backing.getItem(prefix + key);
      return account || backing.getItem(prefix + 'position');
    },
    set(key, value) {
      if (key === "session") {
        account = value;
        const { threadID } = JSON.parse(value);
        if (threadID) backing.setItem(prefix + 'position', JSON.stringify({threadID}));
      }
      else backing.setItem(prefix + key, value);
    },
  };
}
