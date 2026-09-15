export function memoryWipRepository() {
  const paintings = new Map(), states = new Map();
  let nextCode = 0, nextState = 0;
  const matches = (value, query) => Object.entries(query).every(([key, expected]) =>
    key.split(".").reduce((v, name) => v?.[name], value) === expected);
  return {
    paintings, states,
    find: async (query) => structuredClone([...paintings.values()].find((p) => matches(p, query)) || null),
    code: async () => ++nextCode === 1 ? "test" : `test${nextCode}`,
    insert: async (painting) => {
      if ([...paintings.values()].some((p) => p.code === painting.code || p.wip.id === painting.wip.id)) {
        const error = new Error("duplicate"); error.code = 11000; throw error;
      }
      painting._id = painting.code;
      paintings.set(painting.code, structuredClone(painting));
    },
    update: async (query, fields) => {
      const record = [...paintings.values()].find((p) => matches(p, query));
      if (!record) return false;
      Object.assign(record, structuredClone(fields));
      return true;
    },
    writeState: async (data) => { const id = ++nextState; states.set(id, Buffer.from(data)); return id; },
    readState: async (id) => { if (!states.has(id)) throw new Error("missing snapshot"); return states.get(id); },
    deleteState: async (id) => { states.delete(id); },
  };
}
