// In-memory Mongo subset used by account isolation and revision tests.
export function memoryDb() {
  const collections = new Map();
  const matches = (d, q) => Object.entries(q).every(([k, v]) => d[k] === v);
  return {
    collection(name) {
      if (!collections.has(name)) collections.set(name, []);
      const docs = collections.get(name);
      return {
        async insertOne(doc) {
          docs.push(structuredClone(doc));
        },
        async findOne(q) {
          return structuredClone(docs.find((d) => matches(d, q)) ?? null);
        },
        async countDocuments(q) {
          return docs.filter((d) => matches(d, q)).length;
        },
        async updateOne(q, update) {
          const d = docs.find((d) => matches(d, q));
          if (!d) return { matchedCount: 0 };
          Object.assign(d, structuredClone(update.$set || {}));
          for (const k of Object.keys(update.$unset || {})) delete d[k];
          return { matchedCount: 1 };
        },
        async deleteOne(q) {
          const i = docs.findIndex((d) => matches(d, q));
          if (i >= 0) docs.splice(i, 1);
        },
        async findOneAndDelete(q) {
          const i = docs.findIndex((d) => matches(d, q));
          return i < 0 ? null : docs.splice(i, 1)[0];
        },
        async deleteMany(q) {
          for (let i = docs.length - 1; i >= 0; i--)
            if (matches(docs[i], q)) docs.splice(i, 1);
        },
        async createIndex() {},
        find(q) {
          let rows = docs.filter((d) => matches(d, q));
          return {
            sort(order) {
              const [key, dir] = Object.entries(order)[0];
              rows.sort(
                (a, b) =>
                  dir * (a[key] > b[key] ? 1 : a[key] < b[key] ? -1 : 0),
              );
              return this;
            },
            limit(n) {
              rows = rows.slice(0, n);
              return this;
            },
            async toArray() {
              return structuredClone(rows);
            },
          };
        },
      };
    },
  };
}
