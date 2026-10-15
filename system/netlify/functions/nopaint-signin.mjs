import { createNoPaintSignIn } from "../../backend/nopaint-signin.mjs";
let live;
export async function handler(event) {
  live ||= (async () => {
    const [{ connect }, { authorize }, { handler: pairHandler }] =
      await Promise.all([
        import("../../backend/database.mjs"),
        import("../../backend/authorization.mjs"),
        import("./device-pair.mjs"),
      ]);
    const { db } = await connect();
    const links = db.collection("nopaint-login-links");
    await links.createIndex({ expiresAt: 1 }, { expireAfterSeconds: 0 });
    return createNoPaintSignIn({
      pairs: db.collection("device-pairs"),
      links,
      pairHandler,
      authorize,
    });
  })().catch((error) => {
    live = null;
    throw error;
  });
  try {
    return await (
      await live
    )(event);
  } catch {
    return {
      statusCode: 503,
      headers: {
        "Content-Type": "application/json",
        "Cache-Control": "no-store",
      },
      body: JSON.stringify({ error: "Sign-in is temporarily unavailable." }),
    };
  }
}
