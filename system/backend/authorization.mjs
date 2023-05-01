// Authorization, 23.04.30.17.47

// Authenticates a user to make sure they are logged in
// and their local keys match the user database.
// 🧠 (So that they can run authorized server functions.)

export async function authorize({ authorization }) {
  try {
    const { got } = await import("got");
    return (
      await got("https://auth0.aesthetic.computer/userinfo", {
        headers: {
          Authorization: authorization,
        },
        responseType: "json",
      })
    ).body;
  } catch {
    return undefined;
  }
}
