// Sotce Net, 24.06.13.06.38
// A paid diary network, 'handled' by Aesthetic Computer.

/* #region 🏁 TODO 
  - [🟢] auth0 based auth / registration
    - [] review existing ac auth implementation and necessary
          keys
    - [] Set up email sending service via google apps?
    - [] Set up login pages / logo etc. 
    - [] not logged in route
    - [] logged in route
  - [] stripe paywall
    - [] bring in necessary keys
    - [] logged in but unpaid route
    - [] logged in and paid route
    - [] this should be subscription based
  - [] The handle system would be shared among ac users.
    - [] Perhaps the subs could be 'sotce' prefixed.
  - [] Store whether a user is subscribed with an expiration date.
  - [] 
  + Done
  - [x] Set up path->method routing in `sotce-net.js`.
#endregion */

// ♻️ Environment
const AUTH0_CLIENT_ID = process.env.SOTCE_AUTH0_M2M_CLIENT_ID;
const AUTH0_SECRET = process.env.SOTCE_AUTH0_M2M_SECRET;

// 📚 Libraries
// TODO: - [] Auth0 serverside.
//       - [] Auth0 clientside.

export const handler = async (event, context) => {
  // console.log("Event:", event);
  // console.log("Context:", context);
  // console.log("Path:", event.path);

  // 🚙 Router
  const method = event.httpMethod.toLowerCase();
  let path = event.path;
  if (path.startsWith("/sotce-net")) path = path.replace("/sotce-net", "/").replace("//", "/");


  const body = `
    <html><body><h1>${path} : ${method}</h1></body></html>
  `;

  return { body, statusCode: 200, headers: { "Content-Type": "text/html" } };
};
