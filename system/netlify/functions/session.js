// Session
// Produces a valid URL for a given session backend.

/* #region todo 📓 
+ Done
- [x] Simplified to unified session server (removed Jamsocket)
- [x] Production points to session-server.aesthetic.computer for both WS and UDP
#endregion */

const dev = process.env.NETLIFY_DEV;

async function fun(event, context) {
  const forceProd = parseInt(event.queryStringParameters.forceProduction) === 1;

  if (dev && !forceProd) {
    // Local development
    let host = event.headers.host.split(":")[0];
    
    if (host === "local.aesthetic.computer") {
      return {
        statusCode: 200,
        body: JSON.stringify({ 
          url: `https://session.${host}`, 
          udp: `https://session.${host}`,
          state: "Ready"
        }),
        headers: { "Access-Control-Allow-Origin": "*" },
      };
    } else {
      return {
        statusCode: 200,
        body: JSON.stringify({ 
          url: `http://${host}:8889`, 
          udp: `http://${host}:8889`,
          state: "Ready"
        }),
        headers: { "Access-Control-Allow-Origin": "*" },
      };
    }
  } else {
    // Production - unified session server (both WebSocket and UDP)
    return {
      statusCode: 200,
      body: JSON.stringify({
        url: `https://session-server.aesthetic.computer`,
        udp: `https://session-server.aesthetic.computer`,
        state: "Ready"
      }),
      headers: { "Access-Control-Allow-Origin": "*" },
    };
  }
}

export const handler = fun;
