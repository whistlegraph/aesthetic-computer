// HTTP, 23.05.01.21.14
// Helpers for making backend functions more nicely
// written.

function respond(statusCode, body, headers = {}) {
  headers["Content-Type"] = "application/json"; // Always return a JSON reply.

  if (!headers["Access-Control-Allow-Origin"]) {
    headers["Access-Control-Allow-Origin"] = "*";
  }

  const res = { statusCode, headers };
  if (body) res.body = JSON.stringify(body);
  return res;
}

function pathParams(path) {
  if (path.startsWith("/")) path = path.slice(1);
  return path.split("/");
}

export { respond, pathParams };
