// HTTP, 23.05.01.21.14
// Helpers for making backend functions more nicely
// written.

function respond(statusCode, body, headers = {}) {
  if (!headers["Content-Type"]) headers["Content-Type"] = "application/json"; // Default return a JSON reply.

  if (!headers["Access-Control-Allow-Origin"]) {
    headers["Access-Control-Allow-Origin"] = "*";
  }

  const res = { statusCode, headers };
  res.body = typeof body === "object" ? JSON.stringify(body) : body;

  return res;
}

function pathParams(path) {
  if (path.startsWith("/")) path = path.slice(1);
  return path.split("/");
}

export { respond, pathParams };