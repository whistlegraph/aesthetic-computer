// HTTP, 23.05.01.21.14
// Helpers for making backend functions more nicely
// written.

function respond(statusCode, body, headers) {
  const res = { statusCode, headers };
  if (body) res.body = JSON.stringify(body);
  return res;
}

function pathParams(path) {
  if (path.startsWith("/")) path = path.slice(1);
  return path.split("/");
}

export { respond, pathParams };
