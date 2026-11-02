const routers = new WeakMap();

// One upgrade listener must own both dispatch and rejection. Independent
// listeners that ignore unknown paths leave those sockets open until timeout.
export function attachSocketRoute(server, paths, handler, { allowQuery = false } = {}) {
  let router = routers.get(server);
  if (!router) {
    const routes = new Map();
    const upgrade = (request, socket, head) => {
      let route = routes.get(request.url);
      if (!route && request.url.includes('?')) {
        const candidate = routes.get(request.url.split('?')[0]);
        if (candidate?.allowQuery) route = candidate;
      }
      if (route) return route.handler(request, socket, head);
      socket.on("error", () => {});
      socket.end("HTTP/1.1 404 Not Found\r\nConnection: close\r\nContent-Length: 0\r\n\r\n", () => socket.destroy());
    };
    router = { routes, upgrade };
    routers.set(server, router);
    server.on("upgrade", upgrade);
  }
  if (paths.some(path => router.routes.has(path))) throw Error("Socket route already registered");
  for (const path of paths) router.routes.set(path, { handler, allowQuery });
  let detached = false;
  return () => {
    if (detached) return;
    detached = true;
    for (const path of paths) router.routes.delete(path);
    if (!router.routes.size) {
      server.off("upgrade", router.upgrade);
      routers.delete(server);
    }
  };
}
