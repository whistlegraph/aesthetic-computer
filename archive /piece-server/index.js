// HTTP Server
import Fastify from "fastify";

const fastify = Fastify({
  logger: true,
});

// Declare a route...
fastify.get("/", async (request, reply) => {
  return { pieces: "aesthetic.computer" };
});

// Run the http server!
const start = async () => {
  try {
    await fastify.listen({ port: 8080 });
  } catch (err) {
    fastify.log.error(err);
    process.exit(1);
  }
};
start();

// File Watcher
import got from "got";
import chokidar from "chokidar";

chokidar
  .watch([
    "/home/digitpain/pieces",
    "/home/deafbeef/pieces",
    "/home/p1xelfool/pieces",
    "/home/artur/pieces",
    "/home/mxsage/pieces",
    "/home/maya/pieces",
  ])
  .on("all", async (event, path) => {
    if (event === "change") {
      const splitPath = path.split("/");
      console.log(splitPath);
      const user = "@" + splitPath[2];
      const piece = splitPath[splitPath.length - 1].replace(".mjs", "");

      // TODO: This could probably make use of token validation so it isn't abused.
      const data = await got
        .post("https://aesthetic.computer/reload", {
          json: {
            piece: user + "/" + piece,
          },
        })
        .json();
    }
  });

// TODO
// - [x] Add daemon with pm2
//       https://www.digitalocean.com/community/tutorials/how-to-set-up-a-node-js-application-for-production-on-centos-7
