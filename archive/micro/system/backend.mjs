import Fastify from "fastify";
import fastifyStatic from "@fastify/static";
import { WebSocket, WebSocketServer } from "ws";
import chokidar from "chokidar";
import path from "path";
import { fileURLToPath } from 'url';

const fastify = Fastify({ logger: true });

const staticFilesDirectory = path.join(path.dirname(fileURLToPath(import.meta.url)), 'public');

fastify.register(fastifyStatic, {
  root: staticFilesDirectory 
});

fastify.get('/', (req, reply) => {
  reply.sendFile('player.html', staticFilesDirectory);
});

fastify.get('/events', async (req, reply) => {
  reply.raw.writeHead(200, {
    'Content-Type': 'text/event-stream',
    'Cache-Control': 'no-cache',
    'Connection': 'keep-alive',
  });

  // Function to send a reload message
  const sendReloadMessage = () => {
    reply.raw.write('data: reload\n\n');
  };

  // Register file system watcher
  const watcher = chokidar.watch('./public/processor.wasm');
  watcher.on('all', (event, path) => {
    if (event === "change") {
      console.log(`File ${path} has been changed`);
      sendReloadMessage();
    }
  });

  // Handle client disconnect
  req.raw.on('close', () => {
    watcher.close();
  });
});

try {
  fastify.listen({ host: "0.0.0.0", port: 8080 });
} catch (err) {
  fastify.log.error(err);
  process.exit(1);
}
