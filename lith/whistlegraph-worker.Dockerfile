# A Whistlegraph turn worker (apple/whistlegraph/TURNS.md): the model loop, the
# render pool (Chromium) and the picture review, pulling turns from the queue.
# Build from the repo root:  docker build -f lith/whistlegraph-worker.Dockerfile -t whistlegraph-worker .
# Run:  docker run --env-file worker.env whistlegraph-worker
#   worker.env: MONGODB_CONNECTION_STRING, MONGODB_NAME, WHISTLEGRAPH_WORKER_SECRET,
#               AC_SITE (default https://aesthetic.computer), WORKER_NAME, TURN_CONCURRENCY, RENDER_CONCURRENCY
FROM node:22-slim
RUN apt-get update && apt-get install -y --no-install-recommends chromium fonts-liberation ca-certificates \
    && rm -rf /var/lib/apt/lists/*
ENV CHROME_PATH=/usr/bin/chromium PUPPETEER_SKIP_DOWNLOAD=1 NODE_ENV=production
WORKDIR /app
# Only what the worker imports: the engine's modules, the shared Aesel sources, the backend store and the queue.
COPY package.json package-lock.json ./
RUN npm ci --omit=dev --no-audit --no-fund && npm install --no-save --no-audit --no-fund puppeteer@24 mongodb@6 ws@8
COPY aesel/src ./aesel/src
COPY apple/whistlegraph/Resources/Web ./apple/whistlegraph/Resources/Web
COPY system/backend ./system/backend
COPY lith/whistlegraph-worker.mjs lith/whistlegraph-render.mjs ./lith/
RUN groupadd -r worker && useradd -r -g worker -m worker && chown -R worker:worker /app
USER worker
CMD ["node", "lith/whistlegraph-worker.mjs"]
