FROM node:24.18.1
WORKDIR /app
COPY .env .
COPY session.mjs .
COPY chat-manager.mjs .
COPY filter.mjs .
COPY redact.mjs .
COPY package.json .
COPY package-lock.json .
RUN npm install
CMD ["node", "session.mjs"]
