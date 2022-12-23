FROM node:lts-gallium
WORKDIR /app
COPY session.mjs .
COPY package.json .
COPY package-lock.json .
RUN npm install
CMD ["node", "session.mjs"]