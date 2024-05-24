FROM node:lts-iron
WORKDIR /app
COPY .env .
COPY session.mjs .
COPY filter.mjs .
COPY package.json .
COPY package-lock.json .
RUN npm install
CMD ["node", "session.mjs"]
