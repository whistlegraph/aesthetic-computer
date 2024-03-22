FROM node:lts-hydrogen
WORKDIR /app
COPY .env .
COPY aesthetic-computer-firebase-adminsdk-79w8j-5b5cdfced8.json .
COPY session.mjs .
COPY filter.mjs .
COPY package.json .
COPY package-lock.json .
RUN npm install
CMD ["node", "session.mjs"]