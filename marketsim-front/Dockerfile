FROM node:20.3-alpine

COPY ./ /app

WORKDIR /app

RUN npm install && npm run build

CMD ["npm", "run", "start"]
