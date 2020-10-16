FROM node AS build

WORKDIR /opt/build

COPY package.json package-lock.json ./
RUN npm install

COPY . ./

ARG api_url=http://localhost:9999
ENV STRAW_POLL_API_URL $api_url

RUN npm run bundle


FROM nginx:alpine

COPY --from=build /opt/build/dist /usr/share/nginx/html
