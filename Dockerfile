FROM alpine:3.8

RUN apk add --update guile
WORKDIR /app
COPY . .
ENTRYPOINT ["./metacircular.scm"]
