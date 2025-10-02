FROM rust:1.90-alpine AS blaim-builder

RUN rustup update nightly && rustup default nightly
RUN apk add --no-cache musl-dev pkgconfig openssl-dev openssl-libs-static

WORKDIR /app

COPY . .
RUN --mount=type=cache,target=/usr/local/cargo/registry \
    --mount=type=cache,target=/app/target \
    cargo build --release --bin blaim --bin blaim-web \
    && mkdir -p bin && cp target/release/blaim target/release/blaim-web /app/bin 

# [deployment]

FROM blaim-builder AS bot
CMD ["./bin/blaim"]

FROM blaim-builder AS web
EXPOSE 8080
WORKDIR /app/blaim-web
CMD ["/app/bin/blaim-web"]