FROM rust:1.90-alpine AS builder

RUN rustup update nightly && rustup default nightly
RUN apk add --no-cache musl-dev pkgconfig openssl-dev openssl-libs-static

WORKDIR /app
COPY . .

# Build offline against the committed .sqlx/ directory
ENV SQLX_OFFLINE=true \
    OPENSSL_STATIC=1

RUN --mount=type=cache,target=/usr/local/cargo/registry \
    --mount=type=cache,target=/app/target \
    cargo build --release --bin blaim --bin blaim-web \
    && mkdir -p /out \
    && cp target/release/blaim target/release/blaim-web /out/

# [deployment]

FROM alpine:3.20 AS runtime-base
RUN apk add --no-cache ca-certificates
WORKDIR /app

FROM runtime-base AS bot
COPY --from=builder /out/blaim /usr/local/bin/blaim
CMD ["blaim"]

FROM runtime-base AS web
WORKDIR /app/blaim-web
# Add anything blaim-web reads at runtime:
COPY --from=builder /app/blaim-web/pkg /app/blaim-web/pkg
COPY --from=builder /app/blaim-web/templates /app/blaim-web/templates
COPY --from=builder /out/blaim-web /usr/local/bin/blaim-web
EXPOSE 8080
CMD ["blaim-web"]