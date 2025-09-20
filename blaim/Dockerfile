FROM rust:1.90-alpine AS builder

RUN apk add --no-cache musl-dev pkgconfig openssl-dev

WORKDIR /app

# Copy source and build
COPY . .
RUN cargo build --release --bin blaim

# Stage 2: Runtime
FROM alpine:3.20

WORKDIR /app
COPY --from=builder /app/target/release/blaim /app/blaim

CMD ["./blaim"]
