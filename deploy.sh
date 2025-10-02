#!/usr/bin/env bash

set -xeuo pipefail

echo "Building docker image..."

docker build  -t blaim-builder:latest  --target blaim-builder .

echo "Saving compressed image..."

docker save blaim-builder:latest | pv | zstd > target/blaim-builder.zstd

echo "Build complete. Copying image to remote server"

scp target/blaim-builder.zstd seds@blaim.strathseds.org:/home/seds/builder.zstd

echo "Pushing repository to remote"
git push -f deploy HEAD:master

echo "Starting remote deployment"

ssh root@blaim.strathseds.org << EOF
    set -xeuo pipefail
    docker image rm blaim-builder
    zstd -d < /home/seds/builder.zstd | docker load

    # Remove build step from Dockerfile
    cd /home/seds/blaim
    sed -i '1,/\[deployment\]/d' Dockerfile

    systemctl restart blaim
EOF
