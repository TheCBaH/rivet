ARG VARIANT=13
FROM debian:${VARIANT}
ARG NODE_VERSION=24.19.0
RUN set -eux;\
    apt-get update;\
    ARCH="$(dpkg --print-architecture)";\
    case "$ARCH" in\
      armhf|armel) cross='gcc-arm-linux-gnueabihf libc6-dev-armhf-cross' ;;\
      *) cross='gcc-i686-linux-gnu libc6-dev-i386-cross gcc-x86-64-linux-gnu libc6-dev-amd64-cross gcc-arm-linux-gnueabihf libc6-dev-armhf-cross gcc-aarch64-linux-gnu libc6-dev-arm64-cross binutils-riscv64-linux-gnu gcc-riscv64-linux-gnu' ;;\
    esac;\
    env DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends\
  ca-certificates\
  curl\
  gdb-multiarch\
  git\
  make\
  python3\
  qemu-system-arm\
  qemu-system-x86\
  qemu-user\
  ripgrep\
  wget\
  xz-utils\
  $cross\
;\
    case "$ARCH" in\
      amd64) node_arch=x64 ;;\
      arm64) node_arch=arm64 ;;\
      *) node_arch='' ;;\
    esac;\
    if [ -n "$node_arch" ]; then\
      node_pkg="node-v${NODE_VERSION}-linux-${node_arch}";\
      curl -fsSLO "https://nodejs.org/dist/v${NODE_VERSION}/${node_pkg}.tar.xz";\
      curl -fsSLO "https://nodejs.org/dist/v${NODE_VERSION}/SHASUMS256.txt";\
      grep " ${node_pkg}.tar.xz\$" SHASUMS256.txt | sha256sum -c -;\
      tar -xJf "${node_pkg}.tar.xz" -C /usr/local --strip-components=1 --no-same-owner;\
      rm "${node_pkg}.tar.xz" SHASUMS256.txt;\
    fi;\
    apt-get autoremove -y; apt-get clean -y ; apt-get clean; rm -rf /var/lib/apt/lists/*;\
    echo DONE
