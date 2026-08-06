ARG VARIANT=13
FROM debian:${VARIANT}
# Cross toolchains for tools/compcert-cross-smoke.sh. Pure "-linux-gnu"
# cross-gcc packages only - gcc-multilib conflicts with the arm/aarch64
# ones. Debian doesn't build these from armhf hosts, only
# gcc-arm-linux-gnueabihf; the script skips targets missing their package.
RUN set -eux;\
    apt-get update;\
    case "$(dpkg --print-architecture)" in\
      armhf|armel) cross='gcc-arm-linux-gnueabihf libc6-dev-armhf-cross' ;;\
      *) cross='gcc-i686-linux-gnu libc6-dev-i386-cross gcc-x86-64-linux-gnu libc6-dev-amd64-cross gcc-arm-linux-gnueabihf libc6-dev-armhf-cross gcc-aarch64-linux-gnu libc6-dev-arm64-cross' ;;\
    esac;\
    env DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends\
  ca-certificates\
  curl\
  git\
  make\
  qemu-user\
  wget\
  xz-utils\
  $cross\
;\
    apt-get autoremove -y; apt-get clean -y ; apt-get clean; rm -rf /var/lib/apt/lists/*;\
    echo DONE
