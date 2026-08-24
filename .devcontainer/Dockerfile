FROM gentoo/stage3:latest

ARG NODE_VERSION=24.19.0

# Gentoo/OCaml checkpoint (see .ai/devcontainer-cli.md): the vscode user,
# zsh, sudo, and general interactive-shell tooling come from the
# ghcr.io/thecbah/devcontainer.features/common-utils feature (see
# devcontainer.json); OCaml/opam from ./features/ocaml. This layer covers
# everything the previous Debian-based Dockerfile had beyond that: cross
# toolchains (i686/x86_64/arm/aarch64/riscv32/riscv64-linux-gnu, via
# crossdev), qemu-user/qemu-system emulation, gdb, git and Node.js.
#
# ghcr.io/devcontainers/features/common-utils hard-fails on any
# /etc/os-release ID it doesn't special-case (debian/rhel/alpine only - see
# its main.sh), so upstream doesn't support Gentoo.
# ghcr.io/thecbah/devcontainer.features/common-utils is a fork
# (https://github.com/TheCBaH/devcontainer.features, branch
# gentoo-common-utils, PR against upstream devcontainers/features pending)
# of that same feature with a "gentoo" branch added (install_gentoo_packages,
# plus the bash rc-file path Gentoo actually uses) - everything else,
# including the already-distro-generic user/sudo setup, is untouched
# upstream behavior.
RUN set -eux; \
    emerge-webrsync; \
    printf '\nMAKEOPTS="-j%s"\n' "$(nproc)" >> /etc/portage/make.conf; \
    # gentoo/stage3 already ships /etc/portage/binrepos.conf/gentoo.conf
    # pointing at the official arch-matched binhost
    # (https://wiki.gentoo.org/wiki/Gentoo_Binary_Host_Quickstart) - this just
    # turns on using it. Standard packages (git, gdb, ripgrep, crossdev
    # itself, ...) get a binary hit and skip compiling entirely; anything
    # with non-default USE/USE_EXPAND (qemu here, via QEMU_*_TARGETS below)
    # or that isn't on the public binhost at all (the crossdev-built cross
    # toolchains - host x target combinations too specific to be published
    # there) transparently falls back to building from source, so this is
    # purely additive.
    printf '\nFEATURES="getbinpkg"\n' >> /etc/portage/make.conf; \
    # qemu-user targets match asm/tools/lib/target.ml's qemu_bin per target
    # (qemu-i386, qemu-x86_64, qemu-arm, qemu-aarch64, qemu-riscv32,
    # qemu-riscv64); qemu-system targets (arm, x86_64) mirror the old
    # Debian image's qemu-system-arm/qemu-system-x86.
    printf '\nQEMU_SOFTMMU_TARGETS="arm x86_64"\nQEMU_USER_TARGETS="i386 x86_64 arm aarch64 riscv32 riscv64"\n' >> /etc/portage/make.conf; \
    emerge --quiet --noreplace \
      dev-vcs/git \
      dev-debug/gdb \
      sys-apps/ripgrep \
      app-misc/ca-certificates \
      net-misc/wget \
      app-arch/xz-utils \
      app-emulation/qemu \
      sys-devel/crossdev \
    ; \
    mkdir -p /etc/portage/repos.conf /var/db/repos/crossdev; \
    for t in i686-linux-gnu x86_64-linux-gnu arm-linux-gnueabihf aarch64-linux-gnu riscv32-linux-gnu riscv64-linux-gnu; do \
      # --env (not --genv) scopes to every crossdev stage - binutils, gcc
      # *and* glibc - not just gcc: this host's stock MAKEOPTS OOM-killed
      # cc1plus building gcc's generated matcher files at -j"$(nproc)" on a
      # memory-constrained host, and glibc is cheap enough at -j2 to not be
      # worth a separate override.
      crossdev -t "$t" --ov-output /var/db/repos/crossdev --env 'MAKEOPTS="-j2"'; \
      rm -rf /var/cache/distfiles/* /var/cache/binpkgs/* /var/tmp/portage/*; \
    done; \
    NODE_ARCH="$(uname -m | sed -e 's/x86_64/x64/' -e 's/aarch64/arm64/' -e 's/armv7l/armv7l/')"; \
    NODE_PKG="node-v${NODE_VERSION}-linux-${NODE_ARCH}"; \
    cd /tmp; \
    wget -q "https://nodejs.org/dist/v${NODE_VERSION}/${NODE_PKG}.tar.xz" "https://nodejs.org/dist/v${NODE_VERSION}/SHASUMS256.txt"; \
    grep " ${NODE_PKG}.tar.xz\$" SHASUMS256.txt | sha256sum -c -; \
    tar -xJf "${NODE_PKG}.tar.xz" -C /usr/local --strip-components=1 --no-same-owner; \
    rm -f "${NODE_PKG}.tar.xz" SHASUMS256.txt; \
    cd /; \
    rm -rf /var/cache/distfiles/* /var/cache/binpkgs/*; \
    echo DONE
