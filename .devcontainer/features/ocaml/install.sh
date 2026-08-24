#!/bin/sh
set -eu
set -x

echo "Activating feature 'OCaml'"
PACKAGES=${PACKAGES:-$@}
OPTIONAL_PACKAGES=${OPTIONAL_PACKAGES:-}
SYSTEM_PACKAGES=${SYSTEM_PACKAGES:-}
PIN_PACKAGES=${PIN_PACKAGES:-}
REPOSITORIES=${REPOSITORIES:-}
OCAML_VERSION=${VERSION:-4.14.3}
OPAM_OPTIONS=''
if [ -n "${OPTIONS:-}" ]; then
    OPAM_OPTIONS="--packages=ocaml-variants.${OCAML_VERSION}+options,${OPTIONS}"
fi
echo "Selected OCaml:$OCAML_VERSION packages: $PACKAGES optional: ${OPTIONAL_PACKAGES} with ${OPAM_OPTIONS} ${SYSTEM_PACKAGES}"

# Package-manager detection. apt (Debian/Ubuntu) keeps the original,
# unmodified behavior below; emerge (Gentoo) is the new path. Everything
# distro-specific is isolated into the helpers this selects, so the rest of
# the script (opam init/switch/package loop, pin handling, ...) stays a
# single copy.
if command -v apt-get >/dev/null 2>&1; then
    PKG_MANAGER=apt
elif command -v emerge >/dev/null 2>&1; then
    PKG_MANAGER=portage
else
    echo "Unsupported base image: neither apt-get nor emerge found" >&2
    exit 1
fi

# From https://github.com/devcontainers/features/blob/main/src/git/install.sh
apt_get_update()
{
    if [ "$(find /var/lib/apt/lists/* | wc -l)" = "0" ]; then
        echo "Running apt-get update..."
        apt-get update -y
    fi
}

# Mirrors apt_get_update(): re-sync only when the tree looks empty, so a
# Dockerfile that already ran emerge-webrsync (as this repo's does) doesn't
# pay for a second sync here.
portage_sync()
{
    tree="${PORTDIR:-/var/db/repos/gentoo}"
    if [ ! -d "$tree" ] || [ -z "$(ls -A "$tree" 2>/dev/null)" ]; then
        echo "Running emerge-webrsync..."
        emerge-webrsync
    fi
}

# Checks if packages are installed and installs them if not
check_packages_apt() {
    if ! dpkg -s "$@" > /dev/null 2>&1; then
        apt_get_update
        if ! apt-get -o Acquire::Retries=3 -y install --no-install-recommends "$@"; then
            apt-get update -y
            apt-get -o Acquire::Retries=3 -y install --no-install-recommends "$@"
        fi
    fi
}

check_packages_portage() {
    [ "$#" -eq 0 ] && return 0
    portage_sync
    emerge --quiet --noreplace "$@"
}

check_packages() {
    if [ "$PKG_MANAGER" = apt ]; then
        check_packages_apt "$@"
    else
        check_packages_portage "$@"
    fi
}

# devcontainer.json spells system-packages using Debian package names (this
# repo passes "libgmp-dev pkg-config"); on Gentoo those names don't exist, so
# translate the ones this feature actually sees. An unmapped name passes
# through unchanged with a warning rather than failing, so a future
# Debian-only addition here doesn't hard-break the Gentoo path.
translate_packages_portage() {
    for pkg in "$@"; do
        case "$pkg" in
            libgmp-dev) echo dev-libs/gmp ;;
            pkg-config) echo dev-util/pkgconf ;;
            */*) echo "$pkg" ;;
            *)
                echo "no portage atom mapping for '$pkg', passing through as-is" >&2
                echo "$pkg"
                ;;
        esac
    done
}

export DEBIAN_FRONTEND=noninteractive

USERNAME="${USERNAME:-"${_REMOTE_USER:-"automatic"}"}"
UPDATE_RC="${UPDATE_RC:-"true"}"

# Determine the appropriate non-root user
if [ "${USERNAME}" = "auto" ] || [ "${USERNAME}" = "automatic" ]; then
    USERNAME=""
    POSSIBLE_USERS="vscode node codespace $(awk -v val=1000 -F ":" '$3==val{print $1}' /etc/passwd)"
    for CURRENT_USER in $POSSIBLE_USERS; do
        if id -u "${CURRENT_USER}" > /dev/null 2>&1; then
            USERNAME="${CURRENT_USER}"
            break
        fi
    done
    if [ "${USERNAME}" = "" ]; then
        USERNAME=root
    fi
elif [ "${USERNAME}" = "none" ] || ! id -u ${USERNAME} > /dev/null 2>&1; then
    USERNAME=root
fi

updaterc() {
    if [ "${UPDATE_RC}" = "true" ]; then
        echo "Updating /etc/bash.bashrc, /etc/zsh/zshrc and /etc/profile.d..."
        # Debian/Ubuntu vs. Gentoo spell the system-wide bash rc file
        # differently; each is a no-op if absent on this distro.
        if [ -f /etc/bash.bashrc ]; then
            /bin/echo -e "$1" >> /etc/bash.bashrc
        fi
        if [ -f /etc/bash/bashrc ]; then
            /bin/echo -e "$1" >> /etc/bash/bashrc
        fi
        if [ -f "/etc/zsh/zshrc" ]; then
            /bin/echo -e "$1" >> /etc/zsh/zshrc
        fi
        # Gentoo's /etc/bash/bashrc opts out for non-interactive shells
        # ("Proceed no further in the case of a non-interactive shell"), so
        # a login-but-non-interactive invocation (e.g. `devcontainer exec
        # bash -lc ...`, or any script/CI use of `bash -c`) never sees the
        # export above there. /etc/profile.d/*.sh has no such guard and is
        # sourced by /etc/profile for every login shell regardless of
        # interactivity, on both Debian and Gentoo, so mirror the snippet
        # there too as the reliable path.
        if [ -d /etc/profile.d ]; then
            /bin/echo -e "$1" >> /etc/profile.d/ocaml-opam.sh
        fi
    fi
}

export OPAMROOT="/opt/opam"
export OPAMYES="true"
export OPAMCONFIRMLEVEL="unsafe-yes"

rc="$(cat << EOF
# >>> OCaml >>>
export OPAMROOT="$OPAMROOT"
# <<< OCaml <<<
EOF
)"
updaterc "$rc"

if [ "$PKG_MANAGER" = apt ]; then
    check_packages ${SYSTEM_PACKAGES} opam
else
    # shellcheck disable=SC2046
    check_packages $(translate_packages_portage ${SYSTEM_PACKAGES}) dev-ml/opam
fi

export OPAMJOBS="$(getconf _NPROCESSORS_ONLN)"
opam init --no-setup --disable-sandboxing --bare
eval $(opam env)
opam switch create $OCAML_VERSION ${OPAM_OPTIONS}

if [ -n "${REPOSITORIES}" ]; then
    OLDIFS="$IFS"
    IFS=','
    for entry in ${REPOSITORIES}; do
        IFS="$OLDIFS"
        entry=$(echo "$entry" | xargs)
        if [ -n "$entry" ]; then
            repo_name=$(echo "$entry" | awk '{print $1}')
            repo_url=$(echo "$entry" | awk '{print $2}')
            opam repo add "$repo_name" "$repo_url"
        fi
    done
    IFS="$OLDIFS"
    opam update
fi

BASE_PACKAGES="\
 dune\
 ocaml-lsp-server\
 ocamlformat\
 ocamlformat-rpc\
"
OPAM_PACKAGES="${BASE_PACKAGES}"
for pkg in ${PACKAGES}; do
    case "$pkg" in
        *#*)
            pkg_name=$(echo "$pkg" | cut -d'#' -f1)
            pkg_ver=$(echo "$pkg" | cut -d'#' -f2)
            opam pin add --no-action "$pkg_name" "$pkg_ver"
            OPAM_PACKAGES="${OPAM_PACKAGES} ${pkg_name}"
            ;;
        *)
            OPAM_PACKAGES="${OPAM_PACKAGES} ${pkg}"
            ;;
    esac
done

OPTIONAL_OPAM_PACKAGES=""
for pkg in ${OPTIONAL_PACKAGES}; do
    case "$pkg" in
        *#*)
            pkg_name=$(echo "$pkg" | cut -d'#' -f1)
            pkg_ver=$(echo "$pkg" | cut -d'#' -f2)
            opam pin add --no-action "$pkg_name" "$pkg_ver"
            OPTIONAL_OPAM_PACKAGES="${OPTIONAL_OPAM_PACKAGES} ${pkg_name}"
            ;;
        *)
            OPTIONAL_OPAM_PACKAGES="${OPTIONAL_OPAM_PACKAGES} ${pkg}"
            ;;
    esac
done

if [ -n "${PIN_PACKAGES}" ]; then
    OLDIFS="$IFS"
    IFS=','
    for entry in ${PIN_PACKAGES}; do
        IFS="$OLDIFS"
        entry=$(echo "$entry" | xargs)
        if [ -n "$entry" ]; then
            case "$entry" in
                *#*)
                    pkg_name=$(echo "$entry" | cut -d'#' -f1)
                    pkg_ver=$(echo "$entry" | cut -d'#' -f2)
                    opam pin add --no-action "$pkg_name" "$pkg_ver"
                    ;;
                *)
                    pkg_name=$(echo "$entry" | awk '{print $1}')
                    opam pin add --no-action $entry
                    ;;
            esac
            OPAM_PACKAGES="${OPAM_PACKAGES} ${pkg_name}"
        fi
    done
    IFS="$OLDIFS"
fi

# Only the packages declared optional may be dropped; anything in PACKAGES or
# PIN_PACKAGES that opam cannot install must still fail the build.
for pkg in ${OPTIONAL_OPAM_PACKAGES}; do
    if [ -n "$(opam list --available -s "$pkg")" ]; then
        OPAM_PACKAGES="${OPAM_PACKAGES} ${pkg}"
    else
        echo "Skipping optional package '$pkg': not available for this switch/platform" >&2
    fi
done

opam install ${OPAM_PACKAGES}

opam clean --repo-cache
opam list
chown -R ${USERNAME}:${USERNAME} $OPAMROOT

if [ "$PKG_MANAGER" = apt ]; then
    apt-get autoremove -y
    apt-get clean -y
    rm -rf /var/lib/apt/lists/*
else
    rm -rf /var/cache/distfiles/* /var/cache/binpkgs/*
fi
