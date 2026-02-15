FROM --platform=linux/amd64 archlinux:base

# Disable pacman sandbox to avoid seccomp issues in Docker
RUN pacman --disable-sandbox -Syu --noconfirm \
  archiso \
  git \
  rsync \
  sudo \
  base-devel \
  jq \
  go \
  rust \
  curl \
  openssl \
  && pacman --disable-sandbox -Scc --noconfirm

# mkarchiso requires root for chroot mount operations
# makepkg needs a non-root user, so we create one
RUN useradd -m builder && \
  echo "builder ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers

WORKDIR /work

ENTRYPOINT ["/bin/bash", "-lc"]
