FROM --platform=linux/amd64 archlinux:base

RUN pacman -Syu --noconfirm \
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
  && pacman -Scc --noconfirm

# mkarchiso requires root for chroot mount operations
# makepkg needs a non-root user, so we create one
RUN useradd -m builder && \
  echo "builder ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers

WORKDIR /work

ENTRYPOINT ["/bin/bash", "-lc"]
