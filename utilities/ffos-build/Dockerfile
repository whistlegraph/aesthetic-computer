FROM archlinux:base

RUN pacman -Syu --noconfirm \
  archiso \
  git \
  rsync \
  sudo \
  base-devel \
  jq \
  && pacman -Scc --noconfirm

# mkarchiso requires root for chroot mount operations
WORKDIR /work

ENTRYPOINT ["/bin/bash", "-lc"]
