FROM archlinux:base

RUN pacman -Syu --noconfirm \
  archiso \
  git \
  rsync \
  sudo \
  base-devel \
  jq \
  && pacman -Scc --noconfirm

# Create a non-root user for safer operations
RUN useradd -m builder && \
  echo "builder ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers

USER builder
WORKDIR /work

ENTRYPOINT ["/bin/bash", "-lc"]
