# Get the latest fedora version.
FROM fedora:latest
# Update all packages.
RUN dnf update -y

# Install git, gcc, clang, and emacs (without x support) and python and the github cli
# And procps for the `ps` command that linux brew requires.
# And the fish shell, and redis.
# And emacs vterm.
RUN dnf install -y git gcc clang emacs-nox python3 python3-pip gh procps fish util-linux-user redis cmake libtool libvterm

# Add 'me' user.
RUN useradd -m me && \
    # Optional: Set password for the user (replace 'password' with your desired password)
    # echo "me:micro" | chpasswd && \
    # Give 'me' sudo privileges without password
    echo "me ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers

RUN chsh -s /usr/bin/fish me

# Set up homebrew permissions.
RUN mkdir -p /home/linuxbrew/.linuxbrew && \
    chown -R me:me /home/linuxbrew/.linuxbrew

# Download and install the Stripe CLI
RUN curl -sL https://github.com/stripe/stripe-cli/releases/download/v1.19.2/stripe_1.19.2_linux_x86_64.tar.gz | tar xz -C /usr/local/bin

# Switch to current user.
USER me

# Set working directory to me user.
WORKDIR /home/me

# Install shell-gpt for the user.
RUN pip3 install shell-gpt

# Install linux brew.
RUN /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Set environment variable for Homebrew
ENV PATH="/home/linuxbrew/.linuxbrew/bin:/home/linuxbrew/.linuxbrew/sbin:$PATH"

# Install web assembly tools.
# RUN brew install wabt

# Install local ssl certificate generator.
RUN brew install mkcert

# Install tree-sitter, for emacs parsing.
# RUN brew install tree-sitter

# Install fnm, configure it, install Node.js, and global npm packages
RUN brew install fnm && \
    eval "$(fnm env)" && \
    fnm install lts-hydrogen && \
    fnm use lts-hydrogen && \
    npm install -g prettier typescript typescript-language-server

# Copy over a custom xdg-open for opening browser URLs on the host.
COPY xdg-open.fish /usr/local/bin/xdg-open
RUN sudo chmod +x /usr/local/bin/xdg-open

# Copy emacs config.
COPY emacs.el .emacs
RUN sudo chown me:me .emacs
RUN mkdir -p .emacs.d

# Expose the web port.
# EXPOSE 8888

# Copy the fish configuration and entry point script.
RUN mkdir -p .config/fish && \
    mkdir -p .config/fish/conf.d
COPY config.fish .config/fish/config.fish

# Add fnm configuration to Fish shell startup script
RUN echo "fnm env --use-on-cd | source" >> /home/me/.config/fish/conf.d/fnm.fish

COPY entry.fish /entry.fish

# Set the default command.
CMD ["/usr/bin/fish"]

# Set the entry point script.
ENTRYPOINT ["/usr/bin/fish", "/entry.fish"]
