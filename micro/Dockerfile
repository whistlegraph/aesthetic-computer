# Get the latest fedora version.
FROM fedora:latest
# Update all packages.
RUN dnf update -y

# Install git, gcc, and clang
RUN dnf install -y git gcc clang

# Install procps to get the 'ps' command (required for linux brew)
RUN dnf install -y procps

# Add 'me' user.
RUN useradd -m me
# Install fish shell
RUN dnf install -y fish
# And set it as default.
RUN dnf install -y util-linux-user
RUN chsh -s /usr/bin/fish me

# Set up homebrew permissions.
RUN mkdir -p /home/linuxbrew/.linuxbrew && \
    chown -R me:me /home/linuxbrew/.linuxbrew


# Switch to current user.
USER me

# Set working directory.
WORKDIR /home/me

# Copy the fish configuration.
RUN mkdir -p .config/fish
RUN mkdir -p .config/fish/conf.d
COPY config.fish .config/fish/config.fish 

# Install linux brew.
RUN /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Set environment variable for Homebrew
ENV PATH="/home/linuxbrew/.linuxbrew/bin:/home/linuxbrew/.linuxbrew/sbin:$PATH"

# Install web assembly tools.
RUN brew install wabt

# Install node version manager and a good LTS node version (18).
RUN brew install fnm
RUN echo "fnm env --use-on-cd | source" >> /home/me/.config/fish/conf.d/fnm.fish
RUN fnm install lts-hydrogen

# Set the default command.
CMD ["/usr/bin/fish"]
