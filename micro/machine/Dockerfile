# Get the latest fedora version.
FROM fedora:latest
# Update all packages.
RUN dnf update -y

# Install git, gcc, clang, and emacs (without x support) and python and the github cli
RUN dnf install -y git gcc clang emacs-nox python3 python3-pip gh

# Install procps to get the 'ps' command (required for linux brew)
RUN dnf install -y procps

# Add 'me' user.
RUN useradd -m me && \
    # Optional: Set password for the user (replace 'password' with your desired password)
    # echo "me:micro" | chpasswd && \
    # Give 'me' sudo privileges without password
    echo "me ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers

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

# Set working directory to me user.
WORKDIR /home/me

# Install shell-gpt for the user.
RUN pip3 install shell-gpt

# Copy the fish configuration and entry point script.
RUN mkdir -p .config/fish
RUN mkdir -p .config/fish/conf.d
COPY config.fish .config/fish/config.fish 
COPY entry.fish /entry.fish

# Copy emacs config.
COPY init.el .emacs
RUN sudo chown me:me .emacs
# RUN mkdir -p .emacs.d

# Install linux brew.
RUN /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Set environment variable for Homebrew
ENV PATH="/home/linuxbrew/.linuxbrew/bin:/home/linuxbrew/.linuxbrew/sbin:$PATH"

# Install web assembly tools.
RUN brew install wabt

# Install node version manager and a good LTS node version (18).
RUN brew install fnm && \
    fnm install lts-hydrogen
RUN echo "fnm env --use-on-cd | source" >> /home/me/.config/fish/conf.d/fnm.fish

# Set the default command.
CMD ["/usr/bin/fish"]

# Login to Gitgub.
RUN gh auth login

# Set the entry point script.
ENTRYPOINT ["/usr/bin/fish", "/entry.fish"]
