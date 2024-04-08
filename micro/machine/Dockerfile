# Get the latest fedora version.
FROM fedora:latest
# Update all packages.
RUN dnf update -y

RUN dnf install -y which tree procps-ng coreutils xclip xsel unzip jq bat
RUN dnf install -y git gh gcc clang python3 python3-pip emacs-nox fish util-linux-user
RUN dnf install -y qemu-kvm qemu-img
RUN dnf install -y openssl awscli redis
RUN dnf install -y dnf-plugins-core

# Add 'me' user.
RUN useradd -m me && \
    # Optional: Set password for the user (replace 'password' with your desired password)
    # echo "me:micro" | chpasswd && \
    # Give 'me' sudo privileges without password
    echo "me ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers

RUN chsh -s /usr/bin/fish me

# Download and install the Stripe CLI
RUN curl -sL https://github.com/stripe/stripe-cli/releases/download/v1.19.4/stripe_1.19.4_linux_x86_64.tar.gz | tar xz -C /usr/local/bin

# Install mkcert
RUN curl -L https://github.com/FiloSottile/mkcert/releases/download/v1.4.4/mkcert-v1.4.4-linux-amd64 -o /usr/local/bin/mkcert && \
    chmod +x /usr/local/bin/mkcert

# Install websocat
RUN dnf copr enable atim/websocat -y && sudo dnf install -y websocat

# Set up homebrew permissions.
# RUN mkdir -p /home/linuxbrew/.linuxbrew && \
#    chown -R me:me /home/linuxbrew/.linuxbrew

# Switch to current user.
USER me

# Set working directory to me user.
WORKDIR /home/me

# Copy the fish configuration and entry point script.
RUN mkdir -p .config/fish && \
    mkdir -p .config/fish/conf.d

# TODO: Bring fnm back here! ❤️‍🔥
# Install fnm
# RUN curl -fsSL https://fnm.vercel.app/install | bash -s -- --install-dir "/home/me/.fnm" --skip-shell \
#      set -gx PATH $HOME/.fnm $PATH \
#      fnm env | source \
#      fnm install lts-hydrogen \
#      fnm use lts-hydrogen \
#      npm install -g prettier typescript typescript-language-server

# Install fnm
RUN curl -fsSL https://fnm.vercel.app/install | bash -s -- --install-dir "/home/me/.fnm" --skip-shell
ENV PATH="/home/me/.fnm:$PATH"
RUN eval "$(fnm env --use-on-cd)" && \
    fnm install lts-hydrogen && fnm use lts-hydrogen && \
    npm install -g prettier typescript typescript-language-server

# Install shell-gpt for the user.
RUN pip3 install shell-gpt

# Install OPS and Nanos: https://nanos.org/getting_started
RUN curl https://ops.city/get.sh -sSfL | sh

# Install linux brew.
# RUN /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Set environment variable for Homebrew
# ENV PATH="/home/linuxbrew/.linuxbrew/bin:/home/linuxbrew/.linuxbrew/sbin:$PATH"

# Install web assembly tools.
# RUN brew install wabt

# Install local ssl certificate generator.
# RUN brew install mkcert

# Install local websocat cli testing tool.
# RUN brew install websocat

# Install cloudflared management cli.
# RUN brew install cloudflared

# Install roswell lisp manager
# RUN brew install roswell

# Install tree-sitter, for emacs parsing.
# RUN brew install tree-sitter

# Install fnm
# RUN fnm install lts-hydrogen && \
#    fnm use lts-hydrogen
# Install global npm packages
# RUN npm install -g prettier typescript typescript-language-server

# Add fnm configuration to Fish shell startup script

# Install fnm, configure it, install Node.js, and global npm packages
# RUN brew install fnm && \
#     eval "$(fnm env)" && \
#     fnm install lts-hydrogen && \
#     fnm use lts-hydrogen && \
#     npm install -g prettier typescript typescript-language-server

# Copy over a custom xdg-open for opening browser URLs on the host.
COPY xdg-open.fish /usr/local/bin/xdg-open
RUN sudo chmod +x /usr/local/bin/xdg-open

# Clone isomorphic copy.
# Must also be cloned in ~ of host machine and add isomorphic_copy/bin to PATH
RUN git clone https://github.com/ms-jpq/isomorphic_copy

# Copy emacs config.
COPY emacs.el .emacs
RUN sudo chown me:me .emacs
RUN mkdir -p .emacs.d

COPY config.fish .config/fish/config.fish

COPY entry.fish /entry.fish

# Final initialization.
# RUN fish /entry.fish

# Set the default command.
CMD ["/usr/bin/fish"]

# Set the entry point script.
ENTRYPOINT ["/usr/bin/fish", "/entry.fish"]
