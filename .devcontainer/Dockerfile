FROM fedora:latest
# Update all packages.
RUN dnf update -y

RUN dnf install -y which tree procps-ng coreutils unzip jq bat
RUN dnf install -y git gh gcc clang python3 python3-pip emacs-nox fish util-linux-user
RUN dnf install -y qemu-kvm qemu-img
RUN dnf install -y openssl openssl-devel awscli redis
RUN dnf install -y dnf-plugins-core
RUN dnf install -y lolcat toilet
# RUN dnf install -y entr
RUN dnf install -y iproute
RUN dnf install -y xclip 
# RUN dnf install -y golang

# Add the Caddy repository and install Caddy
RUN dnf install -y 'dnf-command(copr)'
RUN dnf copr enable -y @caddy/caddy
RUN dnf install -y caddy

# Install nginx
RUN dnf install -y nginx

# Install 'docker'
RUN sudo dnf config-manager --add-repo https://download.docker.com/linux/fedora/docker-ce.repo
RUN dnf install -y docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin

# Add 'me' user.
RUN useradd -m me && \
    # Optional: Set password for the user (replace 'password' with your desired password)
    # echo "me:micro" | chpasswd && \
    # Give 'me' sudo privileges without password
    echo "me ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers

RUN chsh -s /usr/bin/fish me

# Download and install the Stripe CLI
RUN curl -sL https://github.com/stripe/stripe-cli/releases/download/v1.19.4/stripe_1.19.4_linux_x86_64.tar.gz | tar xz -C /usr/local/bin

# Install the redli redis CLI
RUN curl -sL https://github.com/IBM-Cloud/redli/releases/download/v0.13.0/redli_0.13.0_linux_amd64.tar.gz | tar xz -C /usr/local/bin
RUN mv /usr/local/bin/redli_linux_amd64 /usr/local/bin/redli

# Install mkcert
RUN curl -L https://github.com/FiloSottile/mkcert/releases/download/v1.4.4/mkcert-v1.4.4-linux-amd64 -o /usr/local/bin/mkcert && \
    chmod +x /usr/local/bin/mkcert

# Install websocat
# TODO: Replace websocat copr install with installing the rust toolchain
#       and subsequently building from source;
# RUN dnf copr enable atim/websocat -y && sudo dnf install -y websocat

# Switch to current user.
USER me

# Install gyp
RUN sudo dnf install -y python3-devel && \
    git clone https://chromium.googlesource.com/external/gyp.git /home/me/.local/lib/python3.10/site-packages/gyp

# Set working directory to me user.
WORKDIR /home/me

RUN mkdir -p .config/fish && \
   mkdir -p .config/fish/conf.d

# 🐈 websocat
# Install Rust toolchain &
# Clone websocat repository to build from source.
RUN curl https://sh.rustup.rs -sSf | sh -s -- -y \
    && source $HOME/.cargo/env \
    && git clone https://github.com/vi/websocat.git \
    && cd websocat \
    && cargo build --release \
    && sudo cp target/release/websocat /usr/local/bin/

# Install fnm
RUN curl -fsSL https://fnm.vercel.app/install | bash -s -- --install-dir "/home/me/.fnm" --skip-shell
ENV PATH="/home/me/.fnm:$PATH"
RUN eval "$(fnm env --use-on-cd)" && \
    fnm install lts-iron && fnm use lts-iron && fnm install 20.5.0 && \
    npm install -g prettier typescript typescript-language-server npm-check-updates
    
# @emacs-eask/cli

# Install shell-gpt for the user.
RUN pip3 install shell-gpt

# Install aider: https://github.com/paul-gauthier/aider?tab=readme-ov-file#getting-started
# RUN pip3 install aider-chat

# Install OPS and Nanos: https://nanos.org/getting_started
RUN curl https://ops.city/get.sh -sSfL | sh

# Install deno
# RUN curl -fsSL https://deno.land/install.sh | sh

# Copy over a custom xdg-open for opening browser URLs on the host.
# COPY xdg-open.fish /usr/local/bin/xdg-open
# RUN sudo chmod +x /usr/local/bin/xdg-open

# Clone isomorphic copy.
# Must also be cloned in ~ of host machine and add isomorphic_copy/bin to PATH
# RUN git clone https://github.com/ms-jpq/isomorphic_copy

# Copy emacs config.
# COPY emacs.el .emacs
# RUN sudo chown me:me .emacs
# RUN mkdir -p .emacs.d

# Copy fish configuration.
COPY config.fish .config/fish/config.fish

# Copy entry file.
COPY entry.fish /entry.fish

# Set the default command.
CMD ["/usr/bin/fish"]

# Set the entry point script.
# ENTRYPOINT ["/usr/bin/fish", "/entry.fish"]
