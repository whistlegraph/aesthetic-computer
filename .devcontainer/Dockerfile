FROM fedora:latest

ENV NODE_OPTIONS=--max_old_space_size=8192
ENV DENO_DIR=/home/me/.cache/deno
ENV NETLIFY_DENO_VERSION=1.45.0

# Update all packages.
RUN dnf update -y
RUN dnf install -y awk which tree procps-ng coreutils unzip jq bat
RUN dnf install -y git gh gcc clang python3 python3-pip emacs-nox fish util-linux-user
RUN dnf install -y make cmake gcc-c++ python3-devel
RUN dnf install -y qemu-kvm qemu-img qemu-system-x86 qemu-user-static
RUN dnf install -y openssl openssl-devel awscli redis
RUN dnf install -y dnf-plugins-core
RUN dnf install -y lolcat toilet
RUN dnf install -y iproute
RUN dnf install -y xclip
RUN dnf install -y nmap-ncat
# RUN dnf install -y inotify-tools

# Install PowerShell
# Use Fedora 40 repo instead of Fedora 42
# RUN dnf install -y https://packages.microsoft.com/config/fedora/40/packages-microsoft-prod.rpm \


#  && dnf install -y powershell

RUN ulimit -n 65536

# Add the Caddy repository and install Caddy
RUN dnf install -y 'dnf-command(copr)'
RUN dnf copr enable -y @caddy/caddy
RUN dnf install -y caddy
# RUN dnf install -y dotnet-sdk-9.0

# Install necessary dependencies for Puppeteer
RUN dnf install -y \
    libX11 \
    libxkbfile \
    libsecret \
    libxshmfence \
    libXtst \
    nss \
    atk \
    pango \
    cups-libs \
    libXcomposite \
    libXcursor \
    libXi \
    libXdamage \
    libXrandr \
    alsa-lib \
    gtk3 \
    xorg-x11-server-Xvfb \
    psmisc \
    lsof \
    iproute

# Install Chromium browser explicitly
RUN dnf install -y chromium

# Install keyring
# RUN dnf install -y \
#    libsecret \
#    dbus

# Initialize keyring
# RUN mkdir -p /root/.local/share/keyrings

# Install nginx
# RUN dnf install -y nginx

# Install \'docker\'
# RUN sudo dnf config-manager --add-repo https://download.docker.com/linux/fedora/docker-ce.repo
# RUN dnf install -y docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin

# Add \'me\' user.
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

# Download and install ngrok from the specified URL
RUN curl -s https://bin.equinox.io/c/bNyj1mQVY4c/ngrok-v3-stable-linux-amd64.tgz -o ngrok.tgz && \
    tar -xzf ngrok.tgz && \
    mv ngrok /usr/local/bin/ && \
    rm ngrok.tgz

# Install Python Jupyter dependencies as root
RUN dnf install -y python3 python3-pip python3-notebook python3-nbconvert

# TODO: Install rosrell and common lisp notebook extension.

# Install gcloud CLI using the tarball (compatible with Fedora and Python 3.12)
RUN curl -sSL https://sdk.cloud.google.com > install_gcloud.sh && \
    bash install_gcloud.sh --disable-prompts --install-dir=/opt && \
    ln -s /opt/google-cloud-sdk/bin/gcloud /usr/local/bin/gcloud && \
    ln -s /opt/google-cloud-sdk/bin/gsutil /usr/local/bin/gsutil && \
    ln -s /opt/google-cloud-sdk/bin/bq /usr/local/bin/bq && \
    rm install_gcloud.sh


# Switch to the 'me' user.
USER me

# RUN dotnet tool install --global Microsoft.dotnet-interactive && \
#     mkdir -p /home/me/.config/fish && \
#     echo 'set -gx PATH $HOME/.dotnet/tools $PATH' >> /home/me/.config/fish/config.fish && \
#     mkdir -p /home/me/.local/share/jupyter/kernels && \
#     PATH="/home/me/.dotnet/tools:$PATH" dotnet interactive jupyter install --path /home/me/.local/share/jupyter/kernels

RUN sudo mkdir -p $HOME/.jupyter && \
    sudo chmod -R u+rw $HOME/.jupyter

# Install gyp
RUN sudo dnf install -y python3-devel && \
    git clone https://chromium.googlesource.com/external/gyp.git /home/me/.local/lib/python3.10/site-packages/gyp

# Set working directory to me user.
WORKDIR /home/me

RUN mkdir -p .config/fish && \
   mkdir -p .config/fish/conf.d

# 🐈 websocat
# Install Rust toolchain & 
# Clone websocat repository and build a specific release version
RUN curl https://sh.rustup.rs -sSf | sh -s -- -y \
    && . $HOME/.cargo/env \
    && git clone https://github.com/vi/websocat.git \
    && cd websocat \
    && cargo build --release \
    && sudo cp target/release/websocat /usr/local/bin/

# RUN curl -L https://github.com/vi/websocat/releases/download/v1.11.0/websocat_amd64-linux -o /usr/local/bin/websocat \
#     && chmod +x /usr/local/bin/websocat
# RUN curl -L https://github.com/vi/websocat/releases/download/v1.11.0/websocat_amd64-linux -o /tmp/websocat \
#     && chmod +x /tmp/websocat \
#     && sudo mv /tmp/websocat /usr/local/bin/websocat

# Install fnm and base javascript tooling.
RUN curl -fsSL https://fnm.vercel.app/install | bash -s -- --install-dir "/home/me/.fnm" --skip-shell
ENV PATH="/home/me/.fnm:$PATH"

# Set up fnm for Fish shell
RUN mkdir -p /home/me/.config/fish/conf.d && \
    echo 'fnm env --use-on-cd --shell fish | source' > /home/me/.config/fish/conf.d/fnm.fish

# Install Node.js versions and global packages
# Use a single RUN command with proper fnm environment setup
RUN eval "$(/home/me/.fnm/fnm env --shell bash)" && \
    /home/me/.fnm/fnm install 20.5.0 && \
    /home/me/.fnm/fnm install lts-jod && \
    /home/me/.fnm/fnm default lts-jod && \
    /home/me/.fnm/fnm use lts-jod && \
    npm install -g prettier typescript typescript-language-server npm-check-updates && \
    npm install -g @anthropic-ai/claude-code && \
    npm install -g @openai/codex && \
    npm install -g netlify-cli

# Ensure the default Node.js version is available in Fish shell sessions
RUN echo 'fnm use lts-jod' >> /home/me/.config/fish/config.fish

RUN sudo dnf install -y sbcl zeromq-devel redhat-rpm-config

# Install Quicklisp
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp && \
    sbcl --non-interactive \
         --load quicklisp.lisp \
         --eval '(quicklisp-quickstart:install)' \
         --eval '(ql-util:without-prompting (ql:add-to-init-file))' \
         --eval '(quit)'

# TODO: Eventually use SBCL for something / add it to jupyter kernel?

# # Clone the common-lisp-jupyter repository
# RUN git clone https://github.com/yitzchak/common-lisp-jupyter.git /home/me/common-lisp-jupyter
# 
# # Load and install common-lisp-jupyter
# RUN sbcl --non-interactive \
#     --eval '(load "/home/me/quicklisp/setup.lisp")' \
#     --eval '(pushnew #p"/home/me/common-lisp-jupyter/" ql:*local-project-directories*)' \
#     --eval '(ql:quickload :common-lisp-jupyter)' \
#     --eval '(cl-jupyter:install)' \
#     --eval '(quit)'

# @emacs-eask/cli

# Install shell-gpt for the user.
# RUN pip3 install shell-gpt

# Install aider: https://github.com/paul-gauthier/aider?tab=readme-ov-file#getting-started
# RUN pip3 install aider-chat

# Install OPS and Nanos: https://nanos.org/getting_started
# QEMU components are now installed earlier as root
# Install OPS - the installer checks for qemu-system-x86_64 specifically
RUN curl https://ops.city/get.sh -sSfL | bash

# Install deno
# RUN curl -fsSL https://deno.land/install.sh | sh

# Pre-cache Edge Functions bootstrap files to speed up netlify dev
# RUN eval "$(/home/me/.deno/bin/deno --version)" && \
#     /home/me/.deno/bin/deno cache https://v2-13-0--edge.netlify.com/bootstrap/index-combined.ts 2>/dev/null || echo "Bootstrap cache attempt completed" && \
#     /home/me/.deno/bin/deno cache https://v2-13-0--edge.netlify.com/bootstrap/handler.ts 2>/dev/null || echo "Handler cache attempt completed" && \
#     /home/me/.deno/bin/deno cache https://v2-13-0--edge.netlify.com/bootstrap/server.ts 2>/dev/null || echo "Server cache attempt completed"

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

# Add the notebook directory to the Python path and preload the aesthetic module
RUN mkdir -p /home/me/.ipython/profile_default/startup && \
    echo "import sys; sys.path.append('/workspaces/aesthetic-computer/notebook'); import aesthetic" > /home/me/.ipython/profile_default/startup/00-aesthetic.py && \
    sudo chown -R me:me /home/me/.ipython

# Install watchdog for monitoring file changes
# RUN pip install watchdog

# Update CMD to only run the Jupyter Notebook server (is this necessary?)
# CMD ["jupyter-notebook", "--allow-root"]

# Set the entry point script.
# ENTRYPOINT ["/usr/bin/fish", "/entry.fish"]
