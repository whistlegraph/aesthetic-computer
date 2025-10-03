
1. Install mkcert (for example with Homebrew: `brew install mkcert nss`).

2. `cd` into the `/ssl-dev` directory and run `mkcert --cert-file localhost.pem --key-file localhost-key.pem localhost aesthetic.local 127.0.0.1 0.0.0.0`.

3. Change the generated certificate names to `localhost.pem` and `localhost-key.pem` if mkcert adds suffixes.

4. In GitHub Codespaces the devcontainer startup script now checks for missing certificates and runs `ssl-install.fish` automatically, so manual steps are only needed when working outside the container. 
