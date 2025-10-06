#!/usr/bin/env fish

function usage
    echo "Usage: ssl-install.fish [--install-only]"
    echo "  --install-only  Only install the SSL certificates, do not generate them."
end

# Parse arguments
set INSTALL_ONLY 0
for arg in $argv
    switch $arg
        case --install-only
            set INSTALL_ONLY 1
        case "*"
            usage
            exit 1
    end
end

# Determine OS
set OS (uname)

# Get the script directory
set SCRIPT_DIR (dirname (status --current-filename))
cd $SCRIPT_DIR

echo "🔧 Running ssl-install.fish (install only: $INSTALL_ONLY)"

# Generate certs if not install-only
if test $INSTALL_ONLY -eq 0
    # Build list of domains/IPs to include in certificate
    set -l domains localhost aesthetic.local sotce.local 127.0.0.1 0.0.0.0
    
    # Add HOST_IP if set
    if test -n "$HOST_IP"
        set -a domains $HOST_IP
    end
    
    # Add GitHub Codespace URLs if in Codespaces
    if test -n "$CODESPACE_NAME"; and test -n "$GITHUB_CODESPACES_PORT_FORWARDING_DOMAIN"
        # Add wildcard for all codespace ports
        set -a domains "*.$GITHUB_CODESPACES_PORT_FORWARDING_DOMAIN"
        # Add specific URLs for common ports
        set -a domains "$CODESPACE_NAME-8888.$GITHUB_CODESPACES_PORT_FORWARDING_DOMAIN"
        set -a domains "$CODESPACE_NAME-8111.$GITHUB_CODESPACES_PORT_FORWARDING_DOMAIN"
        set -a domains "$CODESPACE_NAME-8889.$GITHUB_CODESPACES_PORT_FORWARDING_DOMAIN"
        echo "🌐 Adding GitHub Codespace domains to certificate"
    end
    
    mkcert --cert-file localhost.pem --key-file localhost-key.pem $domains >/dev/null 2>&1
    command cat localhost.pem localhost-key.pem >combined.pem
    openssl x509 -outform der -in combined.pem -out localhost.crt
    cp localhost.crt ../system/public/aesthetic.crt
end

# Install cert
set CERT_FILE "localhost.pem"
if test -f $CERT_FILE
    switch $OS
        case Linux
            sudo cp $CERT_FILE /etc/pki/ca-trust/source/anchors/
            sudo update-ca-trust extract
        case Darwin
            sudo security add-trusted-cert -d -r trustRoot -k /Library/Keychains/System.keychain $CERT_FILE
        case '*'
            echo "Unsupported OS: $OS"
            exit 1
    end
else
    echo "🔴 Certificate not found: $CERT_FILE"
end

cd -
