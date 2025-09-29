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
    mkcert --cert-file localhost.pem --key-file localhost-key.pem localhost aesthetic.local sotce.local 127.0.0.1 0.0.0.0 $HOST_IP >/dev/null 2>&1
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
