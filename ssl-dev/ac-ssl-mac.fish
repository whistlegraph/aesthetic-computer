#!/usr/bin/env fish

# Aesthetic Computer SSL setup (macOS + Fedora)
# Place this at ~/bin/ac-ssl and make it executable:
#   chmod +x ~/bin/ac-ssl
#
# sudoers example:
# jas ALL=(ALL) NOPASSWD: /usr/bin/security add-trusted-cert -d -r trustRoot -k /Library/Keychains/System.keychain *
# jas ALL=(ALL) NOPASSWD: /Users/jas/bin/ac-ssl

# Parse --install-only flag
set INSTALL_ONLY 0
for arg in $argv
    switch $arg
        case --install-only
            set INSTALL_ONLY 1
        case "*"
            echo "Usage: ac-ssl [--install-only]"
            exit 1
    end
end

# Detect OS
set OS (uname)

# Script context
set SCRIPT_DIR (dirname (status --current-filename))
cd $SCRIPT_DIR

# Go to ssl-dev directory (assumes standard layout)
cd ~/Desktop/code/aesthetic-computer/ssl-dev

# TODO: Could prevent generation if the cert already exists. 
# Generate certificates if not install-only
if test $INSTALL_ONLY -eq 0
    mkcert --cert-file localhost.pem --key-file localhost-key.pem localhost aesthetic.local sotce.local 127.0.0.1 0.0.0.0 $HOST_IP > /dev/null 2>&1
    cat localhost.pem localhost-key.pem > combined.pem
    openssl x509 -outform der -in combined.pem -out localhost.crt
    cp localhost.crt ../system/public/aesthetic.crt
end

# Install the cert if needed
set CERT_FILE "localhost.pem"
if test -f $CERT_FILE
    switch $OS
        case Linux
            sudo cp $CERT_FILE /etc/pki/ca-trust/source/anchors/
            sudo update-ca-trust extract
        case Darwin
            # Check if cert is already trusted (by SHA256 fingerprint)
            set CERT_SHA256 (openssl x509 -noout -fingerprint -sha256 -in $CERT_FILE | string replace "SHA256 Fingerprint=" "" | string replace ":" "")
            set FOUND (security find-certificate -a -Z /Library/Keychains/System.keychain | grep -ci $CERT_SHA256)

            if test $FOUND -gt 0
                echo "✅ Certificate already trusted. Skipping install."
            else
                echo "🔐 Installing certificate to System keychain..."
                /usr/bin/security add-trusted-cert -d -r trustRoot -k /Library/Keychains/System.keychain $CERT_FILE
            end
        case '*'
            echo "Unsupported OS: $OS"
            exit 1
    end
else
    echo "🔴 Certificate not found: $CERT_FILE"
end

cd -
