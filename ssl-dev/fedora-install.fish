#!/usr/bin/env fish

# Function to display usage
function usage
    echo "Usage: $argv[0] [--install-only]"
    echo "  --install-only  Only install the SSL certificates, do not generate them."
end

# Check for the install-only flag
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

# Get the directory of the script
set SCRIPT_DIR (dirname (status --current-filename))

# Change to the script's directory
cd $SCRIPT_DIR

# Generate certificates if not in install-only mode
if test $INSTALL_ONLY -eq 0
    # generate certificates
    mkcert --cert-file localhost.pem --key-file localhost-key.pem localhost aesthetic.local sotce.local 127.0.0.1 0.0.0.0 $HOST_IP
    # combine cert and key into a single PEM file (if needed)
    cat localhost.pem localhost-key.pem > combined.pem
    # make a .crt for iOS and copy it to the public directory so it can be loaded on iOS
    openssl x509 -outform der -in combined.pem -out localhost.crt
    cp localhost.crt ../system/public/aesthetic.crt
    # let other potential systems sharing a volume know to install these
    rm .ssl
    touch .ssl
end

# Define the certificate file name
set CERT_FILE "localhost.pem"

# Check if the certificate file exists
if test -f $CERT_FILE
    # Copy the certificate to the trusted store
    sudo cp $CERT_FILE /etc/pki/ca-trust/source/anchors/

    # Update the CA trust store
    sudo update-ca-trust extract

    echo "Certificate has been added to the trusted store."
else
    echo "Certificate file not found: $CERT_FILE"
end

cd -
