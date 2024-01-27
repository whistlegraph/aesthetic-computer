#!/usr/bin/fish

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
