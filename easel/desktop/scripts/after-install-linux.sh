#!/bin/bash
set -e
# Root can create user namespaces on Ubuntu even when desktop users cannot.
# Install the Chromium helper with its standard root-owned sandbox permissions.
chown root:root '/opt/${sanitizedProductName}/chrome-sandbox'
chmod 4755 '/opt/${sanitizedProductName}/chrome-sandbox'
if command -v update-alternatives >/dev/null 2>&1; then
  update-alternatives --install '/usr/bin/${executable}' '${executable}' '/opt/${sanitizedProductName}/${executable}' 100
else
  ln -sf '/opt/${sanitizedProductName}/${executable}' '/usr/bin/${executable}'
fi
command -v update-mime-database >/dev/null 2>&1 && update-mime-database /usr/share/mime || true
command -v update-desktop-database >/dev/null 2>&1 && update-desktop-database /usr/share/applications || true
