FROM debian:bullseye-slim

# Install necessary dependencies for Chromium and fonts
RUN apt-get update && apt-get install -y \
    chromium \
    fonts-ipafont-gothic fonts-wqy-zenhei fonts-thai-tlwg fonts-kacst fonts-freefont-ttf libxss1 \
    --no-install-recommends \
    && rm -rf /var/lib/apt/lists/*

# Create a non-root user to run Chromium
RUN groupadd -r pptruser && useradd -r -g pptruser -G audio,video pptruser \
    && mkdir -p /home/pptruser/Downloads \
    && chown -R pptruser:pptruser /home/pptruser

# Run as non-root user
USER pptruser

# Set working directory
WORKDIR /home/pptruser

# Expose the remote debugging port
EXPOSE 9222

# Command to run Chromium
# --no-sandbox and --disable-setuid-sandbox are often needed in containerized environments
# --disable-dev-shm-usage is recommended to prevent issues with /dev/shm size
# --user-data-dir is set to a writable temporary location
CMD ["chromium", \
     "--headless", \
     "--disable-gpu", \
     "--remote-debugging-port=9222", \
     "--remote-debugging-address=0.0.0.0", \
     "--no-sandbox", \
     "--disable-setuid-sandbox", \
     "--disable-dev-shm-usage", \
     "--user-data-dir=/tmp/chromium-user-data" \
    ]
