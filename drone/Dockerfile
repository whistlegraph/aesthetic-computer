FROM fedora:latest

ENV DISPLAY=:99
ENV DEBIAN_FRONTEND=noninteractive

# Install necessary repos and packages
RUN dnf install -y \
  https://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm \
  https://download1.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm && \
  dnf install -y \
    chromium \
    ffmpeg \
    ffmpeg-libs \
    Xvfb \
    xorg-x11-server-Xvfb \
    pulseaudio \
    pulseaudio-utils \
    alsa-utils \
    unclutter \
    nginx && \
  dnf clean all

# Create HLS output directory
RUN mkdir -p /var/www/html/hls && chmod 777 /var/www/html/hls

# Copy files
COPY entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh

COPY nginx.conf /etc/nginx/nginx.conf
COPY player.html /var/www/html/index.html

EXPOSE 80

CMD ["/entrypoint.sh"]
