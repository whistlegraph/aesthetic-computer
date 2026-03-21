FROM fedora:latest

ENV DISPLAY=:1
ENV XVFB_RESOLUTION=320x240x24
ENV LD_LIBRARY_PATH=/usr/local/lib
ENV DBUS_SESSION_BUS_ADDRESS=/dev/null

WORKDIR /app

# --- Base system and dependencies ---
RUN dnf install -y \
    git wget curl tar make gcc gcc-c++ \
    autoconf automake libtool cmake \
    nasm yasm \
    xorg-x11-server-Xvfb \
    chromium \
    pulseaudio pulseaudio-utils alsa-utils libXScrnSaver \
    pulseaudio-libs-devel \
    freetype-devel fontconfig-devel libass-devel \
    lame-devel opus-devel libvpx-devel \
    openssl-devel pcre-devel zlib-devel \
    libX11-devel libXfixes-devel \
    dbus \
    pkgconfig && \
    mkdir -p /run/dbus && \
    dnf clean all

# --- Build and install x264 from source ---
WORKDIR /tmp
RUN git clone https://code.videolan.org/videolan/x264.git && \
    cd x264 && \
    ./configure --prefix=/usr/local --enable-shared --disable-cli && \
    make -j$(nproc) && \
    make install

# --- Build and install FFmpeg with libx264 ---
WORKDIR /tmp
RUN git clone https://git.ffmpeg.org/ffmpeg.git ffmpeg && \
    cd ffmpeg && \
    PKG_CONFIG_PATH="/usr/local/lib/pkgconfig" ./configure \
        --prefix=/usr/local \
        --enable-gpl \
        --enable-libx264 \
        --enable-libfreetype \
        --enable-libass \
        --enable-libpulse \
        --enable-libopus \
        --enable-libmp3lame \
        --enable-libvpx \
        --enable-libfontconfig \
        --enable-shared \
        --disable-static && \
    make -j$(nproc) && \
    make install && \
    ldconfig

# --- Build and install nginx with RTMP module ---
WORKDIR /tmp
RUN curl -LO http://nginx.org/download/nginx-1.25.4.tar.gz && \
    tar -xzf nginx-1.25.4.tar.gz && \
    git clone https://github.com/arut/nginx-rtmp-module.git && \
    cd nginx-1.25.4 && \
    ./configure --add-module=../nginx-rtmp-module && \
    make && \
    make install

# --- Setup app ---
WORKDIR /app
COPY stream.sh .
COPY nginx.conf /usr/local/nginx/conf/nginx.conf
RUN chmod +x stream.sh

EXPOSE 1935
EXPOSE 8080
EXPOSE 9222

CMD ["./stream.sh"]

