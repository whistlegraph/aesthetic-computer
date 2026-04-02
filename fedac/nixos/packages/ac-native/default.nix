{ lib, stdenv, fetchurl, pkg-config
, libdrm, alsa-lib, flite, openssl, curl
, wayland, wayland-protocols, wayland-scanner
, ffmpeg
, gitHash ? "unknown", version ? "dev"
}:

let
  quickjs-src = fetchurl {
    url = "https://bellard.org/quickjs/quickjs-2024-01-13.tar.xz";
    sha256 = "sha256-PEv4+JW/pUvrSGyNEhgRJ3Hs/FrDvhA2hR70FWghLgM=";
  };
in
stdenv.mkDerivation {
  pname = "ac-native";
  inherit version;

  src = ../../../native;

  nativeBuildInputs = [
    pkg-config
    wayland-scanner
  ];

  buildInputs = [
    libdrm
    alsa-lib
    flite
    openssl
    curl
    wayland
    wayland-protocols
    ffmpeg
  ];

  postUnpack = ''
    # Unpack QuickJS into build/quickjs/
    mkdir -p $sourceRoot/build/quickjs
    tar xf ${quickjs-src} -C $sourceRoot/build/
    cp $sourceRoot/build/quickjs-2024-01-13/*.c \
       $sourceRoot/build/quickjs-2024-01-13/*.h \
       $sourceRoot/build/quickjs/
  '';

  # Fix hardcoded wayland protocol path
  postPatch = ''
    substituteInPlace Makefile \
      --replace '/usr/share/wayland-protocols' \
                '${wayland-protocols}/share/wayland-protocols'
  '';

  makeFlags = [
    "USE_WAYLAND=1"
    "CC=cc"
  ];

  # Inject build metadata without git
  NIX_CFLAGS_COMPILE = toString [
    "-DAC_GIT_HASH=\"${gitHash}\""
    "-DAC_BUILD_NAME=\"nix-${version}\""
    "-DAC_BUILD_TS=\"${version}\""
  ];

  # Skip the quickjs fetch target (we already unpacked it)
  preBuild = ''
    touch build/quickjs/quickjs.h
  '';

  enableParallelBuilding = true;

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin $out/share/ac-native/pieces

    # Binary
    cp build/ac-native $out/bin/

    # Default piece (prompt.mjs → piece.mjs)
    cp pieces/prompt.mjs $out/share/ac-native/piece.mjs

    # All pieces
    cp pieces/*.mjs $out/share/ac-native/pieces/ 2>/dev/null || true
    cp pieces/*.lisp $out/share/ac-native/pieces/ 2>/dev/null || true

    runHook postInstall
  '';

  meta = with lib; {
    description = "AC Native OS — creative computing kiosk runtime";
    license = licenses.mit;
    platforms = [ "x86_64-linux" ];
    mainProgram = "ac-native";
  };
}
