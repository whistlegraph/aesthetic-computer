{ lib, stdenv, fetchurl, pkg-config
, libdrm, alsa-lib, flite, openssl, curl
, ffmpeg, esbuild
, nativeSrc
, kidlispSrc ? null
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

  src = nativeSrc;

  nativeBuildInputs = [
    pkg-config
    esbuild
  ];

  buildInputs = [
    libdrm
    alsa-lib
    flite
    openssl
    curl
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

  # DRM-direct mode — no Wayland compositor, no cage overhead.
  makeFlags = [
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

    mkdir -p $out/bin $out/share/ac-native/pieces $out/share/ac-native/jslib

    # Binary
    cp build/ac-native $out/bin/

    # Default piece (prompt.mjs → piece.mjs)
    cp pieces/prompt.mjs $out/share/ac-native/piece.mjs

    # All pieces
    cp pieces/*.mjs $out/share/ac-native/pieces/ 2>/dev/null || true
    cp pieces/*.lisp $out/share/ac-native/pieces/ 2>/dev/null || true

    # KidLisp bundle — ac-native loads /jslib/kidlisp-bundle.js at init.
    # kidlispSrc is the aesthetic.computer web tree; entry point is lib/kidlisp.mjs.
    if [ -n "${toString kidlispSrc}" ] && [ -f "${toString kidlispSrc}/lib/kidlisp.mjs" ]; then
      esbuild "${toString kidlispSrc}/lib/kidlisp.mjs" --bundle --format=iife \
        --global-name=KidLispModule --platform=node \
        --outfile=$out/share/ac-native/jslib/kidlisp-bundle.js
    fi

    runHook postInstall
  '';

  meta = with lib; {
    description = "AC Native OS — creative computing kiosk runtime";
    license = licenses.mit;
    platforms = [ "x86_64-linux" ];
    mainProgram = "ac-native";
  };
}
