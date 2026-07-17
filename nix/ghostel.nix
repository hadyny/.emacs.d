# Vendored copy of nixpkgs' pkgs/applications/editors/emacs/elisp-packages/
# manual-packages/ghostel/package.nix, pinned to the v0.41.0 release tag.
#
# Why: the nixpkgs snapshot `0.41.0-unstable-2026-07-06` (commit eb806d1) ships
# a native module whose registered arity disagrees with the elisp that calls it,
# so the redraw/resize path signals `wrong-number-of-arguments ... 3' and the
# terminal never opens. The v0.41.0 tag is the last point the module and elisp
# were released and tested together (its module.version sidecar and
# `ghostel--minimum-module-version' are both exactly 0.41.0). See
# tests/ghostel-test.el `ghostel/native-module-arity-contract' for the guard.
#
# Only three values differ from the nixpkgs recipe: `version', `src.rev'/`hash',
# and the Zig `deps.hash' (v0.41.0's build.zig.zon differs from the snapshot).
{
  lib,
  fetchFromGitHub,
  melpaBuild,
  nix-update-script,
  stdenv,
  zig_0_15,
  emacs,
  xcbuild,
}:

let
  zig = zig_0_15;

  pname = "ghostel";

  version = "0.41.0";

  src = fetchFromGitHub {
    owner = "dakra";
    repo = "ghostel";
    rev = "b8a302bcf48424d5f5965ab84fbbd958616f30db"; # refs/tags/v0.41.0
    hash = "sha256-07Ml/eRIHyptI6MNlHhDNsfIiXOAUVKaEalDVJ101WM=";
  };

  module = stdenv.mkDerivation (finalAttrs: {
    inherit pname version src;

    deps = zig.fetchDeps {
      inherit (finalAttrs) src pname version;
      fetchAll = true;
      hash = "sha256-lFU0ywNyP1q2NL9MkIfWciH03VAA/Act5dGYAV4V7EY=";
    };

    nativeBuildInputs = [ zig ] ++ lib.optionals stdenv.hostPlatform.isDarwin [ xcbuild ];

    env.EMACS_INCLUDE_DIR = "${emacs}/include";

    dontSetZigDefaultFlags = true;

    doCheck = true;

    zigCheckFlags = [
      "-Dcpu=baseline"
      # See https://github.com/ghostty-org/ghostty/blob/main/PACKAGING.md#build-options
      "-Doptimize=ReleaseFast"
    ];

    zigBuildFlags = finalAttrs.zigCheckFlags;

    postConfigure = ''
      cp -rLT ${finalAttrs.deps} "$ZIG_GLOBAL_CACHE_DIR/p"
      chmod -R u+w "$ZIG_GLOBAL_CACHE_DIR/p"
    '';
  });

  libExt = stdenv.hostPlatform.extensions.sharedLibrary;
in
melpaBuild {
  inherit pname version src;

  files = ''
    (:defaults "etc" "ghostel-module${libExt}" "ghostel-module.version")
  '';

  preBuild = ''
    install ${module}/ghostel-module${libExt} ghostel-module${libExt}
    install --mode=444 ${module}/ghostel-module.version ghostel-module.version
  '';

  passthru = {
    updateScript = nix-update-script { extraArgs = [ "--version=branch=main" ]; };

    inherit module;
  };

  meta = {
    homepage = "https://github.com/dakra/ghostel";
    description = "Terminal emulator powered by libghostty";
    maintainers = with lib.maintainers; [
      rohan-datar
      vonfry
    ];
    license = lib.licenses.gpl3Plus;
  };
}
