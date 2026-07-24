# Home-manager module for Hadyn's Emacs config.
#
# Consumed from a flake like:
#
#   inputs.dotemacs.url = "github:hadyny/.emacs.d";
#   ...
#   # Optional: apply the overlay so `pkgs.emacs` is the cross-platform build
#   # (emacs-plus system-appearance patch on Darwin), then set `package` below.
#   nixpkgs.overlays = [ inputs.dotemacs.overlays.default ];
#
#   home-manager modules = [ inputs.dotemacs.homeModules.default ];
#   programs.dotemacs = {
#     enable = true;
#     configPath = "/Users/hadyn/src/dotemacs.d";  # live, writable checkout
#     tools = [ ];                                  # shared packages.nix already installs them
#     # package = pkgs.emacs;                       # opt into the nix Emacs
#   };
#
# This module installs the language-server closure and places the config. Emacs
# itself is opt-in via `package` (default null): leave it null to keep an
# externally-managed Emacs (Homebrew emacs-plus on macOS), or set it to the
# flake's `pkgs.emacs` to have nix provide Emacs on Linux and Darwin. Either
# way, ELisp packages are Nix-managed (built into the Emacs package set and
# loaded via package.el); use-package installs nothing.
{ self, emacsToolsFor }:
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.programs.dotemacs;
in
{
  options.programs.dotemacs = {
    enable = lib.mkEnableOption "Hadyn's Emacs config and its language-server closure";

    tools = lib.mkOption {
      type = lib.types.listOf lib.types.package;
      default = emacsToolsFor pkgs;
      description = ''
        External tools / language servers config.org expects on PATH. Set to
        `[ ]` when another module already installs them (e.g. a shared package
        list) so the closure is not restated.
      '';
    };

    package = lib.mkOption {
      type = lib.types.nullOr lib.types.package;
      default = null;
      description = ''
        Emacs package to install via home-manager. Leave `null` to keep an
        externally-managed Emacs (Homebrew emacs-plus on macOS). Set it to
        `pkgs.emacs` (with `overlays.default` applied) for the flake's
        cross-platform build — emacs-plus system-appearance patch on Darwin,
        plain PGTK on Linux.
      '';
    };

    configPath = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      example = "/Users/hadyn/src/dotemacs.d";
      description = ''
        Absolute path to a live checkout of this repo. When set, `~/.emacs.d`
        is an out-of-store symlink to it, so edits to config.org take effect
        without a rebuild.

        When `null`, the flake's own read-only source is linked instead. NOTE:
        init.el tangles config.org to config.el inside `~/.emacs.d` at startup,
        which a read-only store path cannot accept; use `null` only if config.el
        is pre-tangled, otherwise point this at a writable checkout.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = cfg.tools ++ lib.optional (cfg.package != null) cfg.package;

    home.file.".emacs.d".source =
      if cfg.configPath != null then config.lib.file.mkOutOfStoreSymlink cfg.configPath else self;
  };
}
