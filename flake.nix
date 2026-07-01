{
  description = "Hadyn's literate Emacs configuration (straight.el), a cross-platform Emacs derivation (emacs-plus patches on Darwin), and its language-server closure, packaged as a home-manager module.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    inputs@{
      self,
      nixpkgs,
      flake-utils,
      ...
    }:
    let
      supportedSystems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      # One Emacs for Linux and Darwin. On Darwin the NS build already provides
      # `ns-appearance` / `ns-transparent-titlebar`; this overlay adds the
      # emacs-plus `system-appearance` patch on top so `ns-system-appearance`
      # (and its change hook) exist, giving auto-dark an event-driven source.
      # The patch touches Cocoa (`nsterm.m`), so it is applied on Darwin only —
      # Linux builds a normal PGTK Emacs from the same derivation.
      emacs-appearance-overlay = final: prev: {
        emacs = prev.emacs.overrideAttrs (old: {
          patches =
            (old.patches or [ ])
            ++ prev.lib.optionals prev.stdenv.hostPlatform.isDarwin [
              (prev.fetchurl {
                # Pinned by content hash: if upstream rewrites the patch the
                # build fails loudly (mismatch) rather than changing silently.
                # For stricter reproducibility, pin the URL to a commit sha.
                url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-30/system-appearance.patch";
                sha256 = "sha256-nrPOgGQAJb/5brrrWJNDARY2jWNJ9OsMtO+LPVhHfbY=";
              })
            ];
        });
      };
    in
    flake-utils.lib.eachSystem supportedSystems (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ emacs-appearance-overlay ];
        };

        # External tools config.org shells out to. Keep in sync with the
        # eglot-server-programs / lsp-mode / executable-find references in
        # config.org (line numbers are a hint, not load-bearing):
        #   coreutils-prefixed            -> gls                                (config.org: dired setup)
        #   marksman                      -> Markdown LSP                        (eglot-server-programs)
        #   roslyn-ls                     -> Microsoft.CodeAnalysis.LanguageServer (eglot-server-programs)
        #   tailwindcss-language-server   -> lsp-tailwindcss
        #   vscode-langservers-extracted  -> vscode-eslint-language-server       (lsp-eslint)
        emacs-tools = with pkgs; [
          coreutils-prefixed
          marksman
          roslyn-ls
          tailwindcss-language-server
          vscode-langservers-extracted
        ];
      in
      {
        packages = {
          # The cross-platform Emacs (emacs-plus system-appearance patch on
          # Darwin). Wire into home-manager via `programs.dotemacs.package` or
          # `programs.emacs.package`.
          emacs = pkgs.emacs;

          # A single joined derivation of the tool closure, handy for `nix run`,
          # ad-hoc profiles, or reuse from another flake.
          emacs-tools = pkgs.buildEnv {
            name = "dotemacs-tools";
            paths = emacs-tools;
          };
        };

        devShells.default = pkgs.mkShell {
          name = "dotemacs-devShell";
          # emacs-nox: headless, for batch tangle/lint in the shell. Avoids a
          # GUI source build; the overlay only overrides `emacs`, not `emacs-nox`.
          packages = [
            pkgs.emacs-nox
            pkgs.gnumake
          ]
          ++ emacs-tools;
          shellHook = ''
            git config --local core.hooksPath .githooks 2>/dev/null || true
          '';
        };

        checks = {
          # Balanced parens in init.el, tangle config.org, byte-compile the
          # result. Run with `nix flake check`.
          smoke = pkgs.runCommand "dotemacs-smoke" { nativeBuildInputs = [ pkgs.emacs-nox ]; } ''
            cp -r ${self}/. work
            chmod -R u+w work
            cd work
            emacs --batch -Q --eval \
              '(with-temp-buffer (insert-file-contents "init.el") (emacs-lisp-mode) (check-parens))'
            emacs --batch -Q \
              --eval "(require 'org)" \
              --eval '(org-babel-tangle-file "config.org" "config.el")'
            emacs --batch -Q \
              --eval "(setq byte-compile-warnings '(not unresolved free-vars noruntime obsolete))" \
              -f batch-byte-compile config.el
            touch $out
          '';

          # The theme auto-switch must be cross-platform: it must go through
          # auto-dark (works on Linux + Darwin) and must NOT read the macOS-only
          # `ns-system-appearance` variable at top level (void-variable on Linux).
          appearance = pkgs.runCommand "dotemacs-appearance" { nativeBuildInputs = [ pkgs.emacs-nox ]; } ''
            cp -r ${self}/. work
            chmod -R u+w work
            cd work
            emacs --batch -Q \
              --eval "(require 'org)" \
              --eval '(org-babel-tangle-file "config.org" "config.el")'
            # Strip elisp comments (`;' to end-of-line) so documentation that
            # merely mentions these symbols does not trip the assertions.
            code="$(sed 's/;.*//' config.el)"
            if ! grep -q 'auto-dark' <<<"$code"; then
              echo "FAIL: config.el does not integrate auto-dark for theme switching" >&2
              exit 1
            fi
            if grep -q 'ns-system-appearance' <<<"$code"; then
              echo "FAIL: config.el still references the macOS-only ns-system-appearance in code" >&2
              grep -n 'ns-system-appearance' <<<"$code" >&2
              exit 1
            fi
            touch $out
          '';
        };

        formatter = pkgs.nixfmt-tree;
      }
    )
    // {
      # System-independent home-manager module. Import it and set
      # `programs.dotemacs.enable = true;` (see nix/hm-module.nix for options).
      homeManagerModules.default = import ./nix/hm-module.nix { inherit self; };

      # Adds the patched, cross-platform `emacs`. Apply it in a home-manager /
      # nix-darwin config's `nixpkgs.overlays`, then reference `pkgs.emacs`.
      overlays.default = emacs-appearance-overlay;
    };
}
