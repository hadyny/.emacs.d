{
  description = "Hadyn's literate Emacs configuration (package.el with a Nix-managed package set), a cross-platform Emacs derivation (emacs-plus patches on Darwin), and its language-server closure, packaged as a home-manager module.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    devenv.url = "github:cachix/devenv";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      devenv,
      ...
    }@inputs:
    let
      supportedSystems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      # External tools / language servers config.org expects on PATH. Defined
      # once as a function of pkgs and shared by both the `emacs-tools` package
      # output below and the home-manager module default (nix/hm-module.nix), so
      # the two can't drift.
      emacsToolsFor =
        pkgs: with pkgs; [
          coreutils-prefixed
          marksman
          roslyn-ls
        ];

      # One Emacs for Linux and Darwin. On Darwin the NS build already provides
      # `ns-appearance` / `ns-transparent-titlebar`; this overlay adds the
      # emacs-plus `system-appearance` patch on top so `ns-system-appearance`
      # (and its change hook) exist, giving auto-dark an event-driven source.
      # The patch touches Cocoa (`nsterm.m`), so it is applied on Darwin only —
      # Linux builds a normal PGTK Emacs from the same derivation.
      emacs-appearance-overlay =
        final: prev:
        let
          patchedEmacs = prev.emacs.overrideAttrs (old: {
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

          # Pin evil-ghostel to 20260712.716 (dakra/ghostel commit 0d41dfb).
          #
          # nixpkgs' MELPA snapshot moved evil-ghostel to 20260713, which
          # requires ghostel 0.42.1: its `:around' advice on `ghostel--redraw'
          # forwards a third `force-sync' arg. The ghostel in nixpkgs is still
          # 0.41.0-unstable-2026-07-06, whose native `ghostel--redraw' has arity
          # (1 . 2) -- so every redraw/resize/window-change signals
          # `wrong-number-of-arguments #<module function ...> 3' and the terminal
          # never opens. 20260712.716 forwards only (term full), matching the
          # installed ghostel. Revisit once nixpkgs ships ghostel >= 0.42.1.
          epkgs = (final.emacsPackagesFor patchedEmacs).overrideScope (
            _efinal: eprev: {
              evil-ghostel = eprev.evil-ghostel.overrideAttrs (_old: rec {
                version = "20260712.716";
                melpaVersion = version;
                commit = "0d41dfbbcd0577e7c7969f08703026f299d2eb71";
                src = final.fetchFromGitHub {
                  owner = "dakra";
                  repo = "ghostel";
                  rev = commit;
                  hash = "sha256-aYV8VYFrRrkhu2ciJHe6uN318OW+Tjav8nv43bIpAxo=";
                };
              });
            }
          );

          # The package list config.org needs on load-path (managed by Nix,
          # not straight.el). Keep this in sync with the (use-package ...) forms
          # in config.org; built-ins are intentionally absent.
          #
          # ghostel + its Evil integration evil-ghostel (which depends on
          # ghostel) build a from-source ghostty/Zig terminal whose fixed-output
          # dependency fetch is network-flaky in CI. They are gated behind
          # `withGhostel` so the CI check builds below can exclude them; the real
          # emacs-dotemacs (what home-manager installs) keeps them.
          dotemacsPackageList =
            withGhostel: e:
            with e;
            [
              apheleia
              auto-dark
              cape
              catppuccin-theme
              consult
              corfu
              corfu-prescient
              diff-hl
              doom-themes
              eldoc-box
              embark
              embark-consult
              evil
              evil-collection
              exec-path-from-shell
              flymake-eslint
              gcmh
              helpful
              ligature
              magit
              marginalia
              markdown-mode
              mood-line
              neotree
              nerd-icons
              nerd-icons-corfu
              nix-mode
              orderless
              org-modern
              prescient
              smartparens
              treesit-auto
              vertico
              vertico-prescient
              vterm
              which-key
            ]
            ++ final.lib.optionals withGhostel [
              ghostel
              evil-ghostel
            ];
        in
        {
          # Raw Emacs with only the Darwin appearance patch (no ELisp packages).
          emacs = patchedEmacs;

          # The Emacs actually used: patched + every config.org package
          # (ghostel included). Wire into home-manager via
          # `programs.dotemacs.package`.
          emacs-dotemacs = epkgs.withPackages (dotemacsPackageList true);

          # CI-only variant without ghostel/evil-ghostel, so `nix flake check`
          # never triggers the network-flaky ghostty/Zig build. Used by the
          # integration-tests and packages-loadable checks below.
          emacs-dotemacs-ci = epkgs.withPackages (dotemacsPackageList false);
        };
    in
    flake-utils.lib.eachSystem supportedSystems (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ emacs-appearance-overlay ];
        };

        # External tools config.org shells out to (shared with the home-manager
        # module default via emacsToolsFor). Keep the list in sync with the
        # eglot-server-programs / executable-find references in config.org:
        #   coreutils-prefixed            -> gls                                (config.org: dired setup)
        #   marksman                      -> Markdown LSP                        (eglot-server-programs)
        #   roslyn-ls                     -> Microsoft.CodeAnalysis.LanguageServer (eglot-server-programs)
        # TypeScript/TSX uses eglot with the project-local typescript-language-server
        # (resolved via my/add-node-modules-path), and ESLint runs through
        # flymake-eslint against the project-local eslint -- neither is a Nix tool.
        emacs-tools = emacsToolsFor pkgs;
      in
      {
        packages = {
          # The cross-platform Emacs with all config.org packages on load-path
          # (emacs-plus system-appearance patch on Darwin). Wire into
          # home-manager via `programs.dotemacs.package`.
          emacs = pkgs.emacs-dotemacs;

          # Raw Emacs (appearance patch only, no ELisp packages).
          emacs-bare = pkgs.emacs;

          # A single joined derivation of the tool closure, handy for `nix run`,
          # ad-hoc profiles, or reuse from another flake.
          emacs-tools = pkgs.buildEnv {
            name = "dotemacs-tools";
            paths = emacs-tools;
          };
        };

        # Dev shell powered by devenv (see ./devenv.nix). `nix develop` still
        # works, and direnv auto-loads it via .envrc. The emacs-tools closure is
        # injected here so its single definition stays shared with the
        # `emacs-tools` package output above.
        devShells.default = devenv.lib.mkShell {
          inherit inputs pkgs;
          modules = [
            ./devenv.nix
            { packages = emacs-tools; }
          ];
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

          # ERT tests for the hand-written helpers in config.org, run on the
          # lightweight emacs-nox. Each tests/*-test.el extracts a single defun
          # from the tangled config.el and exercises it in isolation (see
          # tests/config-test-helper.el), so the whole configuration need not
          # load. Covers the auto-dark detection guard, the catppuccin flavour
          # map, the node_modules/.bin resolver, the Roslyn workspace-open plan,
          # and duplicate-keybinding detection. Tests that need the real package
          # set (command existence, evil undo system) self-skip here and run in
          # `integration-tests' below.
          unit-tests = pkgs.runCommand "dotemacs-unit-tests" { nativeBuildInputs = [ pkgs.emacs-nox ]; } ''
            cp -r ${self}/. work
            chmod -R u+w work
            cd work
            emacs --batch -Q \
              --eval "(require 'org)" \
              --eval '(org-babel-tangle-file "config.org" "config.el")'
            emacs --batch -Q -L tests \
              --eval '(dolist (f (directory-files "tests" t "-test[.]el$")) (load f nil t))' \
              -f ert-run-tests-batch-and-exit
            touch $out
          '';

          # The full test suite run against the *real* environment: the Nix
          # Emacs with the whole package set and the fully-loaded config. This
          # is what lets the otherwise-skipped tests run -- Evil keybindings
          # resolving to defined commands, and the configured undo system
          # resolving to defined undo/redo functions. Uses emacs-dotemacs-ci
          # (no ghostel) so this check does not trigger the network-flaky
          # ghostty/Zig build; the ghostel behavioural test self-skips here and
          # the structural ghostel/configured test still runs.
          integration-tests = pkgs.runCommand "dotemacs-integration-tests" { } ''
            cp -r ${self}/. work
            chmod -R u+w work
            cd work
            ${pkgs.emacs-dotemacs-ci}/bin/emacs --batch \
              --eval "(require 'org)" \
              --eval '(org-babel-tangle-file "config.org" "config.el")'
            ${pkgs.emacs-dotemacs-ci}/bin/emacs --batch -L tests \
              --eval "(progn \
                        (package-activate-all) \
                        (require 'use-package) \
                        (setq use-package-always-ensure nil) \
                        (load (expand-file-name \"config.el\") nil t))" \
              --eval '(dolist (f (directory-files "tests" t "-test[.]el$")) (load f nil t))' \
              -f ert-run-tests-batch-and-exit
            touch $out
          '';

          # Regression guard for the straight -> nix migration. Mimics real
          # startup: `package-activate-all` must make the packages' entry points
          # autoloadable WITHOUT an explicit require (this is what broke when
          # early-init.el disabled package.el — every :init/:config call hit a
          # void function). Also loads vterm, which ships a native module.
          # Runs on emacs-dotemacs-ci (no ghostel), so the ghostel native
          # module is deliberately not built or required here -- that would
          # trigger the network-flaky ghostty/Zig build.
          packages-loadable = pkgs.runCommand "dotemacs-packages-loadable" { } ''
            ${pkgs.emacs-dotemacs-ci}/bin/emacs --batch \
              --eval "(progn \
                        (package-activate-all) \
                        (dolist (fn '(gcmh-mode marginalia-mode exec-path-from-shell-initialize \
                                      corfu-mode corfu-history-mode vertico-mode evil-mode \
                                      doom-themes-visual-bell-config which-key-mode \
                                      apheleia-global-mode)) \
                          (unless (fboundp fn) \
                            (error \"not autoloaded (package activation broken?): %s\" fn))) \
                        (require 'vterm) \
                        (message \"package activation + custom packages OK\"))"
            touch $out
          '';
        };

        formatter = pkgs.nixfmt-tree;
      }
    )
    // {
      # System-independent home-manager module. Import it and set
      # `programs.dotemacs.enable = true;` (see nix/hm-module.nix for options).
      homeModules.default = import ./nix/hm-module.nix { inherit self emacsToolsFor; };

      # Adds `emacs` (patched, no packages) and `emacs-dotemacs` (patched +
      # every config.org package). Apply it in a home-manager / nix-darwin
      # config's `nixpkgs.overlays`, then reference `pkgs.emacs-dotemacs`.
      overlays.default = emacs-appearance-overlay;
    };
}
