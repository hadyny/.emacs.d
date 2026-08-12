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
          delta
          github-copilot-cli
          marksman
          rassumfrassum
          ripgrep
          roslyn-ls
          tailwindcss-language-server
          typescript
          typescript-language-server
          vscode-langservers-extracted
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

          # evil-ghostel lives inside the ghostel repo (dakra/ghostel,
          # extensions/evil-ghostel) and shares ghostel's `:around' advice
          # contract on `ghostel--redraw' / `ghostel--apply-cursor-style'. The two
          # MUST therefore be built from the same revision. nixpkgs packages them
          # from independent revs, which skews the advice arity: e.g. ghostel
          # 0.45.0 calls `ghostel--redraw' with (term full force-sync) so the
          # advice is invoked with 4 args, but an evil-ghostel from an older rev
          # accepts fewer -> "wrong-number-of-arguments evil-ghostel--around-redraw 4"
          # and the terminal never redraws. Build evil-ghostel from the *exact*
          # ghostel package source so the pair can never drift again (self-tracking
          # across future ghostel bumps).
          epkgs = (final.emacsPackagesFor patchedEmacs).overrideScope (
            _efinal: eprev: {
              # ghostel: the `zig-deps' fixed-output derivation downloads ~40 Zig
              # packages from GitHub in one `zig build --fetch=all' run. Zig
              # 0.16 pools keep-alive connections. On a long fetch (~3.5 min)
              # GitHub closes an idle pooled connection, Zig then reuses it and
              # aborts with "invalid HTTP response: HttpConnectionClosing". The
              # package that fails differs on each run, and a single fetch of
              # that same URL always succeeds -- so this is connection reuse,
              # not a bad URL, not the sandbox and not concurrency (it also
              # fails with -j1).
              #
              # nixpkgs' recipe makes a new ZIG_GLOBAL_CACHE_DIR on every build,
              # so each retry restarts at zero packages and never converges.
              # Retry *inside* one build against a cache that stays, so progress
              # accumulates: attempt 1 fetched 39 of 40 packages, attempt 2
              # resumed and completed.
              #
              # This is safe to carry. A fixed-output derivation is addressed by
              # (hash, name), not by its build steps, so the override changes
              # the .drv but keeps the same output path. Nobody who already has
              # the path rebuilds, and the loop only runs on a cold fetch.
              # nixpkgs hoists `zig' and `zigDeps' to the top level of the
              # package for exactly this kind of override, and `passthru.module'
              # reads `finalAttrs.zigDeps', so one override covers both.
              # Drop this once nixpkgs vendors the Zig dependencies with
              # fetchgit, or once Zig retries on a closed pooled connection.
              ghostel = eprev.ghostel.overrideAttrs (old: {
                zigDeps = old.zigDeps.overrideAttrs (_: {
                  buildCommand = ''
                    export ZIG_GLOBAL_CACHE_DIR=$(mktemp -d)
                    mkdir -p $ZIG_GLOBAL_CACHE_DIR/tmp
                    runHook unpackPhase

                    cd $sourceRoot
                    for attempt in $(seq 1 10); do
                      if zig build --fetch''${fetchAll:+=all}; then
                        break
                      fi
                      if [ "$attempt" = 10 ]; then
                        echo "zig fetch failed after $attempt attempts" >&2
                        exit 1
                      fi
                      echo "zig fetch attempt $attempt failed; retrying (cache is kept)"
                    done

                    mv $ZIG_GLOBAL_CACHE_DIR/p $out
                  '';
                });
              });

              evil-ghostel = eprev.evil-ghostel.overrideAttrs (_old: {
                src = eprev.ghostel.src;
              });

              # flycheck: Flycheck 38 (released 2026-07-29) is the whole reason
              # config.org is on Flycheck rather than built-in Flymake -- the
              # bundled Eglot bridge (`global-flycheck-eglot-mode', which
              # obsoletes the third-party flycheck-eglot), inline diagnostics
              # (`global-flycheck-annotate-mode') and applicable fixes
              # (`flycheck-fix-error-at-point') all landed in it. nixpkgs (and
              # nixos-unstable) still package the MELPA snapshot
              # 20260720.531, from nine days *before* the release, which defines
              # none of those, so bumping the nixpkgs input does not help. Pin
              # the released tag instead. src-only is enough and cannot skew the
              # closure: v38.3 is still a single flycheck.el (the MELPA recipe
              # has no `:files') and its Package-Requires is unchanged
              # (emacs 28.1, seq 2.24). Drop this override once nixpkgs'
              # melpa-generated.nix passes 38.
              flycheck = eprev.flycheck.overrideAttrs (_old: {
                src = final.fetchFromGitHub {
                  owner = "flycheck";
                  repo = "flycheck";
                  tag = "v38.3";
                  hash = "sha256-X9AnHTZ2wM36iBgFkv6zS/tSI3iTcdOPNMxczXDCLNY=";
                };
              });

              # org: GNU ELPA has deleted the uncompressed org-9.8.7.tar that
              # nixpkgs still pins (only the .lz remains), so the fixed-output
              # fetch falls back to the .lz whose hash differs from the pinned
              # .tar hash -- breaking every build that fetches org fresh (CI). It
              # passes locally only because org is substituted from the binary
              # cache. nixpkgs master is equally stale (still 9.8.7 -> the dead
              # .tar), so bumping the input does not help. Rebuild org against
              # ELPA's current release 9.8.8, whose plain tarball is still
              # served, mirroring nixpkgs' own elpa-generated.nix definition.
              # Propagates to org-modern and the other org dependents in scope.
              # Revisit once nixpkgs regenerates elpa-generated.nix past 9.8.7.
              org = _efinal.elpaBuild {
                pname = "org";
                ename = "org";
                version = "9.8.8";
                src = final.fetchurl {
                  url = "https://elpa.gnu.org/packages/org-9.8.8.tar";
                  hash = "sha256-oF8gH3O9mj+SeiF1DJSlregspzEDlNO99f2h2dhwt2Y=";
                };
                packageRequires = [ ];
                meta = {
                  homepage = "https://elpa.gnu.org/packages/org.html";
                  license = final.lib.licenses.free;
                };
              };

              # zk4e: Emacs interface for the zk-org CLI (config.org uses it for
              # note browsing/creation). Not in nixpkgs, so build from source.
              # The Citar integration file is dropped so tomlparse is the only
              # extra dependency; tomlparse's TOML tree-sitter grammar is only
              # needed at runtime (notebook parsing), not to byte-compile.
              zk4e = _efinal.trivialBuild {
                pname = "zk4e";
                version = "0-unstable-2026-05-11";
                src = final.fetchgit {
                  url = "https://codeberg.org/mcookly/zk4e.git";
                  rev = "b27ca4a0fe55418b65a0bf95846f8eff11c9507d";
                  hash = "sha256-vwmDTA/7Q+UZg+/zgeHJqsR13JLNULcsj3OUoQ+bhcw=";
                };
                postPatch = "rm -f zk4e-citar.el";
                packageRequires = [ _efinal.tomlparse ];
              };
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
              agent-shell
              apheleia
              auto-dark
              cape
              catppuccin-theme
              consult
              consult-flycheck
              corfu
              corfu-prescient
              diff-hl
              dirvish
              doom-themes
              # eglot from GNU ELPA, not the copy bundled with Emacs 30.2
              # (1.17.30). Roslyn reports diagnostics by pull only, and pull
              # support landed in 1.20; `eglot-code-action-indications' (set in
              # config.org) landed in 1.19 and is a no-op before it. This also
              # brings ELPA jsonrpc/flymake/project/xref/eldoc onto the
              # load-path ahead of the bundled copies.
              eglot
              eldoc-box
              embark
              embark-consult
              evil
              evil-collection
              evil-surround
              exec-path-from-shell
              flycheck
              gcmh
              helpful
              jinx
              ligature
              magit
              magit-delta
              magit-todos
              marginalia
              markdown-mode
              mixed-pitch
              mood-line
              nerd-icons
              nerd-icons-corfu
              nix-mode
              orderless
              org-modern
              org-super-agenda
              prescient
              smartparens
              treesit-auto
              vertico
              vertico-prescient
              wgrep
              which-key
              yasnippet
              yasnippet-snippets
              zk4e
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
          # github-copilot-cli (the `copilot' binary agent-shell's Copilot
          # backend runs) is unfree, and evaluating an unfree package is a hard
          # error unless allowed -- which took down the whole devShell, since
          # emacs-tools is one of its inputs. Allow exactly this package, here
          # rather than via NIXPKGS_ALLOW_UNFREE, so the flake still evaluates
          # under pure eval (`nix flake check') and for anyone consuming it.
          # Consumers of homeModules.default need the same allowance in their
          # own nixpkgs config, or must set `programs.dotemacs.tools = [ ]'.
          config.allowUnfreePredicate = pkg: builtins.elem (nixpkgs.lib.getName pkg) [ "github-copilot-cli" ];
        };

        # External tools config.org shells out to (shared with the home-manager
        # module default via emacsToolsFor). Keep the list in sync with the
        # eglot-server-programs / executable-find references in config.org:
        #   coreutils-prefixed            -> gls                                (config.org: dired setup)
        #   delta                         -> syntax-highlighted Magit diffs      (magit-delta-delta-executable)
        #   github-copilot-cli            -> agent-shell Copilot ACP agent (bin: copilot) (agent-shell-github-acp-command)
        #   marksman                      -> Markdown LSP                        (eglot-server-programs)
        #   rassumfrassum                 -> rass, the LSP multiplexer for TS/TSX (eglot-server-programs)
        #   ripgrep                       -> rg for consult-ripgrep + magit-todos' scanner (magit-todos--choose-scanner)
        #   roslyn-ls                     -> Microsoft.CodeAnalysis.LanguageServer (eglot-server-programs)
        #   tailwindcss-language-server   -> Tailwind class completion in TS/TSX   (rass tslint -- ...)
        #   typescript-language-server    -> TypeScript/TSX LSP                    (rass tslint preset)
        #   typescript                    -> tsserver for the tsls fallback
        #   vscode-langservers-extracted  -> vscode-eslint-language-server         (rass tslint preset)
        # Eglot connects one server per (major-mode, project), so TS/TSX goes through
        # `rass': the bundled tslint preset runs typescript-language-server plus
        # vscode-eslint-language-server, and tailwindcss-language-server is appended.
        # These are global fallbacks -- rass resolves each server from the PATH it
        # inherits, so a project-local copy in node_modules still wins
        # (my/add-node-modules-path prepends node_modules/.bin to exec-path and PATH).
        # The tslint preset probes for vscode-eslint-language-server first and falls
        # back to the name eslint-language-server.
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
          # void function).
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
                                      apheleia-global-mode agent-shell \
                                      magit-todos-mode magit-todos-list \
                                      mixed-pitch-mode dirvish-override-dired-mode \
                                      org-super-agenda-mode consult-flycheck \
                                      flycheck-mode global-flycheck-annotate-mode \
                                      global-flycheck-eglot-mode)) \
                          (unless (fboundp fn) \
                            (error \"not autoloaded (package activation broken?): %s\" fn))) \
                        (message \"package activation + custom packages OK\"))"
            touch $out
          '';

          # The config must load on a terminal-only Emacs, not just a GUI one.
          # This matters in practice: the devenv shell puts `emacs-nox' on PATH
          # (devenv.nix), so `emacs' inside this repo IS the nox build. It has no
          # window system, so window-system-only builtins (`set-fontset-font',
          # `scroll-bar-mode', `tool-bar-mode', ...) are void there -- an
          # unguarded call at top level aborts the rest of config.el, and one
          # inside a `:config' body silently loses the rest of that block.
          #
          # None of the checks above catch that: `smoke' only byte-compiles (top
          # level forms never run) and `integration-tests' loads the config on
          # emacs-dotemacs-ci, a GUI-capable build where those symbols exist.
          #
          # emacs-nox is paired with the CI package set's .elc files
          # (`emacs-dotemacs-ci.deps') rather than a second package set built
          # against nox: same Emacs version, so the byte-code is portable, and
          # this adds no package builds. `use-package-expand-minimally' drops
          # use-package's condition-case wrappers so an error inside a `:config'
          # body is fatal here instead of merely logged.
          terminal-load =
            pkgs.runCommand "dotemacs-terminal-load" { nativeBuildInputs = [ pkgs.emacs-nox ]; }
              ''
                cp -r ${self}/. work
                chmod -R u+w work
                cd work
                # recentf/savehist write under HOME; the sandbox default is read-only.
                export HOME=$TMPDIR
                emacs --batch -Q \
                  --eval "(require 'org)" \
                  --eval '(org-babel-tangle-file "config.org" "config.el")'
                emacs --batch -Q \
                  --eval "(progn \
                            (setq package-directory-list \
                                  (list \"${pkgs.emacs-dotemacs-ci.deps}/share/emacs/site-lisp/elpa\")) \
                            (package-activate-all) \
                            (require 'use-package) \
                            (setq use-package-always-ensure nil \
                                  use-package-expand-minimally t) \
                            (load (expand-file-name \"config.el\") nil t) \
                            (message \"terminal load OK\"))"
                touch $out
              '';

          # `programs.dotemacs.excludeTools` is the escape hatch for a tool the
          # consumer's own config already puts on PATH. `home.packages` is a
          # buildEnv: it dedupes an identical store path, but two *different*
          # derivations owning `bin/delta` (a wrapped one, or one from another
          # nixpkgs) is a hard failure, and the only alternative was `tools = [
          # ]`, which drops the whole closure. Evaluate the module against a
          # stub of the home-manager options it touches and check the filtering,
          # including that it leaves everything else alone.
          hm-module-excludes-tools =
            let
              stub = {
                options.home.packages = pkgs.lib.mkOption {
                  type = pkgs.lib.types.listOf pkgs.lib.types.package;
                  default = [ ];
                };
                options.home.file = pkgs.lib.mkOption {
                  type = pkgs.lib.types.attrs;
                  default = { };
                };
              };
              evalWith =
                settings:
                (pkgs.lib.evalModules {
                  modules = [
                    stub
                    (import ./nix/hm-module.nix { inherit self emacsToolsFor; })
                    {
                      _module.args.pkgs = pkgs;
                      programs.dotemacs = {
                        enable = true;
                      }
                      // settings;
                    }
                  ];
                }).config.home.packages;
              names = settings: map pkgs.lib.getName (evalWith settings);
              kept = names { };
              filtered = names { excludeTools = [ "delta" ]; };
            in
            assert builtins.elem "delta" kept;
            assert !(builtins.elem "delta" filtered);
            # Nothing else may vanish with it.
            assert (builtins.length kept) == (builtins.length filtered) + 1;
            assert builtins.elem "marksman" filtered;
            pkgs.runCommand "dotemacs-hm-module-excludes-tools" { } "touch $out";
        };

        formatter = pkgs.nixfmt-tree;
      }
    )
    // {
      # System-independent home-manager module. Import it and set
      # `programs.dotemacs.enable = true;` (see nix/hm-module.nix for options).
      homeModules.default = import ./nix/hm-module.nix { inherit self emacsToolsFor; };

      # The tool closure as a function of pkgs, so a consumer can build its own
      # list from it rather than restating it. `programs.dotemacs.excludeTools`
      # covers the common case (drop one tool another module already installs);
      # this is for anything more involved.
      lib = { inherit emacsToolsFor; };

      # Adds `emacs` (patched, no packages) and `emacs-dotemacs` (patched +
      # every config.org package). Apply it in a home-manager / nix-darwin
      # config's `nixpkgs.overlays`, then reference `pkgs.emacs-dotemacs`.
      overlays.default = emacs-appearance-overlay;
    };
}
