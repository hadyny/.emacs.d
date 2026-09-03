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
          claude-agent-acp
          coreutils-prefixed
          delta
          github-copilot-cli
          marksman
          nixd
          nixfmt
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
          patchedEmacs = prev.emacs31.overrideAttrs (old: {
            patches =
              (old.patches or [ ])
              ++ prev.lib.optionals prev.stdenv.hostPlatform.isDarwin [
                (prev.fetchurl {
                  # Pinned by content hash: if upstream rewrites the patch the
                  # build fails loudly (mismatch) rather than changing silently.
                  # For stricter reproducibility, pin the URL to a commit sha.
                  url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-31/system-appearance.patch";
                  sha256 = "sha256-Uyg1A9te0oh+nXM7qq+A8sgQ5mjngumIvaWFWgsevrQ=";
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

              # svg-line: renders the mode-line as an SVG image (see the
              # Mode-line section of config.org) instead of laid-out text,
              # replacing Moody's tab/ribbon restyling outright -- Moody and
              # svg-line both rewrite `mode-line-format', and svg-line's
              # `:target 'mode-line' install replaces the whole thing with a
              # single `(:eval ...)' form, leaving Moody's `moody-replace-*'
              # nothing to `cl-subst' into. Not in nixpkgs/MELPA yet, so
              # build from source like zk4e above.
              svg-line = _efinal.trivialBuild {
                pname = "svg-line";
                version = "0.1.6";
                src = final.fetchFromGitHub {
                  owner = "chiply";
                  repo = "svg-line";
                  tag = "v0.1.6";
                  hash = "sha256-WlWvt6sZbk5Fr++Nqa6s71eVxy5Y7hFn8K/bZoD6jp0=";
                };
              };

              # svg-margin: composites indicators from independent "providers"
              # into one SVG image per margin (see the Margins section of
              # config.org) -- the Diagnostics section's own hand-rolled
              # left-margin glyphs are one such provider now, rather than
              # Flycheck drawing there itself. Same chiply author/build shape
              # as svg-line above; also not in nixpkgs/MELPA yet.
              svg-margin = _efinal.trivialBuild {
                pname = "svg-margin";
                version = "0.1.7";
                src = final.fetchFromGitHub {
                  owner = "chiply";
                  repo = "svg-margin";
                  tag = "v0.1.7";
                  hash = "sha256-Ej3hJYZgO949HY4fuXSVr5QyjW17D0Lns0eAvdqPPWk=";
                };
              };
            }
          );

          # The package list config.org needs on load-path. Keep this in sync
          # with the (use-package ...) forms in config.org; built-ins are
          # intentionally absent.
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
              consult
              consult-flycheck
              corfu
              corfu-prescient
              diff-hl
              dirvish
              docker
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
              nerd-icons
              nerd-icons-completion
              nerd-icons-corfu
              nix-mode
              orderless
              org-modern
              org-super-agenda
              prescient
              smartparens
              spacious-padding
              svg-line
              svg-margin
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

          # Exposed so `terminal-load' below can put it on `load-path'
          # directly: built with `trivialBuild' (like `zk4e'), so it lands on
          # plain `site-lisp' rather than an ELPA-shaped directory and
          # `package-activate-all' never finds it (see that check's comment).
          inherit (epkgs) svg-line svg-margin;
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
          # emacs-tools is one of its inputs. Allow exactly these packages, here
          # rather than via NIXPKGS_ALLOW_UNFREE, so the flake still evaluates
          # under pure eval (`nix flake check') and for anyone consuming it.
          # Consumers of homeModules.default need the same allowance in their
          # own nixpkgs config, or must set `programs.dotemacs.tools = [ ]'.
          # `claude-code' is pulled in as a dependency of `claude-agent-acp'
          # (the Claude Agent ACP adapter itself is Apache-2.0).
          config.allowUnfreePredicate =
            pkg:
            builtins.elem (nixpkgs.lib.getName pkg) [
              "github-copilot-cli"
              "claude-code"
            ];
        };

        # External tools config.org shells out to (shared with the home-manager
        # module default via emacsToolsFor). Keep the list in sync with the
        # eglot-server-programs / executable-find references in config.org:
        #   coreutils-prefixed            -> gls                                (config.org: dired setup)
        #   claude-agent-acp              -> agent-shell Claude Code ACP agent (bin: claude-agent-acp) (agent-shell-anthropic-claude-acp-command)
        #   delta                         -> syntax-highlighted Magit diffs      (magit-delta-delta-executable)
        #   github-copilot-cli            -> agent-shell Copilot ACP agent (bin: copilot) (agent-shell-github-acp-command)
        #   marksman                      -> Markdown LSP                        (eglot-server-programs)
        #   nixd                          -> Nix LSP                             (eglot's own nix-mode alternatives)
        #   nixfmt                        -> .nix formatting                     (apheleia's built-in nixfmt)
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

          # `nix run .#try` -- launch a throwaway Emacs on the *working tree*, so
          # a config change can be seen in a real frame before the daily Emacs is
          # restarted into it.
          #
          # `~/.emacs.d` is an out-of-store symlink to this checkout, so there is
          # otherwise no gap between "edited" and "applied": the next Emacs to
          # start tangles whatever config.org currently says, half-finished edits
          # included. The flake checks verify a change loads; this shows what it
          # looks like.
          #
          # The tree is *copied* rather than used in place. Emacs would otherwise
          # tangle config.el and write recentf/savehist/prescient state into the
          # checkout -- and `var/prescient-save.el` and `transient/history.el` are
          # tracked, so a trial run would dirty the repo. Copying keeps
          # uncommitted edits (the whole point) without that side effect.
          #
          # `$PWD`, not `${self}`: a dirty flake tree excludes untracked files,
          # and unstaged edits are exactly what wants previewing.
          try = pkgs.writeShellApplication {
            name = "dotemacs-try";
            runtimeInputs = [ pkgs.emacs-dotemacs ] ++ emacs-tools;
            text = ''
              if [ ! -f "$PWD/config.org" ]; then
                echo "dotemacs-try: no config.org here; run from a checkout" >&2
                exit 1
              fi
              dir=$(mktemp -d)
              trap 'rm -rf "$dir"' EXIT
              mkdir -p "$dir/emacs.d"
              # Skip VCS and build detritus; keep everything the config reads.
              tar -cf - --exclude=.git --exclude=.direnv --exclude=.devenv \
                        --exclude=result --exclude='result-*' -C "$PWD" . \
                | tar -xf - -C "$dir/emacs.d"
              echo "dotemacs-try: $dir/emacs.d (removed on exit)"
              emacs --init-directory="$dir/emacs.d" "$@"
            '';
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
          # load. Covers the auto-dark detection guard, the Doom Themes variant
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

            # jinx needs a libenchant backend or its test can only skip. macOS
            # has AppleSpell, but the sandbox cannot reach the spell service and
            # Linux has no dictionary at all, so supply a hunspell one here.
            # `ENCHANT_CONFIG_DIR' rather than `XDG_CONFIG_HOME': enchant
            # searches both, and this one cannot disturb Emacs' own XDG lookups.
            # en_GB must match `jinx-languages' in config.org.
            export ENCHANT_CONFIG_DIR=$PWD/.enchant
            mkdir -p "$ENCHANT_CONFIG_DIR/hunspell"
            cp ${pkgs.hunspellDicts.en_GB-ise}/share/hunspell/en_GB.aff \
               ${pkgs.hunspellDicts.en_GB-ise}/share/hunspell/en_GB.dic \
               "$ENCHANT_CONFIG_DIR/hunspell/"
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

          # Mimics real startup: `package-activate-all` must make the packages'
          # entry points autoloadable WITHOUT an explicit require. This is the
          # failure mode when early-init.el disables package.el — every
          # :init/:config call then hits a void function.
          # `svg-line'/`svg-margin' (like `zk4e' below) are built with
          # `trivialBuild', which drops their files straight on `site-lisp'
          # rather than into an ELPA-shaped `pkg-version' directory --
          # `package-activate-all' has no per-package autoloads file to find
          # there, so `svg-line-activate'/`svg-margin-register-provider' are
          # deliberately absent from this list; config.org's real
          # `use-package svg-line'/`svg-margin' has no `:defer', so it
          # hard-`require's the feature itself before calling it, never
          # relying on this path.
          # Runs on emacs-dotemacs-ci (no ghostel), so the ghostel native
          # module is deliberately not built or required here -- that would
          # trigger the network-flaky ghostty/Zig build.
          packages-loadable = pkgs.runCommand "dotemacs-packages-loadable" { } ''
            ${pkgs.emacs-dotemacs-ci}/bin/emacs --batch \
              --eval "(progn \
                        (package-activate-all) \
                        (dolist (fn '(gcmh-mode vertico-mode marginalia-mode exec-path-from-shell-initialize \
                                      corfu-mode corfu-history-mode evil-mode \
                                      doom-themes-visual-bell-config which-key-mode \
                                      apheleia-global-mode agent-shell \
                                      magit-todos-mode magit-todos-list \
                                      mixed-pitch-mode dirvish-override-dired-mode \
                                      org-super-agenda-mode consult-flycheck \
                                      flycheck-mode global-flycheck-annotate-mode \
                                      global-flycheck-eglot-mode global-ligature-mode \
                                      spacious-padding-mode)) \
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
          #
          # `svg-line' is `trivialBuild', so it lands on plain `site-lisp'
          # rather than the ELPA-shaped directory `package-directory-list'
          # points at -- added to `load-path' by hand for the same reason (see
          # the `svg-line' comment on the `packages' output above). Unlike
          # `zk4e' (also `trivialBuild'), config.org's `use-package svg-line'
          # is not `:defer'red -- it calls `svg-line-activate' straight from
          # `:config' -- so `require' actually runs here and needs this.
          # `svg-margin' is the same shape (`trivialBuild', not `:defer'red)
          # and needs the same hand-added `load-path' entry.
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
                            (add-to-list 'load-path \"${pkgs.svg-line}/share/emacs/site-lisp\") \
                            (add-to-list 'load-path \"${pkgs.svg-margin}/share/emacs/site-lisp\") \
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

          # `nix run .#try` is a shell script, so nothing else type-checks it.
          # `writeShellApplication` already runs shellcheck at build time; this
          # pins the two things that make it a *trial* rather than a rebuild:
          # an isolated `--init-directory`, and $PWD rather than the store copy.
          try-app-is-isolated =
            let
              script = "${self.packages.${system}.try}/bin/dotemacs-try";
            in
            pkgs.runCommand "dotemacs-try-app-is-isolated" { } ''
              grep -q -- '--init-directory=' ${script} \
                || { echo "try: must use --init-directory, or it writes to ~/.emacs.d" >&2; exit 1; }
              grep -q 'mktemp -d' ${script} \
                || { echo "try: must run in a temp dir, not the checkout" >&2; exit 1; }
              grep -q '\$PWD/config.org' ${script} \
                || { echo "try: must read the working tree, not the store copy" >&2; exit 1; }
              touch $out
            '';

          # Nothing else evaluates the home-manager module. Check it against a stub
          # of the options it touches: the tool closure must reach
          # `home.packages` (that is what puts the language servers on PATH), and
          # `tools = [ ]` must empty it for a consumer whose own package list
          # already installs them.
          hm-module-installs-tools =
            let
              stub = {
                options = {
                  home = {
                    packages = pkgs.lib.mkOption {
                      type = pkgs.lib.types.listOf pkgs.lib.types.package;
                      default = [ ];
                    };
                    file = pkgs.lib.mkOption {
                      type = pkgs.lib.types.attrs;
                      default = { };
                    };
                  };
                  # The module places an enchant dictionary here on Linux, so jinx
                  # has a backend; see nix/hm-module.nix.
                  xdg.configFile = pkgs.lib.mkOption {
                    type = pkgs.lib.types.attrs;
                    default = { };
                  };
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
            in
            assert builtins.elem "marksman" kept;
            assert builtins.elem "rassumfrassum" kept;
            assert (names { tools = [ ]; }) == [ ];
            pkgs.runCommand "dotemacs-hm-module-installs-tools" { } "touch $out";
        };

        formatter = pkgs.nixfmt-tree;
      }
    )
    // {
      # System-independent home-manager module. Import it and set
      # `programs.dotemacs.enable = true;` (see nix/hm-module.nix for options).
      homeModules.default = import ./nix/hm-module.nix { inherit self emacsToolsFor; };

      # The tool closure as a function of pkgs, so a consumer whose own package
      # list overlaps can build a filtered list from it rather than restating it.
      lib = { inherit emacsToolsFor; };

      # Adds `emacs` (patched, no packages) and `emacs-dotemacs` (patched +
      # every config.org package). Apply it in a home-manager / nix-darwin
      # config's `nixpkgs.overlays`, then reference `pkgs.emacs-dotemacs`.
      overlays.default = emacs-appearance-overlay;
    };
}
