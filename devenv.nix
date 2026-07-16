{ pkgs, ... }:
{
  # Dev shell for this repo. See https://devenv.sh/reference/options/ .
  # The emacs-tools language-server closure (marksman, roslyn-ls, …) is added
  # from flake.nix so its definition stays in one place.

  # emacs-nox: headless Emacs for batch tangle/lint/tests. The appearance
  # overlay only overrides `emacs`, not `emacs-nox`, so this is a plain build
  # (no GUI source compile).
  packages = [
    pkgs.emacs-nox
    pkgs.gnumake
  ];

  # Format Nix before every commit, so all .nix files stay consistent with the
  # flake's `nix fmt` (nixfmt). Installed into .git/hooks on shell entry.
  git-hooks.hooks.nixfmt-rfc-style.enable = true;
}
