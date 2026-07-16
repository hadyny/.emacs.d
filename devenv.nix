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

  # Run the flake's own `nix fmt` (nixfmt-tree) before every commit, so a commit
  # can't land unformatted files. Using `nix fmt` itself — rather than a
  # separate nixfmt hook — keeps this identical to CI's `nix fmt -- --ci`, so
  # the two can never disagree. `nix fmt` rewrites in place; if it changes
  # anything the commit aborts ("files were modified by this hook") and you
  # re-stage. Installed into the git hooks dir on shell entry.
  git-hooks.hooks.nix-fmt = {
    enable = true;
    name = "nix fmt";
    entry = "nix fmt";
    # nix fmt (treefmt) walks the tree itself; don't hand it individual paths.
    pass_filenames = false;
    # Only bother when a .nix file is staged.
    files = "\\.nix$";
  };
}
