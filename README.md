# Ret

Ret is a tool that goes up the directory chain and returns the first directory with a landmark.

A landmark is an important property of the directory, such as the existence of a configuration file, being the user’s home directory, or being on a different drive.

## How to use

You can use this to go back to your project root in a jiffy by using your shell’s command substitution syntax,
e.g.:

```bash
cd $(ret)
```

You can specify a set of landmark names as the arguments to `ret` to only use them.
You can also specify a set of default landmark names in `$XDG_CONFIG_HOME/ret/landmarks.txt`.

Current landmarks are:

- Specific files
  - `CMakeLists.txt`
  - `Makefile`
  - `meson.build`
  - `build.xml`
  - `stack.yaml`
  - `manifest.json`
  - `tsconfig.json`
  - `.gitignore`
  - `.sublime-project`
  - `.sublime-workspace`
  - `.envrc`
  - `flake.nix`
  - `shell.nix`
  - `default.nix`
- Specific directories
  - `.git`
  - `.hg`
  - `.svn`
  - `.pijul`
  - `.bzr`
  - `.darcs`
  - `.vscode`
  - `.direnv`
- Other checks
  - `changelog`: A file with base name “changelog” (case-insensitive) exists
  - `license`: ditto, named “license”
  - `.cabal`: A file ending in `.cabal` exists
  - `device`: The directory changed device ID (e.g. after a mountpoint or symlink)
  - `home`: The directory is your home directory
  - `symlink`: The directory is a symlink

You are invited to contribute more landmark specifications! Or send me an email if you want one added.

## Installing

You can, of course, manually copy the `ret` binary into a location of your choice.

### Cabal

Clone this repository and run

```bash
cabal install
```

### NixOS

This repository provides a Nixpkgs overlay you can use to get this into your flake-based NixOS or home-manager config.  
Add `github:schuelermine/ret` to your inputs, add `ret.overlays.default` to your overlays, and add `ret` to your packages.

## How to build

You can build it using The Haskell Cabal or Nix.

### Cabal

```sh
cabal build
```

### Nix

First, enable the nix-command and flakes experimental features.  
Then, run

```sh
nix build
```
