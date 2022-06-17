# Ret

Ret is a tool that goes up the directory chain and returns the first directory with a landmark.

A landmark is an important property of the directory, such as the existence of a configuration file, being the user’s home directory, or being on a different drive.

This project is hosted on GitHub: https://github.com/schuelermine/ret/

## How to use

You can use this to go back to your project root in a jiffy by using your shell’s command substitution syntax,
e.g.:

```bash
cd $(ret)
```
or
```fish
cd (ret)
```

You can specify a set of landmark names as the arguments to `ret` to only use them.
You can also specify a set of default landmark names in `$XDG_CONFIG_HOME/ret/landmarks.txt`, one per line.

You are invited to contribute more landmark specifications! Or send me an email if you want one added.

## Installing

### Shell integration

After installing (see below), you can make using `ret` even more convenient by defining a shell function that does `cd` for you.

e.g.
```bash
ret() {
  cd $(command ret "$@")
}
```
or
```fish
function ret
  cd (command ret $argv)
end
```

### Manual install

You can, of course, manually copy the `ret` binary into a location of your choice.
You can download the binary from the GitHub releases or build it yourself.

### Debian install

You can install the Debian package that’s downloadable from the GitHub releases page using `dpkg` or `apt` or another frontend.

**Warning:** The Debian package is produced without care and has a wrong version number.

### Cabal install

Clone this repository and run

```sh
cabal install
```

### NixOS install

This repository provides a Nixpkgs overlay you can use to get this into your flake-based NixOS or home-manager config.  
Add `github:schuelermine/ret` to your inputs, add `ret.overlays.default` to your overlays, and add `ret` to your packages.

## How to build

You can build it using The Haskell Cabal or Nix.

### Cabal build

```sh
cabal build
```

### Nix build

First, enable the nix-command and flakes experimental features.  
Then, run

```sh
nix build
```

### Build Debian package

Make sure that you have Nix installed and enabled the nix-command and flakes experimental features.

```sh
nix bundle --bundler bundlers#toDEB
```
