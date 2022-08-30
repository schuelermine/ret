# Ret

Ret is a tool that goes up the directory chain and returns the first directory with a landmark.

A landmark is an important property of the directory, such as the existence of a configuration file, being the user’s home directory, or being on a different drive.

This project is hosted on [GitHub](https://github.com/schuelermine/ret/).  
This project is on [Hackage](https://hackage.haskell.org/package/ret/).

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

You are invited to contribute more landmark specifications! Look in the source code, it’s easy.

## Shell integration

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

## Installing

This project is hosted on Hackage. You can install it via:

```sh
cabal install ret
```

This project is also redistributed for Nix in Nixpkgs under `haskellPackages.ret`.

## Building

Clone the repository and run this in it:

```sh
cabal build
```
