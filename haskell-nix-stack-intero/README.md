# haskell-nix-stack-intero

nix: 2.2.2
  - nixpkgs: 19.03
    - ghc: 8.6.4
stack (local installation): 1.9.3 x86_64 hpack-0.31.2
emacs: 25.2.2
spacemacs: Release 0.200.13.x (release-0.200 on GitHub)
  - intero (local installation): 0.1.34

start emacs daemon in nix-shell `emacs --daemon`

## stack config

```yaml
resolver: lts-13.17 -- need to align with the version of ghc provided by nix

nix:
  enable: false

system-ghc: true
```
