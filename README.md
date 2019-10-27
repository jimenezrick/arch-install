# Reproducible automated Arch Linux build

Boot Arch ISO and run:

``` shell
$ wifi-menu
$ # Get arch-rebuild binary
$ ./arch-rebuild build-rootfs -c https://raw.githubusercontent.com/jimenezrick/arch-install/haskell/config/system.dhall
```

To boot Arch ISO from QEMU use:

``` shell
$ sudo tools/boot-qemu.sh iso
```
