# Reproducible automated Arch Linux build

+ Boot an Arch ISO and run:

``` shell
$ wifi-menu
$ # Get arch-rebuild binary
$ ./arch-rebuild build-rootfs -c https://raw.githubusercontent.com/jimenezrick/arch-install/haskell/config/system.dhall
```

+ To boot an Arch ISO from a QEMU VM use:

``` shell
$ sudo tools/boot-qemu.sh iso
```

+ For development, to mount this repo on the QEMU VM:

``` shell
mkdir /mnt/arch-rebuild
mount -t 9p -o trans=virtio,version=9p2000.L,rw arch-rebuild /mnt/arch-rebuild
```
