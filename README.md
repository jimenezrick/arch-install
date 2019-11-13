# Reproducible and automated Arch Linux rootfs builds

+ Boot an Arch ISO and apply your remote custom config:

``` shell
wifi-menu
./arch-rebuild build-arch -c https://raw.githubusercontent.com/jimenezrick/arch-install/master/config/system.dhall
```

+ To boot an Arch ISO from a QEMU VM and bootstrap the whole process from your local repo:

``` shell
sudo tools/boot-qemu.sh iso -nographic
bash <(curl -sL https://git.io/Jer4x)  # It mounts this repo locally and runs tools/bootstrap-qemu.sh
```

+ For development, to mount this repo on the QEMU VM:

``` shell
mkdir /mnt/arch-rebuild
mount -t 9p -o trans=virtio,version=9p2000.L,rw arch-rebuild /mnt/arch-rebuild
cd /mnt/arch-rebuild
./arch-rebuild build-arch -c config/system.dhall
```
