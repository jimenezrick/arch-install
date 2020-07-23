# Reproducible and automated Arch Linux rootfs builds

+ Boot an Arch ISO and apply your remote custom config:

``` shell
wifi-menu
# Run the build using my system.dhall
./arch-rebuild build-arch -c https://git.io/JJlfz
```

+ To boot an Arch ISO from a QEMU VM and bootstrap the whole process from your local repo:

``` shell
# Generate all the AUR packages to be installed later during the build
./arch-rebuild build-aur-packages -c config/system.dhall -d config/restore/aur-packages
# Get a copy of /etc git repo to be used in the build
./tools/bundle-host-etc.sh
# Start VM
sudo tools/boot-qemu.sh iso [-nographic]
# Mount this repo inside the VM and run the build (using tools/bootstrap-qemu.sh)
bash <(curl -sL https://git.io/Jer4x)
```

## Things to setup later

- Get /home BTRFS snapshot

## My BIOS quirks

Enable the `IOMMU controller`.
