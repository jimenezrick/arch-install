# Reproducible and automated Arch Linux rootfs builds

+ Boot an Arch ISO and apply your remote custom config:

``` shell
iwctl
# Run the build using my system.dhall:
./arch-rebuild build-arch -c https://git.io/JJlfz
```

+ To boot an Arch ISO from a QEMU VM and bootstrap the whole process from your local repo:

``` shell
# Get a copy of /etc git repo to be used in the build and prepare AUR packages:
./tools/bundle-host-etc-aur.sh
# Start VM:
sudo tools/boot-qemu.sh iso [-nographic] # boot kernel with `console=ttyS0', edit boot entry with `e'
# Mount this repo inside the VM and run the build (using tools/bootstrap-qemu.sh):
bash <(curl -sL https://git.io/Jer4x)
```

## Things to setup later

- Get /home BTRFS snapshot

## My BIOS quirks

Enable the `IOMMU controller`.

## TODO

- Being able to generate an Arch system outside of a VM, even maybe from CI.
