λ(luksUuid : Text) →
  ''
  title   Arch Linux (initial build)
  linux   /vmlinuz-linux
  initrd  /amd-ucode.img
  initrd  /initramfs-linux.img
  options root=/dev/mapper/cryptroot rootflags=subvol=@snapshots/@-initial-build rd.luks.name=${luksUuid}=cryptroot rw iommu=soft
  ''
