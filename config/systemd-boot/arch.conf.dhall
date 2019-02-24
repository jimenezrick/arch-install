  λ(luksUuid : Text)
→ ''
  title   Arch Linux
  linux   /vmlinuz-linux
  initrd  /amd-ucode.img
  initrd  /initramfs-linux.img
  options root=/dev/mapper/cryptroot rootflags=subvol=@ luks.uuid=${luksUuid} luks.name=${luksUuid}=cryptroot rw iommu=soft
  ''
