\(luksUuid : Text) ->
''
title   Arch Linux LTS
linux   /vmlinuz-linux-lts
initrd  /amd-ucode.img
initrd  /initramfs-linux-lts.img
options root=/dev/mapper/cryptroot rootflags=subvol=@ luks.uuid=${luksUuid} luks.name=${luksUuid}=cryptroot rw iommu=soft
''
