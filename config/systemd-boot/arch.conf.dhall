\(luksName : Text) -> \(luksUuid : Text) ->
''
title   Arch Linux
linux   /vmlinuz-linux
initrd  /amd-ucode.img
initrd  /initramfs-linux.img
options root=/dev/mapper/cryptroot rootflags=subvol=@ luks.name=${luksName} luks.uuid=${luksUuid}=cryptroot rw iommu=soft
''
