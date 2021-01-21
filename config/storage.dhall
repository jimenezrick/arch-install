λ(espPartUuid : Text) →
λ(rootfsLuksUuid : Text) →
  let BlockDev = ./blockdev.dhall

  in  { rootDisk = BlockDev.DiskModel { model = "QEMU_HARDDISK" }
      , rootSubvolumes =
        [ { _1 = "@", _2 = "/" }
        , { _1 = "@home", _2 = "/home" }
        , { _1 = "@snapshots", _2 = "/.snapshots" }
        ]
      , boot = ./boot.dhall rootfsLuksUuid
      , fstabEntries = ./fstab.dhall espPartUuid
      , passphrase = ./secrets/disk-passphrase as Text
      }
