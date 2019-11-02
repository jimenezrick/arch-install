  λ(espPartUuid : Text)
→ λ(rootfsLuksUuid : Text)
→ let BlockDev = ./blockdev.dhall

  in  { rootDisk =
          BlockDev.DiskModel { model = "QEMU_HARDDISK" }
      , rootSubvolumes =
          [ { `1` = "@", `2` = "/" }
          , { `1` = "@etc", `2` = "/etc" }
          , { `1` = "@home", `2` = "/home" }
          , { `1` = "@snapshots", `2` = "/.snapshots" }
          ]
      , boot =
          ./boot.dhall rootfsLuksUuid
      , fstabEntries =
          ./fstab.dhall espPartUuid
      }