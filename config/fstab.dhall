    let BlockDev = ./blockdev.dhall 

in  let disk = ./disk.dhall 

in  [ { fsEntry =
          disk.bootDev
      , fsMountPoint =
          "/boot"
      , fsType =
          "vfat"
      , fsOpts =
          "defaults"
      , fsDump =
          0
      , fsck =
          2
      }
    , { fsEntry =
          BlockDev.DevPath { path = "/dev/mapper/cryptroot" }
      , fsMountPoint =
          "/"
      , fsType =
          "btrfs"
      , fsOpts =
          "subvol=@"
      , fsDump =
          0
      , fsck =
          0
      }
    , { fsEntry =
          BlockDev.DevPath { path = "/dev/mapper/cryptroot" }
      , fsMountPoint =
          "/home"
      , fsType =
          "btrfs"
      , fsOpts =
          "subvol=@home"
      , fsDump =
          0
      , fsck =
          0
      }
    , { fsEntry =
          BlockDev.DevPath { path = "/dev/mapper/cryptroot" }
      , fsMountPoint =
          "/.snapshots"
      , fsType =
          "btrfs"
      , fsOpts =
          "subvol=@snapshots"
      , fsDump =
          0
      , fsck =
          0
      }
    , { fsEntry =
          disk.scratchDev
      , fsMountPoint =
          "/mnt/scratch"
      , fsType =
          "ext4"
      , fsOpts =
          "user,noauto"
      , fsDump =
          0
      , fsck =
          0
      }
    , { fsEntry =
          disk.garageDev
      , fsMountPoint =
          "/mnt/garage"
      , fsType =
          "ext4"
      , fsOpts =
          "user,noauto"
      , fsDump =
          0
      , fsck =
          0
      }
    ]
