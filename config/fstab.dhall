    let BlockDev = ./blockdev.dhall 

in  [ { fsEntry =
          BlockDev.Partition
          { diskModel = "Samsung_SSD_850_XXX_XXX_XXX", partNum = 1 }
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
          BlockDev.DiskModel { model = "INTEL_SSDSA2CW120G3" }
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
          BlockDev.DiskModel { model = "SEAGATE_ST32502NSSUN250G_0920811VV4" }
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
