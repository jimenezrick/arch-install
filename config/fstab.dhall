    let BlockDev = ./blockdev.dhall 

in  [ { fsEntry =
          BlockDev.Partition
          { diskModel = "Samsung_SSD_840_EVO_250GB", partNum = 1 }
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
    ]
