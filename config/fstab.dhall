let FstabEntryType = < Disk : { model : Text } | Partition : { model : Text, number : Natural } | Device : { path : Text } >
in [
  { fsEntry = FstabEntryType.Partition { model = "Samsung_SSD_840_EVO_250GB", number = 1 }
  , fsMountPoint = "/boot"
  , fsType = "vfat"
  , fsOpts = "defaults"
  , fsDump = 0
  , fsck = 2
  }
]
