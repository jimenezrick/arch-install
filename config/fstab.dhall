let FstabEntryType = < Disk : { model : Text } | Device : { path : Text } >
in [
  { fsEntry = FstabEntryType.Disk { model = "Samsung_SSD_840_EVO_250GB" }
  , fsMountPoint = "/boot"
  , fsType = "vfat"
  , fsOpts = "defaults"
  , fsDump = 0
  , fsck = 2
  }
]
