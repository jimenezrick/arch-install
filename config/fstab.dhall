[
  { fstabEntry = DiskModel "Samsung_SSD_840_EVO_250GB"
  , mountPoint = "/boot"
  , fsType = "vfat"
  , fsOpts = "defaults"
  , dump = 0
  , fsck = 2
  }
] : List
      { entry : < DiskModel : Text | Device : Text >
      , mountPoint : Text
      , fsType : Text
      , fsOpts : Text
      , dump : Natural
      , fsck : Natural
      }
