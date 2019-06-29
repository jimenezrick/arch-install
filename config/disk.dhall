    let BlockDev = ./blockdev.dhall 

in  let rootDiskModel = "Samsung_SSD_840_EVO_250GB"

in  { rootDisk =
        BlockDev.DiskModel { model = rootDiskModel }
    , rootDev =
        BlockDev.Partition { diskModel = rootDiskModel, partNum = 1 }
    , bootDev =
        BlockDev.Partition { diskModel = rootDiskModel, partNum = 2 }
    , scratchDev =
        BlockDev.DiskModel { model = "INTEL_SSDSA2CW120G3" }
    , garageDev =
        BlockDev.DiskModel { model = "SEAGATE_ST32502NSSUN250G_0920811VV4" }
    }
