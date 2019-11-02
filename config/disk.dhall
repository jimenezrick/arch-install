let BlockDev = ./blockdev.dhall

in  { scratchDev = BlockDev.DiskModel { model = "INTEL_SSDSA2CW120G3" } }