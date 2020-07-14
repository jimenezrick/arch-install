let BlockDev = ./blockdev.dhall

in  { scratchDev =
        BlockDev.FsUUID { uuid = "b6a1620d-c833-47d0-8ca3-2454088eaad0" }
    }
