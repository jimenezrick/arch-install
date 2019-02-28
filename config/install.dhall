    let BlockDev = ./blockdev.dhall 

in  { system =
        ./system.dhall 
    , rootfsImage =
        "../imgs/rootfs.img"
    , espImage =
        "../imgs/esp.img"
    , rootDisk =
        BlockDev.DiskModel { model = "Samsung SSD 850 XXX XXX XXX" }
    }
