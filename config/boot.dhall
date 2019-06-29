    let UUID = ./uuid.dhall 

in  let withUuid = ./withUuid.dhall 

in    λ(luks : UUID)
    → { loaderConf =
          ./systemd-boot/loader.conf as Text
      , bootEntries =
          [ { bootName =
                "arch"
            , bootConf =
                ./systemd-boot/arch.conf.dhall  (withUuid luks)
            }
          , { bootName =
                "arch-lts"
            , bootConf =
                ./systemd-boot/arch-lts.conf.dhall  (withUuid luks)
            }
          ]
      }
