  λ(rootfsLuksUuid : Text)
→ let disk = ./disk.dhall

  in  { loaderConf =
          ./systemd-boot/loader.conf as Text
      , bootEntries =
          [ { bootName =
                "arch"
            , bootConf =
                ./systemd-boot/arch.conf.dhall rootfsLuksUuid
            }
          , { bootName =
                "arch-lts"
            , bootConf =
                ./systemd-boot/arch-lts.conf.dhall rootfsLuksUuid
            }
          ]
      }