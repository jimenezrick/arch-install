  λ(rootfsLuksUuid : Text)
→ { loaderConf =
      ./systemd-boot/loader.conf as Text
  , bootEntries =
      [ { `1` = "arch", `2` = ./systemd-boot/arch.conf.dhall rootfsLuksUuid }
      , { `1` =
            "arch-lts"
        , `2` =
            ./systemd-boot/arch-lts.conf.dhall rootfsLuksUuid
        }
      ]
  }