\(luksUuid : Text) ->
{ loaderConf = ./systemd-boot/loader.conf as Text
, bootEntries =
    [ { bootName = "arch"
      , bootConf = ./systemd-boot/arch.conf.dhall luksUuid
      }
    , { bootName = "arch-lts"
      , bootConf = ./systemd-boot/arch-lts.conf.dhall luksUuid
      }
    ]
}
