λ(rootfsLuksUuid : Text) →
  { loaderConf = ./systemd-boot/loader.conf as Text
  , bootEntries =
    [ { _1 = "arch", _2 = ./systemd-boot/arch.conf.dhall rootfsLuksUuid }
    , { _1 = "arch-lts"
      , _2 = ./systemd-boot/arch-lts.conf.dhall rootfsLuksUuid
      }
    , { _1 = "arch-initial-build"
      , _2 = ./systemd-boot/arch-initial-build.conf.dhall rootfsLuksUuid
      }
    ]
  }
