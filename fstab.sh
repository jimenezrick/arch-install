# Samsung SSD 850 PRO
UUID=${1}					/boot	vfat	defaults	0 2
UUID=${2}	/         	btrfs	subvol=@	0 0
UUID=${2}	/etc      	btrfs	subvol=@etc	0 0
UUID=${2}	/home     	btrfs	subvol=@home	0 0
UUID=${2}	/.snapshots	btrfs	subvol=@snapshots	0 0

# Samsung SSD 840 EVO
#UUID=6ddce1dc-4933-4178-b53e-4b06c7875c55 /            ext4   defaults                            0      1
#UUID=cc00ba13-ee92-4dfd-b0e6-36b75ffa48b7 /home        ext4   defaults                            0      2

# Intel SSD
UUID=b6a1620d-c833-47d0-8ca3-2454088eaad0 /mnt/scratch btrfs  user,noauto,exec                    0      0

# Seagate HDD
UUID=d958cd76-7bdd-4245-9666-5046e2a99f06 /mnt/garage  btrfs  user,noauto                         0      0
