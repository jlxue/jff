which mformat >/dev/null || {
    echo "mtools isn't installed!"
    exit 1
}

dd if=/dev/zero of=boot.img bs=1k count=1440


# mkdosfs boot.img
# or
mformat -i boot.img -f 1440 ::


# sudo mount -t vfat -o loop boot.img /mnt
# sudo mkdir -p /mnt/boot/grub
# sudo cp /lib/grub/i386-pc/stage? /mnt/boot/grub
# or
mmd -i boot.img ::boot
mmd -i boot.img ::boot/grub
mcopy -i boot.img /lib/grub/i386-pc/stage? ::boot/grub


grub --batch <<EOF
device (fd0) boot.img
root (fd0)
setup (fd0)
quit
EOF


# sudo rm /mnt/boot/grub/stage1
# sudo cp menu.lst /mnt/boot/grub
#   # cp /opt/rtems/i386-rtems4.8/pc386/lib/rtems-4.8/tests/hello.exe .
#   # gzip hello.exe.gz
# sudo cp *.gz /mnt/boot
# sudo umount /mnt
# or
mdel -i boot.img ::boot/grub/stage1
mcopy -i boot.img menu.lst ::boot/grub
mcopy -i boot.img *.gz ::boot

mcopy -i boot.img *.class ::

