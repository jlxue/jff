~/usr/local/bin/qemu -L ~/usr/local/share/qemu -fda boot.img -hda hda.img -boot a

# debug
#/home/dieken/usr/local/bin/qemu -L /home/dieken/usr/local/share/qemu/ \
#   -fda boot.img -hda hda.img -d int,out_asm,in_asm,op,op_opt,exec,cpu,pcall -boot a
