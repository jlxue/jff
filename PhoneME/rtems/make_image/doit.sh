D="/home/dieken/Work/rtems/build-rtems/i386-rtems4.8/c/pc386/testsuites/samples/iostream"
[ -d "$D" ] && {
    P="/home/dieken/Work/rtems/image"
    export PATH=/opt/rtems/bin:$PATH
    cd /home/dieken/Work/tmp/phoneme_rtems/cldc/rtems_i386/target/product &&
        /bin/cp -f jvmspi.o Main_rtems.o NativesTable.o ROMImage.o ../bin/libcldc_vmx.a ../bin/libcldc_vmtest.a ../bin/libcldc_vm.a /home/dieken/Work/tmp/phoneme_rtems/pcsl/rtems_i386/lib/libpcsl_memory.a /home/dieken/Work/tmp/phoneme_rtems/pcsl/rtems_i386/lib/libpcsl_print.a /home/dieken/Work/tmp/phoneme_rtems/pcsl/rtems_i386/lib/libpcsl_network.a /home/dieken/Work/tmp/phoneme_rtems/pcsl/rtems_i386/lib/libpcsl_string.a /home/dieken/Work/tmp/phoneme_rtems/pcsl/rtems_i386/lib/libpcsl_file.a /home/dieken/Work/rtems/build-rtems/i386-rtems4.8/c/pc386/testsuites/samples/iostream &&
        cd $D && /bin/rm -f iostream* init* && 
        make && cp iostream.exe $P
    cd $P
}

sleep 1

gzip -f iostream.exe
./mkbootimg.sh

sleep 1

~/usr/local/bin/qemu -L ~/usr/local/share/qemu -fda boot.img

