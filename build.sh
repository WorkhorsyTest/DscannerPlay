
cores=`grep -c ^processor /proc/cpuinfo`
make all PREFIX=build_ -j${cores}

