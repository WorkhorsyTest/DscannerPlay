
cores=`grep -c ^processor /proc/cpuinfo`
make test PREFIX=test_ -j${cores}

