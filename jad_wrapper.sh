#!/bin/bash

FILES=$(ls *.jar)

# unzip the jar files
for file in $FILES ;
do
    DIR=${file/.jar//}
    mkdir $DIR
    cp $file $DIR
    cd $DIR
    unzip $file
    for srcfile in $(find -name \*.class) ;
    do
        echo $srcfile
        DIR=$(dirname $srcfile)
        BASE=$(basename $srcfile)
        pushd .
        cd $DIR
        jad -s java $BASE
        popd
    done
    cd ..
done
