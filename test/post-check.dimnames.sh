#!/bin/bash

if [[ ${whichCMD:+x} == "" ]]; then
    cmd='ncdump -h'
else
    cmd=${whichCMD}
fi


if [[ -z "`${cmd} test.dimnames.nc | grep 'dim1 = 2'`" ]]; then
    echo -e "\033[31m Error in test.dimnames, please see the nc file \033[0m"
    exit 1
fi
if [[ -z "`${cmd} test.dimnames.nc | grep 'dimension2 = 3'`" ]]; then
    echo -e "\033[31m Error in test.dimnames, please see the nc file \033[0m"
    exit 1
fi
if [[ -z "`${cmd} test.dimnames.nc | grep 'd3 = 4'`" ]]; then
    echo -e "\033[31m Error in test.dimnames, please see the nc file \033[0m"
    exit 1
fi
if [[ -z "`${cmd} test.dimnames.nc | grep 'ia4.d1 = 4'`" ]]; then
    echo -e "\033[31m Error in test.dimnames, please see the nc file \033[0m"
    exit 1
fi