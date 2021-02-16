#!/bin/bash

cd libdatachannel

cmake -B build -DUSE_GNUTLS=0 -DUSE_NICE=0 -DNO_MEDIA=1

cd build
make -j2
