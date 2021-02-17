#!/bin/bash

cd ../../../rtc_datachannel_unix/libdatachannel

make -j2 libdatachannel.a USE_GNUTLS=0 USE_NICE=0 RTC_ENABLE_MEDIA=0
