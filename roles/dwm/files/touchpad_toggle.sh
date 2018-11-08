#!/bin/sh
STATUS=`xinput --list-props 13 | grep "Device Enabled" | awk '{print $4}'`
if [ $STATUS == 1 ];
then
    xinput disable 13;
else
    xinput enable 13;
fi
