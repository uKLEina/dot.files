#!/bin/bash
xmodmap -e 'keycode 255=space'
xmodmap -e 'keycode 65=Shift_L'
PID=`pidof xcape`
if [ -z ${PID} ]; then
    xcape -e '#65=space'
fi
xmodmap ~/.xmodmap
