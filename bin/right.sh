#!/bin/bash
out_w=`expr $1 \* 2`
out_h=`expr $2 \* 2`
size_w=`expr 2560 \+ $1 \* 2`
# xrandr --output eDP-1 --scale 1.25x1.25 --mode 2560x1440 --panning 3200x1800 --output HDMI-2 --scale 1x1 --mode $1x$2 --panning ${out_w}x${out_h}+3200+0 # --right-of eDP-1
xrandr --output eDP-1 --scale 1.25x1.25 --mode 2560x1440 --panning 3200x1800 --pos 0x0 --output HDMI-2 --scale 2x2 --mode $1x$2 --panning ${out_w}x${out_h}+3200+0 --pos 3200x0 --fb ${size_w}x1800
