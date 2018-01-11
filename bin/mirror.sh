#!/bin/bash
out_w=`expr $1 \* 2`
out_h=`expr $2 \* 2`
xrandr --output eDP-1 --scale 2x2 --mode $1x$2 --panning ${out_w}x${out_h} --output HDMI-2 --scale 2x2 --mode $1x$2 --panning ${out_w}x${out_h} --same-as eDP-1
