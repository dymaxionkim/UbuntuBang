#!/bin/bash

cvt 1920 1080
xrandr --newmode "1920x1080_60" 173.00 1920 2040 2248 2576 1080 1081 1084 1118 -hsync +vsync
xrandr --addmode Virtual1 1920x1080_60
xrandr --output Virtual1 --mode 1920x1080_60
