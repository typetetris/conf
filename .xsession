#!/bin/sh

. ~/.profile

xrdb -merge .Xresources
xrandr --output eDP-1 --auto #--dpi 282
blueman-applet &
nm-applet &
xscreensaver &
riot-desktop --hidden &
trayer \
        --edge top\
        --align right\
        --SetDockType true\
        --SetPartialStrut true\
        --expand true\
        --widthtype request\
        --transparent true\
        --heighttype pixel\
        --height 54 \
        --iconspacing 10 \
        --tint 0x191970 &

exec xmonad
