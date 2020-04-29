#!/bin/sh

. ~/.profile

xrdb -merge .Xresources
nm-applet &
blueman-applet &
xscreensaver &
trayer \
        --edge top\
        --align right\
        --SetDockType true\
        --SetPartialStrut true\
        --expand true\
        --widthtype request\
        --transparent true\
        --heighttype pixel\
        --height 36 \
        --iconspacing 10 \
        --tint 0x191970 &

exec xmonad
