#!/bin/sh

. ~/.profile

nm-applet &
trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --widthtype request --transparent true --tint 0x191970 --heighttype request &

exec xmonad
