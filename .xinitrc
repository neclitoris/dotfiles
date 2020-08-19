#!/usr/bin/zsh

setxkbmap us,ru -option 'grp:lalt_lshift_toggle'
xinput set-prop 'DELL07FA:00 06CB:7E92 Touchpad' 'libinput Natural Scrolling Enabled' 1
xsetroot -cursor_name left_ptr
[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources
picom -fcCz -m 1.0 -D 5 --shadow-exclude '!focused' &
feh --bg-max ~/image/Wallpapers/rope\ giant.png &
xhost +si:localuser:root
exec dbus-launch --exit-with-session xmonad
