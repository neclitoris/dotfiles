#!/usr/bin/zsh

xrandr --setprovideroutputsource modesetting NVIDIA-0
xrandr --auto
xset dpms 0 0 0

setxkbmap us,ru -option 'grp:lalt_lshift_toggle'
xinput set-prop 'DELL07FA:00 06CB:7E92 Touchpad' 'libinput Natural Scrolling Enabled' 1
xsetroot -cursor_name left_ptr
xhost +si:localuser:root

[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources

picom --experimental-backends &
# feh --bg-max ~/image/Wallpapers/rope\ giant.png &
nitrogen --restore &

killall notification-manager
dunst &
stalonetray &

redshift &

eval $(ssh-agent -s)
exec dbus-launch --exit-with-session xmonad
