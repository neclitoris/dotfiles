#!/usr/bin/zsh

export GTK_THEME="Material-Black-Blueberry-Suru"

exec >~/.xsession-errors 2>&1

xrandr --setprovideroutputsource modesetting NVIDIA-0
xrandr --auto
xrandr --dpi 144
xset dpms 0 0 0

setxkbmap -layout us,ru -option grp:alt_shift_toggle
# xinput set-prop 'CUST0001:00 04F3:3147 Touchpad' 'libinput Natural Scrolling Enabled' 1
# xinput set-prop 'CUST0001:00 04F3:3147 Touchpad' 'libinput Tapping Enabled' 1
xsetroot -cursor_name left_ptr
xhost +si:localuser:root

[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources

picom --experimental-backends &
# feh --bg-max ~/image/Wallpapers/rope\ giant.png &
nitrogen --restore &

killall notification-manager
dunst &
stalonetray &

/usr/lib/geoclue-2.0/demos/agent &
redshift &

/usr/lib/xfce-polkit/xfce-polkit &

acpi_handler.sh&
xmonad
