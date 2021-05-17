#!/bin/bash

sel_icon() {
    if (( BRIGHTNESS > 75 )); then
        ICON="/usr/share/icons/Material-Black-Blueberry-Suru-GLOW/status/48/notification-display-brightness-full.svg"
    elif (( BRIGHTNESS > 50 )); then
        ICON="/usr/share/icons/Material-Black-Blueberry-Suru-GLOW/status/48/notification-display-brightness-high.svg"
    elif (( BRIGHTNESS > 25 )); then
        ICON="/usr/share/icons/Material-Black-Blueberry-Suru-GLOW/status/48/notification-display-brightness-medium.svg"
    elif (( BRIGHTNESS > 0 )); then
        ICON="/usr/share/icons/Material-Black-Blueberry-Suru-GLOW/status/48/notification-display-brightness-low.svg"
    else
        ICON="/usr/share/icons/Material-Black-Blueberry-Suru-GLOW/status/48/notification-display-brightness-off.svg"
    fi
}

handle() {
    case "$1" in
        button/lid)
            case "$3" in
                close)
                    xset dpms force off
                    ;;
                open)
                    xset dpms force on
                    ;;
            esac
            ;;
        button/volumeup)
            pactl set-sink-volume @DEFAULT_SINK@ +2%
            ;;
        button/volumedown)
            pactl set-sink-volume @DEFAULT_SINK@ -2%
            ;;
        button/mute)
            pactl set-sink-mute @DEFAULT_SINK@ toggle
            ;;
        video/brightnessup)
            BRIGHTNESS=$(backlight.hs update +5)
            sel_icon
            dunstify -i "$ICON" \
                -h string:x-canonical-private-synchronous:brightness \
                -t 500 'Current brightness' -h "int:value:$BRIGHTNESS"
            ;;
        video/brightnessdown)
            BRIGHTNESS=$(backlight.hs update -5)
            sel_icon
            dunstify -i "$ICON" \
                -h string:x-canonical-private-synchronous:brightness \
                -t 500 'Current brightness' -h "int:value:$BRIGHTNESS"
            ;;
    esac
}

coproc acpi_listen
trap 'kill $COPROC_PID' EXIT

while read -u "${COPROC[0]}" -a event; do
    handle "${event[@]}"
done

