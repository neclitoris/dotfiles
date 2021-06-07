#!/bin/bash

sel_icon() {
    if (( $1 > 75 )); then
        ICON="/usr/share/icons/Material-Black-Blueberry-Suru/status/48/notification-display-brightness-full.svg"
    elif (( $1 > 50 )); then
        ICON="/usr/share/icons/Material-Black-Blueberry-Suru/status/48/notification-display-brightness-high.svg"
    elif (( $1 > 25 )); then
        ICON="/usr/share/icons/Material-Black-Blueberry-Suru/status/48/notification-display-brightness-medium.svg"
    elif (( $1 > 0 )); then
        ICON="/usr/share/icons/Material-Black-Blueberry-Suru/status/48/notification-display-brightness-low.svg"
    else
        ICON="/usr/share/icons/Material-Black-Blueberry-Suru/status/48/notification-display-brightness-off.svg"
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
            sel_icon $(backlight.hs update +5)
            dunstify -i "$ICON" -a "acpi_handler" \
                -t 500 -h "int:value:$(backlight.hs query)" \
                "Brightness"
            ;;
        video/brightnessdown)
            sel_icon $(backlight.hs update -5)
            dunstify -i "$ICON" -a "acpi_handler" \
                -t 500 -h "int:value:$(backlight.hs query)" \
                "Brightness"
            ;;
    esac
}

coproc acpi_listen
trap 'kill $COPROC_PID' EXIT

while read -u "${COPROC[0]}" -a event; do
    handle "${event[@]}"
done

