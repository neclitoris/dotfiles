#!/bin/zsh

zparseopts -- c=clip m:=mode

case "${clip[@]}" in
    -c)
        append=" | tee /tmp/img.png | xclip -target image/png -sel clipboard"
        ;;
    "")
        mkdir -p "$HOME/pic/Screenshots"
        append=" $(date +\"$HOME/pic/Screenshots/%FT%T.png\")"
        ;;
    *)
        echo "usage: screenshot.sh [-c] -m (full|window|region)"
        exit 1
        ;;
esac

find_geom() {
    MONITORS=$(xrandr | grep -o '[0-9]*x[0-9]*[+-][0-9]*[+-][0-9]*')
    # Get the location of the mouse
    XMOUSE=$(xdotool getmouselocation | awk -F "[: ]" '{print $2}')
    YMOUSE=$(xdotool getmouselocation | awk -F "[: ]" '{print $4}')

    echo $MONITORS | while IFS= read -r mon; do
      # Parse the geometry of the monitor
      MONW=$(echo ${mon} | awk -F "[x+]" '{print $1}')
      MONH=$(echo ${mon} | awk -F "[x+]" '{print $2}')
      MONX=$(echo ${mon} | awk -F "[x+]" '{print $3}')
      MONY=$(echo ${mon} | awk -F "[x+]" '{print $4}')
      # Use a simple collision check
      if (( XMOUSE >= MONX )); then
        if (( XMOUSE <= MONX+MONW )); then
          if (( YMOUSE >= MONY )); then
            if (( YMOUSE <= MONY+MONH )); then
              # We have found our monitor!
              echo "${MONW}x${MONH}+${MONX}+${MONY}"
              exit 0
            fi
          fi
        fi
      fi
    done
}

case "${mode[@]}" in
    "-m full")
        com="maim -u -g $(find_geom)"
        ;;
    "-m window")
        com="maim -u -i $(xdotool getactivewindow)"
        ;;
    "-m region")
        maim -u -g $(find_geom) | feh -FZ -&
        sleep .5
        com="maim -u -s"
        ;;
    *)
        echo "usage: screenshot.sh [-c] -m (full|window|region)"
        exit 1
        ;;
esac

sleep .1
dunstify "$com $append"
eval "$com $append"
pkill -P $$
