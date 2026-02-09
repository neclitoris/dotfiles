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

case "${mode[@]}" in
    "-m full")
        com="maim -u"
        ;;
    "-m window")
        com="maim -u -i $(xdotool getactivewindow)"
        ;;
    "-m region")
        maim -u | feh -FZ -&
        sleep .5
        com="maim -u -s"
        ;;
    *)
        echo "usage: screenshot.sh [-c] -m (full|window|region)"
        exit 1
        ;;
esac

sleep .1
eval $com $append
pkill -P $$
