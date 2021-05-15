#!/bin/zsh

killall pipewire

export PIPEWIRE_LOG="$HOME/.pipewire.log"

pipewire &
pipewire-media-session &
