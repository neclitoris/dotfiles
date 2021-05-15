#!/bin/zsh

recompile() {
    cp "$1" "$dir/f.hs"
    ghc "$1" -odir $dir -o "$dir/f.out" 1&>/dev/null
}

dir="/tmp/runghc-cached/$1"
mkdir -p $dir
touch "$dir/f.hs"
diff $1 "$dir/f.hs" 1&>/dev/null || recompile $@

eval "$dir/f.out" "${@:2}"
