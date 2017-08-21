#!/bin/sh
# Initialize New Terminal

if test -z $1; then
  case $SHELL in
    (*zsh) ;;
    (*) chsh -s "$(which zsh)" ;;
  esac

  osascript - $0 << EOF > /dev/null 2>&1
    on run { this }
      tell app "Terminal" to do script "source " & quoted form of this & " 0"
    end run
EOF
fi
