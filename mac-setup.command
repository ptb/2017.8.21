#!/bin/sh
# Initialize New Terminal

if test -z "${1}"; then
  osascript - "${0}" << EOF > /dev/null 2>&1
    on run { _this }
      tell app "Terminal" to do script "source " & quoted form of _this & " 0"
    end run
EOF
fi

# Define Function =ask=

ask () {
  osascript - "${1}" "${2}" "${3}" << EOF 2> /dev/null
    on run { _title, _action, _default }
      tell app "System Events" to return text returned of (display dialog _title with title _title buttons { "Cancel", _action } default answer _default)
    end run
EOF
}

# Define Function =run=

run () {
  osascript - "${1}" "${2}" "${3}" << EOF 2> /dev/null
    on run { _title, _cancel, _action }
      tell app "System Events" to return button returned of (display dialog _title with title _title buttons { _cancel, _action } cancel button 1 default button 2 giving up after 5)
    end run
EOF
}

# Define Function =init=

if test -n "${1}"; then
  init () {
    init_sudo
    init_no_sleep
    init_hostname
    init_devtools "/Volumes/Install"
    init_updates
  }
  printf "\n$(which init)\n"
fi

# Eliminate Prompts for Password

_sudo='timeout	Defaults:%admin timestamp_timeout=-1
tty_tickets	Defaults:%admin !tty_tickets'

init_sudo () {
  sudo -v

  while true; do
    sudo -n true
    sleep 60
  done &

  printf "%b\n" "${_sudo}" | \
  while IFS="$(printf '%b' '\t')" read filename policy; do
    printf "%b\n" "${policy}" | \
    sudo tee "/etc/sudoers.d/${filename}" > /dev/null
  done
}

# Set Defaults for Sleep

init_no_sleep () {
  sudo systemsetup -setcomputersleep "Never" > /dev/null
  sudo systemsetup -setharddisksleep "Never" > /dev/null
}

# Set Hostname from DNS

init_hostname () {
  h="$(hostname -s)"

  sudo systemsetup -setcomputername \
    "$(ruby -e "print '${h}'.capitalize")" > /dev/null
  sudo systemsetup -setlocalsubnetname "${h}" > /dev/null
}

# Install Developer Tools

init_devtools () {
  p="${1}/Command Line Tools (macOS High Sierra version 10.13).pkg"
  i="com.apple.pkg.CLTools_SDK_macOS1013"

  if test -f "${p}"; then
    if ! pkgutil --pkg-info "${i}" > /dev/null; then
      sudo installer -pkg "${p}" -target /
    fi
  else
    xcode-select --install
  fi
}

# Install macOS Updates

init_updates () {
  sudo softwareupdate --install --all
}

# Define Function =install_sw=

if test -n "${1}"; then
  install_sw () {
    install_brew
  }
  printf "\n$(which install_sw)\n"
fi

# Install Homebrew Package Manager

install_brew () {
  if ! which brew > /dev/null; then
    ruby -e \
      "$(curl -Ls 'https://github.com/Homebrew/install/raw/master/install')"
  fi
  brew analytics off
  brew update
  brew doctor
  brew tap "homebrew/bundle"
}

# Configure Z-Shell

config_zsh () {
  case $SHELL in
    (*zsh) ;;
    (*) chsh -s "$(which zsh)" ;;
  esac
}

config_zsh
