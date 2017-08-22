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
    init_devtools "${CACHES}"
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
    install_brewfile_taps
    install_brewfile_brew_pkgs
    install_brewfile_cask_args
    install_brewfile_cask_pkgs
  }
  printf "\n$(which install_sw)\n"
fi

# Install Homebrew Package Manager

install_brew () {
  if ! which brew > /dev/null; then
    ruby -e \
      "$(curl -Ls 'https://github.com/Homebrew/install/raw/master/install')"
    printf "" > "${BREWFILE}"
  fi
  brew analytics off
  brew update
  brew doctor
  brew tap "homebrew/bundle"
}

# Add Homebrew Taps to Brewfile

_taps='caskroom/cask
caskroom/fonts
caskroom/versions
homebrew/bundle
homebrew/command-not-found
homebrew/nginx
homebrew/php
homebrew/services
ptb/custom
railwaycat/emacsmacport'

install_brewfile_taps () {
  printf "%b\n" "${_taps}" | \
  while IFS="$(printf '%b' '\t')" read tap; do
    printf 'tap "%s"\n' "${tap}" >> "${BREWFILE}"
  done
  printf "\n" >> "${BREWFILE}"
}

# Add Homebrew Packages to Brewfile

_pkgs='git
gnupg
mas
nodenv
openssl
perl-build
php71
pinentry-mac
plenv
pyenv
rbenv
rsync
shellcheck
vim
zsh'

install_brewfile_brew_pkgs () {
  printf "%b\n" "${_pkgs}" | \
  while IFS="$(printf '%b' '\t')" read pkg; do
    printf 'brew "%s"\n' "${pkg}" >> "${BREWFILE}"
  done
  printf "\n" >> "${BREWFILE}"
}

# Add Caskroom Options to Brewfile

_args='fontdir	/Library/Fonts
colorpickerdir	/Library/ColorPickers
input_methoddir	/Library/Input Methods
prefpanedir	/Library/PreferencePanes
qlplugindir	/Library/QuickLook
screen_saverdir	/Library/Screen Savers'

install_brewfile_cask_args () {
  printf 'cask_args \%s' "\n" >> "${BREWFILE}"
  printf "%b\n" "${_args}" | \
  while IFS="$(printf '%b' '\t')" read arg dir; do
    printf '  %s: "%s",\n' "${arg}" "${dir}" >> "${BREWFILE}"
  done
  sed -ie "$ s/,//" "${BREWFILE}"
  printf "\n" >> "${BREWFILE}"
}

# Add Homebrew Casks to Brewfile

_casks='java
xquartz
adium
alfred
arduino
atom
autodmg
bbedit
caffeine
carbon-copy-cloner
charles
dash
docker-toolbox
dropbox
duet
exifrenamer
firefox
flux
github-desktop
gitup
google-chrome
handbrake
hermes
imageoptim
inkscape
integrity
istat-menus
iterm2
jubler
little-snitch
machg
makemkv
menubar-countdown
meteorologist
moom
mp4tools
munki
musicbrainz-picard
namechanger
nvalt
nzbget
nzbvortex
openemu
opera
pacifist
platypus
plex-media-server
qlstephen
quitter
rescuetime
scrivener
sizeup
sketch
sketchup
skitch
skype
slack
sonarr
sonarr-menu
sourcetree
steermouse
subler
sublime-text
the-unarchiver
time-sink
torbrowser
tower
unrarx
vimr
vlc
vmware-fusion
wireshark
xld
caskroom/fonts/font-inconsolata-lgc
caskroom/fonts/font-skola-sans
railwaycat/emacsmacport/emacs-mac-spacemacs-icon'

install_brewfile_cask_pkgs () {
  printf "%b\n" "${_casks}" | \
  while IFS="$(printf '%b\n' '\t')" read cask; do
    printf 'cask "%s"\n' "${cask}" >> "${BREWFILE}"
  done
  printf "\n" >> "${BREWFILE}"
}

# Configure Z-Shell

config_zsh () {
  case $SHELL in
    (*zsh) ;;
    (*) chsh -s "$(which zsh)" ;;
  esac

  if test ! -f "/etc/zshenv"; then
    sudo tee /etc/zshenv << EOF > /dev/null
#!/bin/sh

export ZDOTDIR="${HOME}/.zsh"

if test -d "/Volumes/Caches"; then
  export CACHES="/Volumes/Caches"
  export HOMEBREW_CACHE="/Volumes/Caches/Homebrew"
  export BREWFILE="/Volumes/Caches/Homebrew/Brewfile"
else
  export CACHES="${HOME}/Library/Caches"
  export HOMEBREW_CACHE="${HOME}/Library/Caches/Homebrew"
  export BREWFILE="${HOME}/Library/Caches/Homebrew/Brewfile"
fi
EOF
  fi
}

config_zsh
