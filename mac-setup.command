#!/bin/sh
# Quick Start

case "${SHELL}" in
  (*zsh) ;;
  (*) chsh -s "$(which zsh)"; exit 1 ;;
esac

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

init () {
  init_sudo
  init_cache
  init_no_sleep
  init_hostname
  init_perms
  init_mas_save
  init_devtools
  init_updates

  which install
}

if test "${1}" = 0; then
  printf "\n$(which init)\n"
fi

# Eliminate Prompts for Password

init_sudo () {
  printf "%s\n" "%wheel ALL=(ALL) NOPASSWD: ALL" | \
  sudo tee "/etc/sudoers.d/wheel" > /dev/null && \
  sudo dscl /Local/Default append /Groups/wheel GroupMembership "$(whoami)"
}

# Select Installation Cache Location

init_cache () {
  grep -q "CACHES" "/etc/zshenv" 2> /dev/null || \
  a=$(osascript << EOF 2> /dev/null
    on run
      return POSIX path of (choose folder with prompt "Select Installation Cache Location")
    end run
EOF
) && \
  test -d "${a}" || \
    a="${HOME}/Library/Caches/"

  grep -q "CACHES" "/etc/zshenv" 2> /dev/null || \
  printf "%s\n" \
    "export CACHES=\"${a}\"" \
    "export HOMEBREW_CACHE=\"${a}Homebrew\"" \
    "export BREWFILE=\"${a}Homebrew/Brewfile\"" | \
  sudo tee -a "/etc/zshenv" > /dev/null
  . "/etc/zshenv"

}

# Set Defaults for Sleep

init_no_sleep () {
  sudo pmset -a sleep 0
  sudo pmset -a disksleep 0
}

# Set Hostname from DNS

init_hostname () {
  sudo systemsetup -setcomputername \
    "$(ruby -e "print '$(hostname -s)'.capitalize")" > /dev/null
  sudo systemsetup -setlocalsubnetname "$(hostname -s)" > /dev/null
}

# Set Permissions on Install Destinations

_dest='/usr/local/bin
/Library/Fonts
/Library/ColorPickers
/Library/Input Methods
/Library/PreferencePanes
/Library/QuickLook
/Library/Screen Savers
/Library/User Pictures'

init_perms () {
  printf "%s\n" "${_dest}" | \
  while IFS="$(printf '\t')" read d; do
    test -d "${d}" || sudo mkdir -p "${d}"
    sudo chgrp -R admin "${d}"
    sudo chmod -R g+w "${d}"
  done
}

# Save Mac App Store Packages
# #+begin_example sh
# sudo lsof -c softwareupdated -F -r 2 | sed '/^n\//!d;/com.apple.SoftwareUpdate/!d;s/^n//'
# sudo lsof -c storedownloadd -F -r 2 | sed '/^n\//!d;/com.apple.appstore/!d;s/^n//'
# #+end_example

_mas_save_plist='add	:KeepAlive	bool	false
add	:Label	string	com.github.ptb.mas_save
add	:Program	string	/usr/local/bin/mas_save
add	:RunAtLoad	bool	true
add	:UserName	string	root'

init_mas_save () {
  sudo softwareupdate --reset-ignored > /dev/null

  cat << EOF > "/usr/local/bin/mas_save"
#!/bin/sh

asdir="/Library/Caches/storedownloadd"
as="\$(getconf DARWIN_USER_CACHE_DIR)com.apple.appstore"
sudir="/Library/Caches/softwareupdated"
su="\$(sudo find "/private/var/folders" -name "com.apple.SoftwareUpdate" -type d -user _softwareupdate 2> /dev/null)"

for i in 1 2 3 4 5; do
  mkdir -m a=rwxt -p "\${asdir}"
  find "\${as}" -iname "[0-9]*" -type d -print | \\
  while read a; do
    b="\${asdir}/\$(basename \$a)"
    mkdir -p "\${b}"
    find "\${a}" -type f -print | \\
    while read c; do
      d="\$(basename \$c)"
      test -e "\${b}/\${d}" || \\
        ln "\${c}" "\${b}/\${d}" && \\
        chmod 666 "\${b}/\${d}"
    done
  done

  mkdir -m a=rwxt -p "\${sudir}"
  find "\${su}" -name "*.tmp" -type f -print | \\
  while read a; do
    d="\$(basename \$a)"
    test -e "\${sudir}/\${d}.xar" ||
      ln "\${a}" "\${sudir}/\${d}.xar" && \\
      chmod 666 "\${sudir}/\${d}.xar"
  done

  sleep 1
done
EOF

  chmod a+x "/usr/local/bin/mas_save"
  rehash

  la="/Library/LaunchDaemons/com.github.ptb.mas_save"
  as="$(getconf DARWIN_USER_CACHE_DIR)com.apple.appstore"
  su="$(sudo find "/private/var/folders" -name "com.apple.SoftwareUpdate" -type d -user _softwareupdate 2> /dev/null)"

  sudo mkdir -p "$(dirname ${la})"
  sudo launchctl unload "${la}.plist" 2> /dev/null
  sudo rm -f "${la}.plist"
  config_defaults "$(printf '%s\t%s\t%s\t%s\t' ${la} 'WatchPaths' '-array-add' ${as})" "sudo"
  config_defaults "$(printf '%s\t%s\t%s\t%s\t' ${la} 'WatchPaths' '-array-add' ${su})" "sudo"
  config_plist "${_mas_save_plist}" "${la}.plist" "" "sudo"
  sudo plutil -convert xml1 "${la}.plist"
  sudo launchctl load "${la}.plist" 2> /dev/null
}

# Install Developer Tools

init_devtools () {
  p="${CACHES}/Command Line Tools (macOS High Sierra version 10.13).pkg"
  i="com.apple.pkg.CLTools_SDK_macOS1013"

  if test -f "${p}"; then
    if ! pkgutil --pkg-info "${i}" > /dev/null 2>&1; then
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

# Define Function =install=

install () {
  install_paths
  install_brew
  install_brewfile_taps
  install_brewfile_brew_pkgs
  install_brewfile_cask_args
  install_brewfile_cask_pkgs
  install_brewfile_mas_apps
  install_brew_bundle
  install_links
  install_node_sw
  install_perl_sw
  install_python_sw
  install_ruby_sw

  which config
}

# Add =/usr/local/bin/sbin= to Default Path

install_paths () {
  if ! grep -Fq "/usr/local/sbin" /etc/paths; then
    sudo sed -i -e "/\/usr\/sbin/{x;s/$/\/usr\/local\/sbin/;G;}" /etc/paths
  fi
}

# Install Homebrew Package Manager

install_brew () {
  if ! which brew > /dev/null; then
    ruby -e \
      "$(curl -Ls 'https://github.com/Homebrew/install/raw/master/install')" \
      < /dev/null > /dev/null 2>&1
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
  printf "%s\n" "${_taps}" | \
  while IFS="$(printf '\t')" read tap; do
    printf 'tap "%s"\n' "${tap}" >> "${BREWFILE}"
  done
  printf "\n" >> "${BREWFILE}"
}

# Add Homebrew Packages to Brewfile

_pkgs='aspell
chromedriver
coreutils
duti
fasd
fdupes
gawk
getmail
git
git-flow
git-lfs
gnu-sed
gnupg
gpac
httpie
hub
ievms
imagemagick
mas
mercurial
mp4v2
mtr
nmap
node
nodenv
openssl
p7zip
perl-build
php71
pinentry-mac
plenv
pyenv
rbenv
rsync
selenium-server-standalone
shellcheck
sqlite
stow
terminal-notifier
trash
unrar
vim
yarn
youtube-dl
zsh
ptb/custom/dovecot
ptb/custom/ffmpeg
ptb/custom/nginx-full'

install_brewfile_brew_pkgs () {
  printf "%s\n" "${_pkgs}" | \
  while IFS="$(printf '\t')" read pkg; do
    printf 'brew "%s", args: [ "force-bottle" ]\n' "${pkg}" >> "${BREWFILE}"
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
  printf 'cask_args \' >> "${BREWFILE}"
  printf "%s\n" "${_args}" | \
  while IFS="$(printf '\t')" read arg dir; do
    printf '\n  %s: "%s",' "${arg}" "${dir}" >> "${BREWFILE}"
  done
  sed -i -e "$ s/,/$(printf '\n\n\n')/" "${BREWFILE}"
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
caskroom/versions/transmit4
ptb/custom/adobe-creative-cloud-2014
ptb/custom/blankscreen
ptb/custom/composer
ptb/custom/ipmenulet
ptb/custom/pcalc-3
ptb/custom/sketchup-pro
ptb/custom/synergy
railwaycat/emacsmacport/emacs-mac-spacemacs-icon'

install_brewfile_cask_pkgs () {
  printf "%s\n" "${_casks}" | \
  while IFS="$(printf '\t')" read cask; do
    printf 'cask "%s"\n' "${cask}" >> "${BREWFILE}"
  done
  printf "\n" >> "${BREWFILE}"
}

# Add App Store Packages to Brewfile

_mas='1Password	443987910
autoping	632347870
Coffitivity	659901392
Growl	467939042
HardwareGrowler	475260933
I Love Stars	402642760
Icon Slate	439697913
Justnotes	511230166
Keynote	409183694
Metanota Pro	515250764
Numbers	409203825
Pages	409201541
WiFi Explorer	494803304'

install_brewfile_mas_apps () {
  open "/Applications/App Store.app"
  run "Sign in to the App Store with your Apple ID" "Cancel" "OK"

  MASDIR="$(getconf DARWIN_USER_CACHE_DIR)com.apple.appstore"
  sudo chown -R "$(whoami)" "${MASDIR}"
  rsync -a --delay-updates \
    "${CACHES}/storedownloadd/" "${MASDIR}/"

  printf "%s\n" "${_mas}" | \
  while IFS="$(printf '\t')" read app id; do
    printf 'mas "%s", id: %s\n' "${app}" "${id}" >> "${BREWFILE}"
  done
}

# Install macOS Software with =brew bundle=

install_brew_bundle () {
  brew bundle --file="${BREWFILE}"

  x="$(find '/Applications' -maxdepth 1 -name 'Xcode[^ ]*.app' -print -quit)"
  if test -n "${x}"; then
    sudo xcode-select -s "${x}"
    sudo xcodebuild -license accept
  fi
}

# Link System Utilities to Applications

_links='/System/Library/CoreServices/Applications
/Applications/Xcode.app/Contents/Applications
/Applications/Xcode.app/Contents/Developer/Applications
/Applications/Xcode-beta.app/Contents/Applications
/Applications/Xcode-beta.app/Contents/Developer/Applications'

install_links () {
  brew linkapps 2> /dev/null
  printf "%s\n" "${_links}" | \
  while IFS="$(printf '\t')" read link; do
    find "${link}" -maxdepth 1 -name "*.app" -type d -print0 2> /dev/null | \
    xargs -0 -I {} -L 1 ln -s "{}" "/Applications" 2> /dev/null
  done
}

# Install Node Software with =nodenv=

install_node_sw () {
  if which nodenv > /dev/null; then
    sudo mkdir -p "/usr/local/node"
    sudo chown -R "$(whoami):admin" "/usr/local/node"
    test -f "/etc/zshenv" && \
    grep -q "NODENV_ROOT" "/etc/zshenv" || \
    printf "%s\n" \
      'export NODENV_ROOT="/usr/local/node"' | \
    sudo tee -a "/etc/zshenv" > /dev/null
    . "/etc/zshenv"

    test -f "/etc/zshrc" && \
    grep -q "nodenv" "/etc/zshrc" || \
    printf "%s\n" \
      'eval "$(nodenv init - zsh)"' | \
    sudo tee -a "/etc/zshrc" > /dev/null
    . "/etc/zshrc"

    nodenv install --skip-existing 8.3.0
    nodenv global 8.3.0
    rehash

    grep -q "${NODENV_ROOT}" "/etc/paths" || \
    sudo sed -i -e "1i\\
${NODENV_ROOT}/shims
" "/etc/paths"
  fi
}

# Install Perl Software with =plenv=

install_perl_sw () {
  if which plenv > /dev/null; then
    sudo mkdir -p "/usr/local/perl"
    sudo chown -R "$(whoami):admin" "/usr/local/perl"
    test -f "/etc/zshenv" && \
    grep -q "PLENV_ROOT" "/etc/zshenv" || \
    printf "%s\n" \
      'export PLENV_ROOT="/usr/local/perl"' | \
    sudo tee -a "/etc/zshenv" > /dev/null
    . "/etc/zshenv"

    test -f "/etc/zshrc" && \
    grep -q "plenv" "/etc/zshrc" || \
    printf "%s\n" \
      'eval "$(plenv init - zsh)"' | \
    sudo tee -a "/etc/zshrc" > /dev/null
    . "/etc/zshrc"

    plenv install 5.26.0 > /dev/null 2>&1
    plenv global 5.26.0
    rehash

    grep -q "${PLENV_ROOT}" "/etc/paths" || \
    sudo sed -i -e "1i\\
${PLENV_ROOT}/shims
" "/etc/paths"
  fi
}

# Install Python Software with =pyenv=

install_python_sw () {
  if which pyenv > /dev/null; then
    CFLAGS="-I$(brew --prefix openssl)/include" && export CFLAGS
    LDFLAGS="-L$(brew --prefix openssl)/lib" && export LDFLAGS

    sudo mkdir -p "/usr/local/python"
    sudo chown -R "$(whoami):admin" "/usr/local/python"
    test -f "/etc/zshenv" && \
    grep -q "PYENV_ROOT" "/etc/zshenv" || \
    printf "%s\n" \
      'export PYENV_ROOT="/usr/local/python"' | \
    sudo tee -a "/etc/zshenv" > /dev/null
    . "/etc/zshenv"

    test -f "/etc/zshrc" && \
    grep -q "pyenv" "/etc/zshrc" || \
    printf "%s\n" \
      'eval "$(pyenv init - zsh)"' | \
    sudo tee -a "/etc/zshrc" > /dev/null
    . "/etc/zshrc"

    pyenv install --skip-existing 2.7.13
    pyenv install --skip-existing 3.6.2
    pyenv global 2.7.13
    rehash

    pip install --upgrade "pip" "setuptools"

    grep -q "${PYENV_ROOT}" "/etc/paths" || \
    sudo sed -i -e "1i\\
${PYENV_ROOT}/shims
" "/etc/paths"
  fi
}

# Install Ruby Software with =rbenv=

install_ruby_sw () {
  if which rbenv > /dev/null; then
    sudo mkdir -p "/usr/local/ruby"
    sudo chown -R "$(whoami):admin" "/usr/local/ruby"
    test -f "/etc/zshenv" && \
    grep -q "RBENV_ROOT" "/etc/zshenv" || \
    printf "%s\n" \
      'export RBENV_ROOT="/usr/local/ruby"' | \
    sudo tee -a "/etc/zshenv" > /dev/null
    . "/etc/zshenv"

    test -f "/etc/zshrc" && \
    grep -q "rbenv" "/etc/zshrc" || \
    printf "%s\n" \
      'eval "$(rbenv init - zsh)"' | \
    sudo tee -a "/etc/zshrc" > /dev/null
    . "/etc/zshrc"

    rbenv install --skip-existing 2.4.1
    rbenv global 2.4.1
    rehash

    printf "%s\n" \
      "gem: --no-document" | \
    tee "${HOME}/.gemrc" > /dev/null

    gem update --system
    yes | gem update
    gem install bundler

    grep -q "${RBENV_ROOT}" "/etc/paths" || \
    sudo sed -i -e "1i\\
${RBENV_ROOT}/shims
" "/etc/paths"
  fi
}

# Define Function =config=

config () {
  config_bbedit
  config_desktop
  config_dovecot
  config_emacs
  config_zsh
  config_new_account
  config_rm_sudoers

  which custom
}

# Define Function =config_defaults=

config_defaults () {
  printf "%s\n" "${1}" | \
  while IFS="$(printf '\t')" read domain key type value host; do
    ${2} defaults ${host} write ${domain} "${key}" ${type} "${value}"
  done
}

# Define Function =config_plist=

config_plist () {
  printf "%s\n" "${1}" | \
  while IFS="$(printf '\t')" read command entry type value; do
    ${4} /usr/libexec/PlistBuddy "${2}" \
      -c "${command} '${3}${entry}' ${type} '${value}'" 2> /dev/null
  done
}

# Configure BBEdit

config_bbedit () {
  if test -d "/Applications/BBEdit.app"; then
    test -f "/usr/local/bin/bbdiff" || \
    ln /Applications/BBEdit.app/Contents/Helpers/bbdiff /usr/local/bin/bbdiff && \
    ln /Applications/BBEdit.app/Contents/Helpers/bbedit_tool /usr/local/bin/bbedit && \
    ln /Applications/BBEdit.app/Contents/Helpers/bbfind /usr/local/bin/bbfind && \
    ln /Applications/BBEdit.app/Contents/Helpers/bbresults /usr/local/bin/bbresults
  fi
}

# Configure Default Apps

config_default_apps () {
  true
}

# Configure Desktop Picture

config_desktop () {
  sudo rm -f "/Library/Caches/com.apple.desktop.admin.png"

  base64 -D << EOF > "/Library/Caches/com.apple.desktop.admin.png"
iVBORw0KGgoAAAANSUhEUgAAAIAAAACAAQAAAADrRVxmAAAAGElEQVR4AWOgMxgFo2AUjIJRMApGwSgAAAiAAAH3bJXBAAAAAElFTkSuQmCC
EOF
}

# Configure Dovecot

config_dovecot () {
  if which /usr/local/sbin/dovecot > /dev/null; then
    if ! run "Configure Dovecot Email Server?" "Configure Server" "Cancel"; then
      cat << EOF > "/usr/local/etc/dovecot/dovecot.conf"
auth_mechanisms = cram-md5
default_internal_user = _dovecot
default_login_user = _dovenull
log_path = /dev/stderr
mail_location = maildir:~/.mail:INBOX=~/.mail/Inbox:LAYOUT=fs
mail_plugins = zlib
maildir_copy_with_hardlinks = no
namespace {
  inbox = yes
  mailbox Drafts {
    auto = subscribe
    special_use = \Drafts
  }
  mailbox Junk {
    auto = subscribe
    special_use = \Junk
  }
  mailbox Sent {
    auto = subscribe
    special_use = \Sent
  }
  mailbox "Sent Messages" {
    special_use = \Sent
  }
  mailbox Trash {
    auto = subscribe
    special_use = \Trash
  }
  separator = .
  type = private
}
passdb {
  args = scheme=cram-md5 /usr/local/etc/dovecot/cram-md5.pwd
  driver = passwd-file

  # driver = pam

  # args = nopassword=y
  # driver = static
}
plugin {
  sieve = file:/Users/%u/.sieve
  sieve_plugins = sieve_extprograms
  zlib_save = bz2
  zlib_save_level = 9
}
protocols = imap
service imap-login {
  inet_listener imap {
    port = 0
  }
}
ssl = required
ssl_cipher_list = AES128+EECDH:AES128+EDH
ssl_dh_parameters_length = 4096
ssl_prefer_server_ciphers = yes
ssl_protocols = !SSLv2 !SSLv3
userdb {
  driver = passwd
}
protocol lda {
  mail_plugins = sieve zlib
}

# auth_debug = yes
# auth_debug_passwords = yes
# auth_verbose = yes
# auth_verbose_passwords = plain
# mail_debug = yes
# verbose_ssl = yes
EOF

      MAILADM="$(ask 'Email Administrator Address' 'Set Email' "$(whoami)@$(hostname)")"
      MAILSVR="$(ask 'Email Server DNS Hostname' 'Set Hostname' "$(hostname)")"
      SSL="$(brew --prefix openssl)"
      printf "%s\n" \
        "postmaster_address = '${MAILADM}'" \
        "ssl_cert = <${SSL}/certs/${MAILSVR}/${MAILSVR}.crt" \
        "ssl_key = <${SSL}/certs/${MAILSVR}/${MAILSVR}.key" | \
      tee -a "/usr/local/etc/dovecot/dovecot.conf" > /dev/null

      if test ! -f "/usr/local/etc/dovecot/cram-md5.pwd"; then
        while true; do
          MAILUSR="$(ask 'Username for New Email Account?' 'Create Account' "$(whoami)")"
          test -n "${MAILUSR}" || break
          doveadm pw | \
          sed -e "s/^/${MAILUSR}:/" | \
          sudo tee -a "/usr/local/etc/dovecot/cram-md5.pwd"
        done
        sudo chown _dovecot "/usr/local/etc/dovecot/cram-md5.pwd"
        sudo chmod go= "/usr/local/etc/dovecot/cram-md5.pwd"
      fi

      sudo tee "/etc/pam.d/dovecot" << EOF > /dev/null
auth	required	pam_opendirectory.so	try_first_pass
account	required	pam_nologin.so
account	required	pam_opendirectory.so
password	required	pam_opendirectory.so
EOF

      grep -Fq "${MAILSVR}" "/etc/hosts" || \
      printf "%s\t%s\n" "127.0.0.1" "${MAILSVR}" | \
      sudo tee -a "/etc/hosts" > /dev/null

      sudo brew services start dovecot

      cat << EOF > "/usr/local/bin/imaptimefix.py"
#!/usr/bin/env python

# Author: Zachary Cutlip <@zcutlip>
# http://shadow-file.blogspot.com/2012/06/parsing-email-and-fixing-timestamps-in.html
# Updated: Peter T Bosse II <@ptb>
# Purpose: A program to fix sorting of mail messages that have been POPed or
#          IMAPed in the wrong order. Compares time stamp sent and timestamp
#          received on an RFC822-formatted email message, and renames the
#          message file using the most recent timestamp that is no more than
#          24 hours after the date sent. Updates the file's atime/mtime with
#          the timestamp, as well. Does not modify the headers or contents of
#          the message.

from bz2 import BZ2File
from email import message_from_string
from email.utils import mktime_tz, parsedate_tz
from os import rename, utime, walk
from os.path import abspath, isdir, isfile, join
from re import compile, match
from sys import argv

if isdir(argv[1]):
  e = compile("([0-9]+)(\..*$)")

  for a, b, c in walk(argv[1]):
    for d in c:
      if e.match(d):
        f = message_from_string(BZ2File(join(a, d)).read())
        g = mktime_tz(parsedate_tz(f.get("Date")))

        h = 0
        for i in f.get_all("Received", []):
          j = i.split(";")[-1]
          if parsedate_tz(j):
            k = mktime_tz(parsedate_tz(j))
            if (k - g) > (60*60*24):
              continue

            h = k
          break

        if (h < 1):
          h = g

        l = e.match(d)

        if len(l.groups()) == 2:
          m = str(int(h)) + l.groups()[1]
          if not isfile(join(a, m)):
            rename(join(a, d), join(a, m))
          utime(join(a, m), (h, h))
EOF
      chmod +x /usr/local/bin/imaptimefix.py
    fi
  fi
}

# Configure Emacs

config_emacs () {
  test -f "/usr/local/bin/vi" || \
  cat << EOF > "/usr/local/bin/vi"
#!/bin/sh

if [ -e "/Applications/Emacs.app" ]; then
  t=()

  if [ \${#@} -ne 0 ]; then
    while IFS= read -r file; do
      [ ! -f "\$file" ] && t+=("\$file") && /usr/bin/touch "\$file"
      file=\$(echo \$(cd \$(dirname "\$file") && pwd -P)/\$(basename "\$file"))
      \$(/usr/bin/osascript <<-END
        if application "Emacs.app" is running then
          tell application id (id of application "Emacs.app") to open POSIX file "\$file"
        else
          tell application ((path to applications folder as text) & "Emacs.app")
            activate
            open POSIX file "\$file"
          end tell
        end if
END
        ) &  # Note: END on the previous line may be indented with tabs but not spaces
    done <<<"\$(printf '%s\n' "\$@")"
  fi

  if [ ! -z "\$t" ]; then
    \$(/bin/sleep 10; for file in "\${t[@]}"; do
      [ ! -s "\$file" ] && /bin/rm "\$file";
    done) &
  fi
else
  vim -No "\$@"
fi
EOF

  chmod a+x /usr/local/bin/vi
  rehash
}

# Configure Login Window

_loginwindow='/Library/Preferences/com.apple.loginwindow
SHOWFULLNAME
-bool
true
'

config_loginwindow () {
  config_defaults "${_loginwindow}" "sudo"
}

# Configure OpenSSL

config_openssl () {
  true
}

# Configure App Store

_swupdate='/Library/Preferences/com.apple.commerce	AutoUpdate	-bool	true	
/Library/Preferences/com.apple.commerce	AutoUpdateRestartRequired	-bool	true	'

config_mas () {
  config_defaults "${_swupdate}" "sudo"
}

# Configure Guest Users

config_guest () {
  sudo sysadminctl -guestAccount off
}

# Configure Z-Shell

config_zsh () {
  grep -q "ZDOTDIR" "/etc/zshenv" || \
  sudo tee -a /etc/zshenv << EOF > /dev/null
export ZDOTDIR="\${HOME}/.zsh"
export MASDIR="\$(getconf DARWIN_USER_CACHE_DIR)com.apple.appstore"

export EDITOR="vi"
export VISUAL="vi"
export PAGER="less"

test -z "\${LANG}" && \\
  export LANG="en_US.UTF-8"

# Ensure path arrays do not contain duplicates.
typeset -gU cdpath fpath mailpath path

# Set the default Less options.
export LESS="-egiMQRS -x2 -z-2"
EOF
  sudo chmod +x "/etc/zshenv"
  . "/etc/zshenv"
}

# Configure New Account

config_new_account () {
  e="$(ask 'New Account Email Address' 'OK' '')"
  curl --output "/Library/User Pictures/${e}.jpg" --silent \
    "https://www.gravatar.com/avatar/$(md5 -qs ${e}).jpg?s=512"

  g="$(curl --location --silent \
    "https://api.github.com/search/users?q=${e}" | \
    sed -n 's/^.*"url": "\(.*\)".*/\1/p')"
  g="$(curl --location --silent ${g})"

  n="$(printf ${g} | sed -n 's/^.*"name": "\(.*\)".*/\1/p')"
  n="$(ask 'New Account Real Name' 'OK' ${n})"

  u="$(printf ${g} | sed -n 's/^.*"login": "\(.*\)".*/\1/p')"
  u="$(ask 'New Account User Name' 'OK' ${u})"

  sudo defaults write \
    "/System/Library/User Template/Non_localized/Library/Preferences/.GlobalPreferences.plist" \
    "com.apple.swipescrolldirection" -bool false

  sudo sysadminctl -addUser "${u}" -fullName "${n}" -password - \
    -shell "$(which zsh)" -picture "/Library/User Pictures/${e}.jpg"
}

# Reinstate =sudo= Password

config_rm_sudoers () {
  sudo dscl /Local/Default -delete /Groups/wheel GroupMembership "$(whoami)"
  sudo rm -f "/etc/sudoers.d/wheel"

  if run "Log Out Then Log Back In?" "Cancel" "Log Out"; then
    osascript -e 'tell app "loginwindow" to «event aevtrlgo»'
  fi
}

# Define Function =custom=

custom () {
  custom_home
  custom_atom
  custom_emacs
  custom_terminal
  custom_zsh
}

# Customize Home

custom_home () {
  git -C "${HOME}" init

  a=$(ask "Existing Home Repository Path or URL" "Add Remote" "")
  if test -n "${a}"; then
    git -C "${HOME}" remote add origin "${a}"
    git -C "${HOME}" pull origin master
  fi

  chmod -R go= "${HOME}" > /dev/null 2>&1
}

# Customize Atom

_atom='atom-beautify
atom-css-comb
atom-jade
atom-wallaby
autoclose-html
autocomplete-python
busy-signal
double-tag
editorconfig
ex-mode
file-icons
git-plus
git-time-machine
highlight-selected
intentions
language-docker
language-jade
language-javascript-jsx
language-lisp
language-slim
linter
linter-eslint
linter-rubocop
linter-shellcheck
linter-ui-default
MagicPython
python-yapf
react
riot
sort-lines
term3
tomorrow-night-eighties-syntax
tree-view-open-files
vim-mode-plus
vim-mode-zz'

custom_atom () {
  if which apm > /dev/null; then
    mkdir -p "${HOME}/.atom/.apm"

    cat << EOF > "${HOME}/.atom/.apmrc"
cache = ${CACHES}/apm
EOF

    cat << EOF > "${HOME}/.atom/.apm/.apmrc"
cache = ${CACHES}/apm
EOF

    printf "%s\n" "${_atom}" | \
    while IFS="$(printf '\t')" read pkg; do
      test -d "${HOME}/.atom/packages/${pkg}" ||
      apm install "${pkg}"
    done

    cat << EOF > "${HOME}/.atom/config.cson"
"*":
  "autocomplete-python":
    useKite: false
  core:
    telemetryConsent: "limited"
    themes: [
      "one-dark-ui"
      "tomorrow-night-eighties-syntax"
    ]
  editor:
    fontFamily: "Inconsolata LGC"
    fontSize: 13
  welcome:
    showOnStartup: false
EOF

    cat << EOF > "${HOME}/.atom/packages/tomorrow-night-eighties-syntax/styles/colors.less"
@background: #222222;
@current-line: #333333;
@selection: #4c4c4c;
@foreground: #cccccc;
@comment: #999999;
@red: #f27f7f;
@orange: #ff994c;
@yellow: #ffcc66;
@green: #99cc99;
@aqua: #66cccc;
@blue: #6699cc;
@purple: #cc99cc;
EOF
  fi
}

# Customize autoping

_autoping='com.memset.autoping	Hostname	-string	google.com	
com.memset.autoping	SlowPingLowThreshold	-int	100	
com.memset.autoping	LaunchAtLogin	-bool	true	
com.memset.autoping	ShowIcon	-bool	true	
com.memset.autoping	ShowText	-bool	true	
com.memset.autoping	ShowPacketLossText	-bool	true	
com.memset.autoping	ShowNotifications	-bool	true	'

custom_autoping () {
  config_defaults "${_autoping}"
}

# Customize Desktop Picture

custom_desktop () {
  osascript - "${1}" << EOF 2> /dev/null
    on run { _this }
      tell application "System Events"
        set a to POSIX file quoted form of _this
        set b to a reference to every desktop
        repeat with c in b
          set picture of c to a
        end repeat
      end tell
    end run
EOF
}

# Customize Dock

_dock='Metanota Pro
Mail
Safari
Messages
Emacs
Atom
Utilities/Terminal
System Preferences
PCalc
Hermes
iTunes
VLC'

custom_dock () {
  defaults write com.apple.dock "autohide-delay" -float 0
  defaults write com.apple.dock "autohide-time-modifier" -float 0.5

  defaults delete com.apple.dock "persistent-apps"

  printf "%s\n" "${_dock}" | \
  while IFS="$(printf '\t')" read app; do
    if test -e "/Applications/${app}.app"; then
      defaults write com.apple.dock "persistent-apps" -array-add \
        "<dict><key>tile-data</key><dict><key>file-data</key><dict><key>_CFURLString</key><string>/Applications/${app}.app/</string><key>_CFURLStringType</key><integer>0</integer></dict></dict></dict>"
    fi
  done

  defaults delete com.apple.dock "persistent-others"

  osascript -e 'tell app "Dock" to quit'
}

# Customize Emacs

custom_emacs () {
  mkdir -p "${HOME}/.emacs.d" && \
  curl --compressed --location --silent \
    "https://github.com/syl20bnr/spacemacs/archive/master.tar.gz" | \
  tar -C "${HOME}/.emacs.d" --strip-components 1 -xf -
  mkdir -p "${HOME}/.emacs.d/private/ptb"
  chmod -R go= "${HOME}/.emacs.d"

  cat << EOF > "${HOME}/.spacemacs"
(defun dotspacemacs/layers ()
  (setq-default
    dotspacemacs-configuration-layers '(
      auto-completion
      (colors :variables
        colors-colorize-identifiers 'variables)
      dash
      deft
      docker
      emacs-lisp
      evil-cleverparens
      git
      github
      helm
      html
      ibuffer
      imenu-list
      javascript
      markdown
      nginx
      (org :variables
        org-enable-github-support t)
      (osx :variables
        osx-use-option-as-meta nil)
      ptb
      react
      ruby
      ruby-on-rails
      search-engine
      semantic
      shell-scripts
      (spell-checking :variables
        spell-checking-enable-by-default nil)
      syntax-checking
      (version-control :variables
        version-control-diff-side 'left)
      vim-empty-lines
    )
    dotspacemacs-excluded-packages '(org-bullets)
  )
)

(defun dotspacemacs/init ()
  (setq-default
    dotspacemacs-startup-banner nil
    dotspacemacs-startup-lists nil
    dotspacemacs-scratch-mode 'org-mode
    dotspacemacs-themes '(sanityinc-tomorrow-eighties)
    dotspacemacs-default-font '(
      "Inconsolata LGC"
      :size 13
      :weight normal
      :width normal
      :powerline-scale 1.1)
    dotspacemacs-loading-progress-bar nil
    dotspacemacs-active-transparency 100
    dotspacemacs-inactive-transparency 100
    dotspacemacs-line-numbers t
    dotspacemacs-whitespace-cleanup 'all
  )
)

(defun dotspacemacs/user-init ())
(defun dotspacemacs/user-config ())
EOF

  cat << EOF > "${HOME}/.emacs.d/private/ptb/config.el"
(setq
  default-frame-alist '(
    (top . 22)
    (left . 1279)
    (height . 48)
    (width . 123)
    (vertical-scroll-bars . right))
  initial-frame-alist (copy-alist default-frame-alist)

  deft-directory "~/Dropbox/Notes"
  focus-follows-mouse t
  mouse-wheel-follow-mouse t
  mouse-wheel-scroll-amount '(1 ((shift) . 1))
  org-src-preserve-indentation t
  purpose-display-at-right 20
  recentf-max-saved-items 5
  scroll-step 1
  system-uses-terminfo nil

  ibuffer-formats '(
    (mark modified read-only " "
    (name 18 18 :left :elide)))

  ibuffer-shrink-to-minimum-size t
  ibuffer-always-show-last-buffer nil
  ibuffer-sorting-mode 'recency
  ibuffer-use-header-line nil
  x-select-enable-clipboard nil)

(global-linum-mode t)
(recentf-mode t)
(x-focus-frame nil)
(with-eval-after-load 'org
  (org-babel-do-load-languages
    'org-babel-load-languages '(
      (ruby . t)
      (shell . t)
    )
  )
)
EOF

  cat << EOF > "${HOME}/.emacs.d/private/ptb/funcs.el"
(defun is-useless-buffer (buffer)
  (let ((name (buffer-name buffer)))
    (and (= ?* (aref name 0))
        (string-match "^\\**" name))))

(defun kill-useless-buffers ()
  (interactive)
  (loop for buffer being the buffers
        do (and (is-useless-buffer buffer) (kill-buffer buffer))))

(defun org-babel-tangle-hook ()
  (add-hook 'after-save-hook 'org-babel-tangle))

(add-hook 'org-mode-hook #'org-babel-tangle-hook)

(defun ptb/new-untitled-buffer ()
  "Create a new untitled buffer in the current frame."
  (interactive)
  (let
    ((buffer "Untitled-") (count 1))
    (while
      (get-buffer (concat buffer (number-to-string count)))
      (setq count (1+ count)))
    (switch-to-buffer
    (concat buffer (number-to-string count))))
  (org-mode))

(defun ptb/previous-buffer ()
  (interactive)
  (kill-useless-buffers)
  (previous-buffer))

(defun ptb/next-buffer ()
  (interactive)
  (kill-useless-buffers)
  (next-buffer))

(defun ptb/kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer))
  (kill-useless-buffers))
EOF

  cat << EOF > "${HOME}/.emacs.d/private/ptb/keybindings.el"
(define-key evil-insert-state-map (kbd "<return>") 'newline)

(define-key evil-normal-state-map (kbd "s-c") 'clipboard-kill-ring-save)
(define-key evil-insert-state-map (kbd "s-c") 'clipboard-kill-ring-save)
(define-key evil-visual-state-map (kbd "s-c") 'clipboard-kill-ring-save)

(define-key evil-ex-completion-map (kbd "s-v") 'clipboard-yank)
(define-key evil-ex-search-keymap (kbd "s-v") 'clipboard-yank)
(define-key evil-insert-state-map (kbd "s-v") 'clipboard-yank)

(define-key evil-normal-state-map (kbd "s-x") 'clipboard-kill-region)
(define-key evil-insert-state-map (kbd "s-x") 'clipboard-kill-region)
(define-key evil-visual-state-map (kbd "s-x") 'clipboard-kill-region)

(define-key evil-normal-state-map (kbd "<S-up>") 'evil-previous-visual-line)
(define-key evil-insert-state-map (kbd "<S-up>") 'evil-previous-visual-line)
(define-key evil-visual-state-map (kbd "<S-up>") 'evil-previous-visual-line)

(define-key evil-normal-state-map (kbd "<S-down>") 'evil-next-visual-line)
(define-key evil-insert-state-map (kbd "<S-down>") 'evil-next-visual-line)
(define-key evil-visual-state-map (kbd "<S-down>") 'evil-next-visual-line)

(global-set-key (kbd "C-l") 'evil-search-highlight-persist-remove-all)

(global-set-key (kbd "s-t") 'make-frame)
(global-set-key (kbd "s-n") 'ptb/new-untitled-buffer)
(global-set-key (kbd "s-w") 'ptb/kill-this-buffer)
(global-set-key (kbd "s-{") 'ptb/previous-buffer)
(global-set-key (kbd "s-}") 'ptb/next-buffer)
EOF

  cat << EOF > "${HOME}/.emacs.d/private/ptb/packages.el"
(setq ptb-packages '(adaptive-wrap auto-indent-mode))

(defun ptb/init-adaptive-wrap ()
  "Load the adaptive wrap package"
  (use-package adaptive-wrap
    :init
    (setq adaptive-wrap-extra-indent 2)
    :config
    (progn
      ;; http://stackoverflow.com/questions/13559061
      (when (fboundp 'adaptive-wrap-prefix-mode)
        (defun ptb/activate-adaptive-wrap-prefix-mode ()
          "Toggle 'visual-line-mode' and 'adaptive-wrap-prefix-mode' simultaneously."
          (adaptive-wrap-prefix-mode (if visual-line-mode 1 -1)))
        (add-hook 'visual-line-mode-hook 'ptb/activate-adaptive-wrap-prefix-mode)))))

(defun ptb/init-auto-indent-mode ()
  (use-package auto-indent-mode
    :init
    (setq
      auto-indent-delete-backward-char t
      auto-indent-fix-org-auto-fill t
      auto-indent-fix-org-move-beginning-of-line t
      auto-indent-fix-org-return t
      auto-indent-fix-org-yank t
      auto-indent-start-org-indent t
    )
  )
)
EOF
}

# Customize Finder

_finder='com.apple.finder	ShowHardDrivesOnDesktop	-bool	false	
com.apple.finder	ShowExternalHardDrivesOnDesktop	-bool	false	
com.apple.finder	ShowRemovableMediaOnDesktop	-bool	true	
com.apple.finder	ShowMountedServersOnDesktop	-bool	true	
com.apple.finder	NewWindowTarget	-string	PfHm	
com.apple.finder	NewWindowTargetPath	-string	file://${HOME}/	
-globalDomain	AppleShowAllExtensions	-bool	true	
com.apple.finder	FXEnableExtensionChangeWarning	-bool	false	
com.apple.finder	FXEnableRemoveFromICloudDriveWarning	-bool	true	
com.apple.finder	WarnOnEmptyTrash	-bool	false	
com.apple.finder	ShowPathbar	-bool	true	
com.apple.finder	ShowStatusBar	-bool	true	'

custom_finder () {
  config_defaults "${_finder}"
  defaults write "com.apple.finder" "NSToolbar Configuration Browser" \
    '{
      "TB Display Mode" = 2;
      "TB Item Identifiers" = (
        "com.apple.finder.BACK",
        "com.apple.finder.PATH",
        "com.apple.finder.SWCH",
        "com.apple.finder.ARNG",
        "NSToolbarFlexibleSpaceItem",
        "com.apple.finder.SRCH",
        "com.apple.finder.ACTN"
      );
    }'
}

# Configure getmail

custom_getmail () {
  true
}

# Configure Git

custom_git () {
  true
}

# Customize GnuPG

custom_gnupg () {
  true
}

# Customize iStat Menus

_istat_menus='com.bjango.istatmenus5.extras	MenubarSkinColor	-int	8	
com.bjango.istatmenus5.extras	MenubarTheme	-int	0	
com.bjango.istatmenus5.extras	DropdownTheme	-int	1	
com.bjango.istatmenus5.extras	CPU_MenubarMode	-string	100,2,0	
com.bjango.istatmenus5.extras	CPU_MenubarTextSize	-int	14	
com.bjango.istatmenus5.extras	CPU_MenubarGraphShowBackground	-int	0	
com.bjango.istatmenus5.extras	CPU_MenubarGraphWidth	-int	32	
com.bjango.istatmenus5.extras	CPU_MenubarGraphBreakdowns	-int	0	
com.bjango.istatmenus5.extras	CPU_MenubarGraphCustomColors	-int	0	
com.bjango.istatmenus5.extras	CPU_MenubarGraphOverall	-string	0.40 0.60 0.40 1.00	
com.bjango.istatmenus5.extras	CPU_MenubarCombineCores	-int	1	
com.bjango.istatmenus5.extras	CPU_MenubarGroupItems	-int	0	
com.bjango.istatmenus5.extras	CPU_MenubarSingleHistoryGraph	-int	0	
com.bjango.istatmenus5.extras	CPU_CombineLogicalCores	-int	1	
com.bjango.istatmenus5.extras	CPU_AppFormat	-int	0	
com.bjango.istatmenus5.extras	Memory_MenubarMode	-string	100,2,6	
com.bjango.istatmenus5.extras	Memory_MenubarPercentageSize	-int	14	
com.bjango.istatmenus5.extras	Memory_MenubarGraphBreakdowns	-int	1	
com.bjango.istatmenus5.extras	Memory_MenubarGraphCustomColors	-int	0	
com.bjango.istatmenus5.extras	Memory_MenubarGraphOverall	-string	0.40 0.60 0.40 1.00	
com.bjango.istatmenus5.extras	Memory_MenubarGraphWired	-string	0.40 0.60 0.40 1.00	
com.bjango.istatmenus5.extras	Memory_MenubarGraphActive	-string	0.47 0.67 0.47 1.00	
com.bjango.istatmenus5.extras	Memory_MenubarGraphCompressed	-string	0.53 0.73 0.53 1.00	
com.bjango.istatmenus5.extras	Memory_MenubarGraphInactive	-string	0.60 0.80 0.60 1.00	
com.bjango.istatmenus5.extras	Memory_IgnoreInactive	-int	0	
com.bjango.istatmenus5.extras	Memory_AppFormat	-int	0	
com.bjango.istatmenus5.extras	Memory_DisplayFormat	-int	1	
com.bjango.istatmenus5.extras	Disks_MenubarMode	-string	100,9,8	
com.bjango.istatmenus5.extras	Disks_MenubarGroupItems	-int	1	
com.bjango.istatmenus5.extras	Disks_MenubarRWShowLabel	-int	1	
com.bjango.istatmenus5.extras	Disks_MenubarRWBold	-int	0	
com.bjango.istatmenus5.extras	Disks_MenubarGraphActivityWidth	-int	32	
com.bjango.istatmenus5.extras	Disks_MenubarGraphActivityShowBackground	-int	0	
com.bjango.istatmenus5.extras	Disks_MenubarGraphActivityCustomColors	-int	0	
com.bjango.istatmenus5.extras	Disks_MenubarGraphActivityRead	-string	0.60 0.80 0.60 1.00	
com.bjango.istatmenus5.extras	Disks_MenubarGraphActivityWrite	-string	0.40 0.60 0.40 1.00	
com.bjango.istatmenus5.extras	Disks_SeperateFusion	-int	1	
com.bjango.istatmenus5.extras	Network_MenubarMode	-string	4,0,1	
com.bjango.istatmenus5.extras	Network_TextUploadColor-Dark	-string	1.00 1.00 1.00 1.00	
com.bjango.istatmenus5.extras	Network_TextDownloadColor-Dark	-string	1.00 1.00 1.00 1.00	
com.bjango.istatmenus5.extras	Network_GraphWidth	-int	32	
com.bjango.istatmenus5.extras	Network_GraphShowBackground	-int	0	
com.bjango.istatmenus5.extras	Network_GraphCustomColors	-int	0	
com.bjango.istatmenus5.extras	Network_GraphUpload	-string	0.60 0.80 0.60 1.00	
com.bjango.istatmenus5.extras	Network_GraphDownload	-string	0.40 0.60 0.40 1.00	
com.bjango.istatmenus5.extras	Network_GraphMode	-int	1	
com.bjango.istatmenus5.extras	Battery_MenubarMode	-string	5,0	
com.bjango.istatmenus5.extras	Battery_ColorGraphCustomColors	-int	1	
com.bjango.istatmenus5.extras	Battery_ColorGraphCharged	-string	0.40 0.60 0.40 1.00	
com.bjango.istatmenus5.extras	Battery_ColorGraphCharging	-string	0.60 0.80 0.60 1.00	
com.bjango.istatmenus5.extras	Battery_ColorGraphDraining	-string	1.00 0.60 0.60 1.00	
com.bjango.istatmenus5.extras	Battery_ColorGraphLow	-string	1.00 0.20 0.20 1.00	
com.bjango.istatmenus5.extras	Battery_PercentageSize	-int	14	
com.bjango.istatmenus5.extras	Battery_MenubarCustomizeStates	-int	0	
com.bjango.istatmenus5.extras	Battery_MenubarHideBluetooth	-int	1	
com.bjango.istatmenus5.extras	Time_MenubarFormat	-array-add	EE	
com.bjango.istatmenus5.extras	Time_MenubarFormat	-array-add	\\040	
com.bjango.istatmenus5.extras	Time_MenubarFormat	-array-add	MMM	
com.bjango.istatmenus5.extras	Time_MenubarFormat	-array-add	\\040	
com.bjango.istatmenus5.extras	Time_MenubarFormat	-array-add	d	
com.bjango.istatmenus5.extras	Time_MenubarFormat	-array-add	\\040	
com.bjango.istatmenus5.extras	Time_MenubarFormat	-array-add	h	
com.bjango.istatmenus5.extras	Time_MenubarFormat	-array-add	:	
com.bjango.istatmenus5.extras	Time_MenubarFormat	-array-add	mm	
com.bjango.istatmenus5.extras	Time_MenubarFormat	-array-add	:	
com.bjango.istatmenus5.extras	Time_MenubarFormat	-array-add	ss	
com.bjango.istatmenus5.extras	Time_MenubarFormat	-array-add	\\040	
com.bjango.istatmenus5.extras	Time_MenubarFormat	-array-add	a	
com.bjango.istatmenus5.extras	Time_DropdownFormat	-array-add	EE	
com.bjango.istatmenus5.extras	Time_DropdownFormat	-array-add	\\040	
com.bjango.istatmenus5.extras	Time_DropdownFormat	-array-add	h	
com.bjango.istatmenus5.extras	Time_DropdownFormat	-array-add	:	
com.bjango.istatmenus5.extras	Time_DropdownFormat	-array-add	mm	
com.bjango.istatmenus5.extras	Time_DropdownFormat	-array-add	\\040	
com.bjango.istatmenus5.extras	Time_DropdownFormat	-array-add	a	
com.bjango.istatmenus5.extras	Time_DropdownFormat	-array-add	\\040\\050	
com.bjango.istatmenus5.extras	Time_DropdownFormat	-array-add	zzz	
com.bjango.istatmenus5.extras	Time_DropdownFormat	-array-add	\\051	
com.bjango.istatmenus5.extras	Time_Cities	-array-add	4930956	
com.bjango.istatmenus5.extras	Time_Cities	-array-add	4887398	
com.bjango.istatmenus5.extras	Time_Cities	-array-add	5419384	
com.bjango.istatmenus5.extras	Time_Cities	-array-add	5392171	
com.bjango.istatmenus5.extras	Time_Cities	-array-add	5879400	
com.bjango.istatmenus5.extras	Time_Cities	-array-add	5856195	
com.bjango.istatmenus5.extras	Time_TextSize	-int	14	'

custom_istat_menus () {
  defaults delete com.bjango.istatmenus5.extras Time_MenubarFormat
  defaults delete com.bjango.istatmenus5.extras Time_DropdownFormat
  defaults delete com.bjango.istatmenus5.extras Time_Cities
  config_defaults "${_istat_menus}"
}

# Customize Login Items

custom_loginitems () {
  true
}

# Customize Moom

_moom='com.manytricks.Moom	Allow For Drawers	-bool	true	
com.manytricks.Moom	Grid Spacing	-bool	true	
com.manytricks.Moom	Grid Spacing: Gap	-int	2	
com.manytricks.Moom	Grid Spacing: Apply To Edges	-bool	false	
com.manytricks.Moom	Target Window Highlight	-float	0.25	
com.manytricks.Moom	Stealth Mode	-bool	true	
com.manytricks.Moom	Application Mode	-int	2	
com.manytricks.Moom	Mouse Controls	-bool	true	
com.manytricks.Moom	Mouse Controls Delay	-float	0.1	
com.manytricks.Moom	Mouse Controls Grid	-bool	true	
com.manytricks.Moom	Mouse Controls Grid: Mode	-int	3	
com.manytricks.Moom	Mouse Controls Grid: Columns	-int	10	
com.manytricks.Moom	Mouse Controls Grid: Rows	-int	6	
com.manytricks.Moom	Mouse Controls Include Custom Controls	-bool	true	
com.manytricks.Moom	Mouse Controls Include Custom Controls: Show On Hover	-bool	true	
com.manytricks.Moom	Mouse Controls Auto-Activate Window	-bool	true	
com.manytricks.Moom	Snap	-bool	false	
com.manytricks.Moom	Custom Controls	-array-add	{ Action = 19; "Relative Frame" = "{{0, 0.33333}, {0.5, 0.66666}}"; }	
com.manytricks.Moom	Custom Controls	-array-add	{ Action = 19; "Relative Frame" = "{{0, 0}, {0.3, 0.33333}}"; }	
com.manytricks.Moom	Custom Controls	-array-add	{ Action = 19; "Relative Frame" = "{{0.4, 0.33333}, {0.3, 0.66666}}"; }	
com.manytricks.Moom	Custom Controls	-array-add	{ Action = 19; "Relative Frame" = "{{0.3, 0}, {0.4, 0.33333}}"; }	
com.manytricks.Moom	Custom Controls	-array-add	{ Action = 19; "Relative Frame" = "{{0.7, 0.66666}, {0.3, 0.33333}}"; }	
com.manytricks.Moom	Custom Controls	-array-add	{ Action = 19; "Relative Frame" = "{{0.7, 0.33333}, {0.3, 0.33333}}"; }	
com.manytricks.Moom	Custom Controls	-array-add	{ Action = 19; "Relative Frame" = "{{0.7, 0}, {0.3, 0.33333}}"; }	
com.manytricks.Moom	Configuration Grid: Columns	-int	10	
com.manytricks.Moom	Configuration Grid: Rows	-int	6	
com.manytricks.Moom	SUEnableAutomaticChecks	-bool	true	'

custom_moom () {
  config_defaults "${_moom}"
}

# Customize nvALT

_nvalt='net.elasticthreads.nv	TableFontPointSize	-int	11	
net.elasticthreads.nv	AppActivationKeyCode	-int	-1	
net.elasticthreads.nv	AppActivationModifiers	-int	-1	
net.elasticthreads.nv	AutoCompleteSearches	-bool	true	
net.elasticthreads.nv	ConfirmNoteDeletion	-bool	true	
net.elasticthreads.nv	QuitWhenClosingMainWindow	-bool	false	
net.elasticthreads.nv	StatusBarItem	-bool	true	
net.elasticthreads.nv	ShowDockIcon	-bool	false	
net.elasticthreads.nv	PastePreservesStyle	-bool	false	
net.elasticthreads.nv	CheckSpellingInNoteBody	-bool	false	
net.elasticthreads.nv	TabKeyIndents	-bool	true	
net.elasticthreads.nv	UseSoftTabs	-bool	true	
net.elasticthreads.nv	MakeURLsClickable	-bool	true	
net.elasticthreads.nv	AutoSuggestLinks	-bool	false	
net.elasticthreads.nv	UseMarkdownImport	-bool	false	
net.elasticthreads.nv	UseReadability	-bool	false	
net.elasticthreads.nv	rtl	-bool	false	
net.elasticthreads.nv	UseAutoPairing	-bool	true	
net.elasticthreads.nv	DefaultEEIdentifier	-string	org.gnu.Emacs	
net.elasticthreads.nv	UserEEIdentifiers	-array-add	com.apple.TextEdit	
net.elasticthreads.nv	UserEEIdentifiers	-array-add	org.gnu.Emacs	
net.elasticthreads.nv	NoteBodyFont	-data	040b73747265616d747970656481e803840140848484064e53466f6e741e8484084e534f626a65637400858401692884055b3430635d060000001e000000fffe49006e0063006f006e0073006f006c006100740061004c004700430000008401660d8401630098019800980086	
net.elasticthreads.nv	HighlightSearchTerms	-bool	true	
net.elasticthreads.nv	SearchTermHighlightColor	-data	040b73747265616d747970656481e803840140848484074e53436f6c6f72008484084e534f626a65637400858401630184046666666683cdcc4c3f0183cdcc4c3f0186	
net.elasticthreads.nv	ForegroundTextColor	-data	040b73747265616d747970656481e803840140848484074e53436f6c6f72008484084e534f626a65637400858401630184046666666683cdcc4c3f83cdcc4c3f83cdcc4c3f0186	
net.elasticthreads.nv	BackgroundTextColor	-data	040b73747265616d747970656481e803840140848484074e53436f6c6f72008484084e534f626a65637400858401630184046666666683d1d0d03d83d1d0d03d83d1d0d03d0186	
net.elasticthreads.nv	ShowGrid	-bool	true	
net.elasticthreads.nv	AlternatingRows	-bool	true	
net.elasticthreads.nv	UseETScrollbarsOnLion	-bool	false	
net.elasticthreads.nv	KeepsMaxTextWidth	-bool	true	
net.elasticthreads.nv	NoteBodyMaxWidth	-int	650	
net.elasticthreads.nv	HorizontalLayout	-bool	false	
net.elasticthreads.nv	NoteAttributesVisible	-array-add	Title	
net.elasticthreads.nv	NoteAttributesVisible	-array-add	Tags	
net.elasticthreads.nv	TableIsReverseSorted	-bool	true	
net.elasticthreads.nv	TableSortColumn	-string	Date Modified	
net.elasticthreads.nv	TableColumnsHaveBodyPreview	-bool	true	'

custom_nvalt () {
  config_defaults "${_nvalt}"
}

# Customize Safari

_safari='com.apple.Safari	AlwaysRestoreSessionAtLaunch	-bool	false	
com.apple.Safari	OpenPrivateWindowWhenNotRestoringSessionAtLaunch	-bool	false	
com.apple.Safari	NewWindowBehavior	-int	1	
com.apple.Safari	NewTabBehavior	-int	1	
com.apple.Safari	AutoOpenSafeDownloads	-bool	false	
com.apple.Safari	TabCreationPolicy	-int	2	
com.apple.Safari	AutoFillFromAddressBook	-bool	false	
com.apple.Safari	AutoFillPasswords	-bool	true	
com.apple.Safari	AutoFillCreditCardData	-bool	false	
com.apple.Safari	AutoFillMiscellaneousForms	-bool	false	
com.apple.Safari	SuppressSearchSuggestions	-bool	false	
com.apple.Safari	UniversalSearchEnabled	-bool	false	
com.apple.Safari	WebsiteSpecificSearchEnabled	-bool	true	
com.apple.Safari	PreloadTopHit	-bool	true	
com.apple.Safari	ShowFavoritesUnderSmartSearchField	-bool	false	
com.apple.Safari	SafariGeolocationPermissionPolicy	-int	0	
com.apple.Safari	SendDoNotTrackHTTPHeader	-bool	true	
com.apple.Safari	com.apple.Safari.ContentPageGroupIdentifier.WebKit2ApplePayCapabilityDisclosureAllowed	-bool	true	
com.apple.Safari	CanPromptForPushNotifications	-bool	false	
com.apple.Safari	ShowFullURLInSmartSearchField	-bool	true	
com.apple.Safari	WebKitDefaultTextEncodingName	-string	utf-8	
com.apple.Safari	com.apple.Safari.ContentPageGroupIdentifier.WebKit2DefaultTextEncodingName	-string	utf-8	
com.apple.Safari	IncludeDevelopMenu	-bool	true	
com.apple.Safari	WebKitDeveloperExtrasEnabledPreferenceKey	-bool	true	
com.apple.Safari	com.apple.Safari.ContentPageGroupIdentifier.WebKit2DeveloperExtrasEnabled	-bool	true	
com.apple.Safari	ShowFavoritesBar-v2	-bool	true	
com.apple.Safari	AlwaysShowTabBar	-bool	true	
com.apple.Safari	ShowStatusBar	-bool	true	
com.apple.Safari	ShowStatusBarInFullScreen	-bool	true	'

custom_safari () {
  config_defaults "${_safari}"
}

# Customize Screen Saver

custom_screensaver () {
  if [ -e "/Library/Screen Savers/BlankScreen.saver" ]; then
    defaults -currentHost write com.apple.screensaver moduleDict \
      '{
        moduleName = "BlankScreen";
        path = "/Library/Screen Savers/BlankScreen.saver";
        type = 0;
      }'
  fi
}

# Customize Sieve

custom_sieve () {
  true
}

# Customize SSH

custom_ssh () {
  true
}

# Customize Sound

_sound='-globalDomain	com.apple.sound.beep.sound	-string	/System/Library/Sounds/Sosumi.aiff	
-globalDomain	com.apple.sound.uiaudio.enabled	-int	0	
-globalDomain	com.apple.sound.beep.feedback	-int	0	'

custom_sound () {
  config_defaults "${_sound}"
}

# Customize Siri

custom_siri () {
  defaults write com.apple.assistant.backedup "Output Voice" \
    '{
      Custom = 1;
      Footprint = 0;
      Gender = 1;
      Language = "en-US";
    }'
  defaults write com.apple.Siri StatusMenuVisible -bool false
}

# Customize Clock

custom_clock () {
  defaults -currentHost write com.apple.systemuiserver dontAutoLoad \
    -array-add "/System/Library/CoreServices/Menu Extras/Clock.menu"
  defaults write com.apple.menuextra.clock DateFormat \
    -string "EEE MMM d  h:mm:ss a"
}

# Customize Accessibility

_a11y='com.apple.universalaccess
reduceTransparency
-bool
true
'
_speech='com.apple.speech.voice.prefs	SelectedVoiceName	-string	Allison	
com.apple.speech.voice.prefs	SelectedVoiceCreator	-int	1886745202	
com.apple.speech.voice.prefs	SelectedVoiceID	-int	184555197	'

custom_a11y () {
  config_defaults "Acessibility" "${_a11y}"

  if test -d "/System/Library/Speech/Voices/Allison.SpeechVoice"; then
    config_defaults "${_speech}"
    defaults write com.apple.speech.voice.prefs VisibleIdentifiers \
      '{
        "com.apple.speech.synthesis.voice.allison.premium" = 1;
      }'
  fi
}

# Customize Terminal

_term_plist='delete			
add	:		dict
add	:name	string	ptb
add	:type	string	Window Settings
add	:ProfileCurrentVersion	real	2.05
add	:BackgroundColor	data	<?xml version="1.0" encoding="UTF-8"?><!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd"><plist version="1.0"><dict><key>$archiver</key><string>NSKeyedArchiver</string><key>$objects</key><array><string>$null</string><dict><key>$class</key><dict><key>CF$UID</key><integer>2</integer></dict><key>NSColorSpace</key><integer>1</integer><key>NSRGB</key><data>MC4xIDAuMSAwLjE=</data></dict><dict><key>$classes</key><array><string>NSColor</string><string>NSObject</string></array><key>$classname</key><string>NSColor</string></dict></array><key>$top</key><dict><key>root</key><dict><key>CF$UID</key><integer>1</integer></dict></dict><key>$version</key><integer>100000</integer></dict></plist>
add	:BackgroundBlur	real	0
add	:BackgroundSettingsForInactiveWindows	bool	false
add	:BackgroundAlphaInactive	real	1
add	:BackgroundBlurInactive	real	0
add	:Font	data	<?xml version="1.0" encoding="UTF-8"?><!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd"><plist version="1.0"><dict><key>$archiver</key><string>NSKeyedArchiver</string><key>$objects</key><array><string>$null</string><dict><key>$class</key><dict><key>CF$UID</key><integer>3</integer></dict><key>NSName</key><dict><key>CF$UID</key><integer>2</integer></dict><key>NSSize</key><real>13</real><key>NSfFlags</key><integer>16</integer></dict><string>InconsolataLGC</string><dict><key>$classes</key><array><string>NSFont</string><string>NSObject</string></array><key>$classname</key><string>NSFont</string></dict></array><key>$top</key><dict><key>root</key><dict><key>CF$UID</key><integer>1</integer></dict></dict><key>$version</key><integer>100000</integer></dict></plist>
add	:FontWidthSpacing	real	1
add	:FontHeightSpacing	real	1
add	:FontAntialias	bool	true
add	:UseBoldFonts	bool	true
add	:BlinkText	bool	false
add	:DisableANSIColor	bool	false
add	:UseBrightBold	bool	false
add	:TextColor	data	<?xml version="1.0" encoding="UTF-8"?><!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd"><plist version="1.0"><dict><key>$archiver</key><string>NSKeyedArchiver</string><key>$objects</key><array><string>$null</string><dict><key>$class</key><dict><key>CF$UID</key><integer>2</integer></dict><key>NSColorSpace</key><integer>1</integer><key>NSRGB</key><data>MC44IDAuOCAwLjg=</data></dict><dict><key>$classes</key><array><string>NSColor</string><string>NSObject</string></array><key>$classname</key><string>NSColor</string></dict></array><key>$top</key><dict><key>root</key><dict><key>CF$UID</key><integer>1</integer></dict></dict><key>$version</key><integer>100000</integer></dict></plist>
add	:TextBoldColor	data	<?xml version="1.0" encoding="UTF-8"?><!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd"><plist version="1.0"><dict><key>$archiver</key><string>NSKeyedArchiver</string><key>$objects</key><array><string>$null</string><dict><key>$class</key><dict><key>CF$UID</key><integer>2</integer></dict><key>NSColorSpace</key><integer>1</integer><key>NSRGB</key><data>MC44IDAuOCAwLjg=</data></dict><dict><key>$classes</key><array><string>NSColor</string><string>NSObject</string></array><key>$classname</key><string>NSColor</string></dict></array><key>$top</key><dict><key>root</key><dict><key>CF$UID</key><integer>1</integer></dict></dict><key>$version</key><integer>100000</integer></dict></plist>
add	:SelectionColor	data	<?xml version="1.0" encoding="UTF-8"?><!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd"><plist version="1.0"><dict><key>$archiver</key><string>NSKeyedArchiver</string><key>$objects</key><array><string>$null</string><dict><key>$class</key><dict><key>CF$UID</key><integer>2</integer></dict><key>NSColorSpace</key><integer>1</integer><key>NSRGB</key><data>MC4zIDAuMyAwLjM=</data></dict><dict><key>$classes</key><array><string>NSColor</string><string>NSObject</string></array><key>$classname</key><string>NSColor</string></dict></array><key>$top</key><dict><key>root</key><dict><key>CF$UID</key><integer>1</integer></dict></dict><key>$version</key><integer>100000</integer></dict></plist>
add	:ANSIBlackColor	data	<?xml version="1.0" encoding="UTF-8"?><!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd"><plist version="1.0"><dict><key>$archiver</key><string>NSKeyedArchiver</string><key>$objects</key><array><string>$null</string><dict><key>$class</key><dict><key>CF$UID</key><integer>2</integer></dict><key>NSColorSpace</key><integer>1</integer><key>NSRGB</key><data>MC4zIDAuMyAwLjM=</data></dict><dict><key>$classes</key><array><string>NSColor</string><string>NSObject</string></array><key>$classname</key><string>NSColor</string></dict></array><key>$top</key><dict><key>root</key><dict><key>CF$UID</key><integer>1</integer></dict></dict><key>$version</key><integer>100000</integer></dict></plist>
add	:ANSIRedColor	data	<?xml version="1.0" encoding="UTF-8"?><!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd"><plist version="1.0"><dict><key>$archiver</key><string>NSKeyedArchiver</string><key>$objects</key><array><string>$null</string><dict><key>$class</key><dict><key>CF$UID</key><integer>2</integer></dict><key>NSColorSpace</key><integer>1</integer><key>NSRGB</key><data>MC45NSAwLjUgMC41</data></dict><dict><key>$classes</key><array><string>NSColor</string><string>NSObject</string></array><key>$classname</key><string>NSColor</string></dict></array><key>$top</key><dict><key>root</key><dict><key>CF$UID</key><integer>1</integer></dict></dict><key>$version</key><integer>100000</integer></dict></plist>
add	:ANSIGreenColor	data	<?xml version="1.0" encoding="UTF-8"?><!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd"><plist version="1.0"><dict><key>$archiver</key><string>NSKeyedArchiver</string><key>$objects</key><array><string>$null</string><dict><key>$class</key><dict><key>CF$UID</key><integer>2</integer></dict><key>NSColorSpace</key><integer>1</integer><key>NSRGB</key><data>MC42IDAuOCAwLjY=</data></dict><dict><key>$classes</key><array><string>NSColor</string><string>NSObject</string></array><key>$classname</key><string>NSColor</string></dict></array><key>$top</key><dict><key>root</key><dict><key>CF$UID</key><integer>1</integer></dict></dict><key>$version</key><integer>100000</integer></dict></plist>
add	:ANSIYellowColor	data	<?xml version="1.0" encoding="UTF-8"?><!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd"><plist version="1.0"><dict><key>$archiver</key><string>NSKeyedArchiver</string><key>$objects</key><array><string>$null</string><dict><key>$class</key><dict><key>CF$UID</key><integer>2</integer></dict><key>NSColorSpace</key><integer>1</integer><key>NSRGB</key><data>MSAwLjggMC40</data></dict><dict><key>$classes</key><array><string>NSColor</string><string>NSObject</string></array><key>$classname</key><string>NSColor</string></dict></array><key>$top</key><dict><key>root</key><dict><key>CF$UID</key><integer>1</integer></dict></dict><key>$version</key><integer>100000</integer></dict></plist>
add	:ANSIBlueColor	data	<?xml version="1.0" encoding="UTF-8"?><!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd"><plist version="1.0"><dict><key>$archiver</key><string>NSKeyedArchiver</string><key>$objects</key><array><string>$null</string><dict><key>$class</key><dict><key>CF$UID</key><integer>2</integer></dict><key>NSColorSpace</key><integer>1</integer><key>NSRGB</key><data>MC40IDAuNiAwLjg=</data></dict><dict><key>$classes</key><array><string>NSColor</string><string>NSObject</string></array><key>$classname</key><string>NSColor</string></dict></array><key>$top</key><dict><key>root</key><dict><key>CF$UID</key><integer>1</integer></dict></dict><key>$version</key><integer>100000</integer></dict></plist>
add	:ANSIMagentaColor	data	<?xml version="1.0" encoding="UTF-8"?><!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd"><plist version="1.0"><dict><key>$archiver</key><string>NSKeyedArchiver</string><key>$objects</key><array><string>$null</string><dict><key>$class</key><dict><key>CF$UID</key><integer>2</integer></dict><key>NSColorSpace</key><integer>1</integer><key>NSRGB</key><data>MC44IDAuNiAwLjg=</data></dict><dict><key>$classes</key><array><string>NSColor</string><string>NSObject</string></array><key>$classname</key><string>NSColor</string></dict></array><key>$top</key><dict><key>root</key><dict><key>CF$UID</key><integer>1</integer></dict></dict><key>$version</key><integer>100000</integer></dict></plist>
add	:ANSICyanColor	data	<?xml version="1.0" encoding="UTF-8"?><!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd"><plist version="1.0"><dict><key>$archiver</key><string>NSKeyedArchiver</string><key>$objects</key><array><string>$null</string><dict><key>$class</key><dict><key>CF$UID</key><integer>2</integer></dict><key>NSColorSpace</key><integer>1</integer><key>NSRGB</key><data>MC40IDAuOCAwLjg=</data></dict><dict><key>$classes</key><array><string>NSColor</string><string>NSObject</string></array><key>$classname</key><string>NSColor</string></dict></array><key>$top</key><dict><key>root</key><dict><key>CF$UID</key><integer>1</integer></dict></dict><key>$version</key><integer>100000</integer></dict></plist>
add	:ANSIWhiteColor	data	<?xml version="1.0" encoding="UTF-8"?><!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd"><plist version="1.0"><dict><key>$archiver</key><string>NSKeyedArchiver</string><key>$objects</key><array><string>$null</string><dict><key>$class</key><dict><key>CF$UID</key><integer>2</integer></dict><key>NSColorSpace</key><integer>1</integer><key>NSRGB</key><data>MC44IDAuOCAwLjg=</data></dict><dict><key>$classes</key><array><string>NSColor</string><string>NSObject</string></array><key>$classname</key><string>NSColor</string></dict></array><key>$top</key><dict><key>root</key><dict><key>CF$UID</key><integer>1</integer></dict></dict><key>$version</key><integer>100000</integer></dict></plist>
add	:ANSIBrightBlackColor	data	<?xml version="1.0" encoding="UTF-8"?><!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd"><plist version="1.0"><dict><key>$archiver</key><string>NSKeyedArchiver</string><key>$objects</key><array><string>$null</string><dict><key>$class</key><dict><key>CF$UID</key><integer>2</integer></dict><key>NSColorSpace</key><integer>1</integer><key>NSRGB</key><data>MC41IDAuNSAwLjU=</data></dict><dict><key>$classes</key><array><string>NSColor</string><string>NSObject</string></array><key>$classname</key><string>NSColor</string></dict></array><key>$top</key><dict><key>root</key><dict><key>CF$UID</key><integer>1</integer></dict></dict><key>$version</key><integer>100000</integer></dict></plist>
add	:ANSIBrightRedColor	data	<?xml version="1.0" encoding="UTF-8"?><!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd"><plist version="1.0"><dict><key>$archiver</key><string>NSKeyedArchiver</string><key>$objects</key><array><string>$null</string><dict><key>$class</key><dict><key>CF$UID</key><integer>2</integer></dict><key>NSColorSpace</key><integer>1</integer><key>NSRGB</key><data>MSAwLjcgMC43</data></dict><dict><key>$classes</key><array><string>NSColor</string><string>NSObject</string></array><key>$classname</key><string>NSColor</string></dict></array><key>$top</key><dict><key>root</key><dict><key>CF$UID</key><integer>1</integer></dict></dict><key>$version</key><integer>100000</integer></dict></plist>
add	:ANSIBrightGreenColor	data	<?xml version="1.0" encoding="UTF-8"?><!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd"><plist version="1.0"><dict><key>$archiver</key><string>NSKeyedArchiver</string><key>$objects</key><array><string>$null</string><dict><key>$class</key><dict><key>CF$UID</key><integer>2</integer></dict><key>NSColorSpace</key><integer>1</integer><key>NSRGB</key><data>MC44IDEgMC44</data></dict><dict><key>$classes</key><array><string>NSColor</string><string>NSObject</string></array><key>$classname</key><string>NSColor</string></dict></array><key>$top</key><dict><key>root</key><dict><key>CF$UID</key><integer>1</integer></dict></dict><key>$version</key><integer>100000</integer></dict></plist>
add	:ANSIBrightYellowColor	data	<?xml version="1.0" encoding="UTF-8"?><!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd"><plist version="1.0"><dict><key>$archiver</key><string>NSKeyedArchiver</string><key>$objects</key><array><string>$null</string><dict><key>$class</key><dict><key>CF$UID</key><integer>2</integer></dict><key>NSColorSpace</key><integer>1</integer><key>NSRGB</key><data>MSAxIDAuNg==</data></dict><dict><key>$classes</key><array><string>NSColor</string><string>NSObject</string></array><key>$classname</key><string>NSColor</string></dict></array><key>$top</key><dict><key>root</key><dict><key>CF$UID</key><integer>1</integer></dict></dict><key>$version</key><integer>100000</integer></dict></plist>
add	:ANSIBrightBlueColor	data	<?xml version="1.0" encoding="UTF-8"?><!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd"><plist version="1.0"><dict><key>$archiver</key><string>NSKeyedArchiver</string><key>$objects</key><array><string>$null</string><dict><key>$class</key><dict><key>CF$UID</key><integer>2</integer></dict><key>NSColorSpace</key><integer>1</integer><key>NSRGB</key><data>MC42IDAuOCAx</data></dict><dict><key>$classes</key><array><string>NSColor</string><string>NSObject</string></array><key>$classname</key><string>NSColor</string></dict></array><key>$top</key><dict><key>root</key><dict><key>CF$UID</key><integer>1</integer></dict></dict><key>$version</key><integer>100000</integer></dict></plist>
add	:ANSIBrightMagentaColor	data	<?xml version="1.0" encoding="UTF-8"?><!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd"><plist version="1.0"><dict><key>$archiver</key><string>NSKeyedArchiver</string><key>$objects</key><array><string>$null</string><dict><key>$class</key><dict><key>CF$UID</key><integer>2</integer></dict><key>NSColorSpace</key><integer>1</integer><key>NSRGB</key><data>MSAwLjggMQ==</data></dict><dict><key>$classes</key><array><string>NSColor</string><string>NSObject</string></array><key>$classname</key><string>NSColor</string></dict></array><key>$top</key><dict><key>root</key><dict><key>CF$UID</key><integer>1</integer></dict></dict><key>$version</key><integer>100000</integer></dict></plist>
add	:ANSIBrightCyanColor	data	<?xml version="1.0" encoding="UTF-8"?><!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd"><plist version="1.0"><dict><key>$archiver</key><string>NSKeyedArchiver</string><key>$objects</key><array><string>$null</string><dict><key>$class</key><dict><key>CF$UID</key><integer>2</integer></dict><key>NSColorSpace</key><integer>1</integer><key>NSRGB</key><data>MC42IDEgMQ==</data></dict><dict><key>$classes</key><array><string>NSColor</string><string>NSObject</string></array><key>$classname</key><string>NSColor</string></dict></array><key>$top</key><dict><key>root</key><dict><key>CF$UID</key><integer>1</integer></dict></dict><key>$version</key><integer>100000</integer></dict></plist>
add	:ANSIBrightWhiteColor	data	<?xml version="1.0" encoding="UTF-8"?><!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd"><plist version="1.0"><dict><key>$archiver</key><string>NSKeyedArchiver</string><key>$objects</key><array><string>$null</string><dict><key>$class</key><dict><key>CF$UID</key><integer>2</integer></dict><key>NSColorSpace</key><integer>1</integer><key>NSRGB</key><data>MC45IDAuOSAwLjk=</data></dict><dict><key>$classes</key><array><string>NSColor</string><string>NSObject</string></array><key>$classname</key><string>NSColor</string></dict></array><key>$top</key><dict><key>root</key><dict><key>CF$UID</key><integer>1</integer></dict></dict><key>$version</key><integer>100000</integer></dict></plist>
add	:CursorType	integer	0
add	:CursorBlink	bool	false
add	:CursorColor	data	<?xml version="1.0" encoding="UTF-8"?><!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd"><plist version="1.0"><dict><key>$archiver</key><string>NSKeyedArchiver</string><key>$objects</key><array><string>$null</string><dict><key>$class</key><dict><key>CF$UID</key><integer>2</integer></dict><key>NSColorSpace</key><integer>1</integer><key>NSRGB</key><data>MC43IDAuNyAwLjc=</data></dict><dict><key>$classes</key><array><string>NSColor</string><string>NSObject</string></array><key>$classname</key><string>NSColor</string></dict></array><key>$top</key><dict><key>root</key><dict><key>CF$UID</key><integer>1</integer></dict></dict><key>$version</key><integer>100000</integer></dict></plist>
add	:ShowRepresentedURLInTitle	bool	true
add	:ShowRepresentedURLPathInTitle	bool	true
add	:ShowActiveProcessInTitle	bool	true
add	:ShowActiveProcessArgumentsInTitle	bool	false
add	:ShowShellCommandInTitle	bool	false
add	:ShowWindowSettingsNameInTitle	bool	false
add	:ShowTTYNameInTitle	bool	false
add	:ShowDimensionsInTitle	bool	false
add	:ShowCommandKeyInTitle	bool	false
add	:columnCount	integer	124
add	:rowCount	integer	20
add	:ShouldLimitScrollback	integer	0
add	:ScrollbackLines	integer	0
add	:ShouldRestoreContent	bool	false
add	:ShowRepresentedURLInTabTitle	bool	false
add	:ShowRepresentedURLPathInTabTitle	bool	false
add	:ShowActiveProcessInTabTitle	bool	true
add	:ShowActiveProcessArgumentsInTabTitle	bool	false
add	:ShowTTYNameInTabTitle	bool	false
add	:ShowComponentsWhenTabHasCustomTitle	bool	true
add	:ShowActivityIndicatorInTab	bool	true
add	:shellExitAction	integer	1
add	:warnOnShellCloseAction	integer	1
add	:useOptionAsMetaKey	bool	false
add	:ScrollAlternateScreen	bool	true
add	:TerminalType	string	xterm-256color
add	:deleteSendsBackspace	bool	false
add	:EscapeNonASCIICharacters	bool	true
add	:ConvertNewlinesOnPaste	bool	true
add	:StrictVTKeypad	bool	true
add	:scrollOnInput	bool	true
add	:Bell	bool	false
add	:VisualBell	bool	false
add	:VisualBellOnlyWhenMuted	bool	false
add	:BellBadge	bool	false
add	:BellBounce	bool	false
add	:BellBounceCritical	bool	false
add	:CharacterEncoding	integer	4
add	:SetLanguageEnvironmentVariables	bool	true
add	:EastAsianAmbiguousWide	bool	false'
_term_defaults='com.apple.Terminal	Startup Window Settings	-string	ptb	
com.apple.Terminal	Default Window Settings	-string	ptb	'

custom_terminal () {
  config_plist "${_term_plist}" \
    "${HOME}/Library/Preferences/com.apple.Terminal.plist" \
    ":Window Settings:ptb"
  config_defaults "${_term_defaults}"
}

# Customize Vim

custom_vim () {
  true
}

# Customize VLC

_vlc_defaults='org.videolan.vlc	SUEnableAutomaticChecks	-bool	true	
org.videolan.vlc	SUHasLaunchedBefore	-bool	true	
org.videolan.vlc	SUSendProfileInfo	-bool	true	'
_vlcrc='macosx	macosx-nativefullscreenmode	1
macosx	macosx-video-autoresize	0
macosx	macosx-appleremote	0
macosx	macosx-pause-minimized	1
macosx	macosx-continue-playback	1
core	metadata-network-access	1
core	volume-save	0
core	spdif	1
core	sub-language	English
subsdec	subsdec-encoding	UTF-8
avcodec	avcodec-hw	vda'

custom_vlc () {
  config_defaults "${_vlc_defaults}"
  if which crudini > /dev/null; then
    printf "%s\n" "${_vlcrc}" | \
    while IFS="$(printf '\t')" read section key value; do
      crudini --set "${HOME}/Library/Preferences/org.videolan.vlc/vlcrc" "${section}" "${key}" "${value}"
    done
  fi
}

# Customize Z-Shell

custom_zsh () {
  mkdir -m go= "${ZDOTDIR:-$HOME}"
  cat << EOF > "${ZDOTDIR:-$HOME}/.zshrc"
#!/bin/sh

curl --location --silent \
  "https://github.com/ptb/2017.8.21/raw/master/mac-setup.command" | \
  . /dev/stdin 1
EOF
  chmod +x "${ZDOTDIR:-$HOME}/.zshrc"
}

# Define Function =personalize=

personalize () {
  true
}

# Personalize Airfoil 4

personalize_airfoil4 () {
  true
}

# Personalize BBEdit 10

personalize_bbedit10 () {
  true
}

# Personalize Carbon Copy Cloner 4

personalize_ccc4 () {
  true
}

# Personalize ExpanDrive 5

personalize_expandrive5 () {
  true
}

# Personalize Flip4Mac

personalize_flip4mac () {
  true
}

# Personalize Sketchup Pro 8

personalize_sketchuppro8 () {
  true
}

# Personalize iStat Menus 5

personalize_istatmenus5 () {
  true
}

# Personalize Little Snitch 4

personalize_littlesnitch4 () {
  true
}

# Personalize Meteorologist 3

personalize_meteorologist3 () {
  true
}

# Personalize Moom

personalize_moom () {
  true
}

# Personalize NZBGet

personalize_nzbget () {
  true
}

# Personalize NZBVortex

personalize_nzbvortex () {
  true
}

# Personalize Pacifist

personalize_pacifist () {
  true
}

# Personalize PCalc 3

personalize_pcalc3 () {
  true
}

# Personalize Scrivener

personalize_scrivener () {
  true
}

# Personalize SizeUp

personalize_sizeup () {
  true
}

# Personalize SteerMouse 5

personalize_steermouse5 () {
  true
}

# Personalize SourceTree

personalize_sourcetree () {
  true
}

# Personalize Tower 2

personalize_tower2 () {
  true
}

# Personalize Transmit 4

personalize_transmit4 () {
  true
}

# Personalize Tune4mac

personalize_tune4mac () {
  true
}

# Personalize VMware Fusion 8 Pro

personalize_vmwarefusion8pro () {
  true
}
