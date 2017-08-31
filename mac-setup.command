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

# Define Function =p=

p () {
  printf "\n\033[1m\033[34m%s\033[0m\n\n" "${1}"
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
  init_devtools
  init_xcode
  init_updates
  init_mas_save

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
      return text 1 through -2 of POSIX path of (choose folder with prompt "Select Installation Cache Location")
    end run
EOF
) && \
  test -d "${a}" || \
    a="${HOME}/Library/Caches/"

  grep -q "CACHES" "/etc/zshenv" 2> /dev/null || \
  printf "%s\n" \
    "export CACHES=\"${a}\"" \
    "export HOMEBREW_CACHE=\"${a}/brew\"" \
    "export BREWFILE=\"${a}/brew/Brewfile\"" | \
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
/Library/Desktop Pictures
/Library/ColorPickers
/Library/Fonts
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

# Install Developer Tools

init_devtools () {
  p="${HOMEBREW_CACHE}/Cask/Command Line Tools (macOS High Sierra version 10.13).pkg"
  i="com.apple.pkg.CLTools_SDK_macOS1013"

  if test -f "${p}"; then
    if ! pkgutil --pkg-info "${i}" > /dev/null 2>&1; then
      sudo installer -pkg "${p}" -target /
    fi
  else
    xcode-select --install
  fi
}

# Install Xcode

init_xcode () {
  if test -f ${HOMEBREW_CACHE}/Cask/xcode*.xip; then
    p "Installing Xcode"
    dest="${HOMEBREW_CACHE}/Cask/xcode"
    if ! test -d "$dest"; then
      pkgutil --expand ${HOMEBREW_CACHE}/Cask/xcode*.xip "$dest"
      curl --location --silent \
        "https://gist.githubusercontent.com/pudquick/ff412bcb29c9c1fa4b8d/raw/24b25538ea8df8d0634a2a6189aa581ccc6a5b4b/parse_pbzx2.py" | \
        python - "${dest}/Content"
      find "${dest}" -name "*.xz" -print0 | \
        xargs -0 -L 1 gunzip
      cat ${dest}/Content.part* > \
        ${dest}/Content.cpio
    fi
    cd /Applications && \
      sudo cpio -dimu --file=${dest}/Content.cpio
    for pkg in /Applications/Xcode*.app/Contents/Resources/Packages/*.pkg; do
      sudo installer -pkg "$pkg" -target /
    done
    x="$(find '/Applications' -maxdepth 1 -name 'Xcode[^ ]*.app' -print -quit)"
    if test -n "${x}"; then
      sudo xcode-select -s "${x}"
      sudo xcodebuild -license accept
    fi
  fi
}

# Install macOS Updates

init_updates () {
  sudo softwareupdate --install --all
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
as="$(getconf DARWIN_USER_CACHE_DIR)com.apple.appstore"
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

# Define Function =install=

install () {
  install_macos_sw
  install_node_sw
  install_perl_sw
  install_python_sw
  install_ruby_sw

  which config
}

# Install macOS Software with =brew=

install_macos_sw () {
  p "Installing macOS Software"
  install_paths
  install_brew
  install_brewfile_taps
  install_brewfile_brew_pkgs
  install_brewfile_cask_args
  install_brewfile_cask_pkgs
  install_brewfile_mas_apps
  install_links

  x="$(find '/Applications' -maxdepth 1 -name 'Xcode[^ ]*.app' -print -quit)"
  if test -n "${x}"; then
    sudo xcode-select -s "${x}"
    sudo xcodebuild -license accept
  fi

  brew bundle --file="${BREWFILE}"
  sudo xattr -rd "com.apple.quarantine" "/Applications" > /dev/null 2>&1
  sudo chmod -R go=u-w "/Applications" > /dev/null 2>&1
}

# Add =/usr/local/bin/sbin= to Default Path

install_paths () {
  if ! grep -Fq "/usr/local/sbin" /etc/paths; then
    sudo sed -i "" -e "/\/usr\/sbin/{x;s/$/\/usr\/local\/sbin/;G;}" /etc/paths
  fi
}

# Install Homebrew Package Manager

install_brew () {
  if ! which brew > /dev/null; then
    ruby -e \
      "$(curl -Ls 'https://github.com/Homebrew/install/raw/master/install')" \
      < /dev/null > /dev/null 2>&1
  fi
  printf "" > "${BREWFILE}"
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
pinentry-mac
plenv
pyenv
rbenv
rsync
selenium-server-standalone
shellcheck
sqlite
stow
tag
terminal-notifier
the_silver_searcher
trash
unrar
vim
yarn
youtube-dl
zsh
zsh-history-substring-search
homebrew/php/php71
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

_args='colorpickerdir	/Library/ColorPickers
fontdir	/Library/Fonts
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
  sed -i "" -e '$ s/,/\
/' "${BREWFILE}"
}

# Add Homebrew Casks to Brewfile

_casks='java
xquartz
adium
alfred
arduino
atom
bbedit
caffeine
carbon-copy-cloner
charles
dash
dropbox
exifrenamer
firefox
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
menubar-countdown
meteorologist
moom
mp4tools
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
ptb/custom/enhanced-dictation
ptb/custom/ipmenulet
ptb/custom/pcalc-3
ptb/custom/sketchup-pro
ptb/custom/text-to-speech-alex
ptb/custom/text-to-speech-allison
ptb/custom/text-to-speech-samantha
ptb/custom/text-to-speech-tom
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
Affinity Photo	824183456
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
    "${CACHES}/mas/" "${MASDIR}/"

  printf "%s\n" "${_mas}" | \
  while IFS="$(printf '\t')" read app id; do
    printf 'mas "%s", id: %s\n' "${app}" "${id}" >> "${BREWFILE}"
  done
}

# Link System Utilities to Applications

_links='/System/Library/CoreServices/Applications
/Applications/Xcode.app/Contents/Applications
/Applications/Xcode.app/Contents/Developer/Applications
/Applications/Xcode-beta.app/Contents/Applications
/Applications/Xcode-beta.app/Contents/Developer/Applications'

install_links () {
  printf "%s\n" "${_links}" | \
  while IFS="$(printf '\t')" read link; do
    find "${link}" -maxdepth 1 -name "*.app" -type d -print0 2> /dev/null | \
    xargs -0 -I {} -L 1 ln -s "{}" "/Applications" 2> /dev/null
  done
}

# Install Node Software with =nodenv=

install_node_sw () {
  if which nodenv > /dev/null; then
    p "Installing Node.js Software"
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

    git clone https://github.com/nodenv/node-build-update-defs.git \
      "$(nodenv root)"/plugins/node-build-update-defs
    nodenv update-version-defs > /dev/null

    nodenv install --skip-existing 8.4.0
    nodenv global 8.4.0
    rehash

    grep -q "${NODENV_ROOT}" "/etc/paths" || \
    sudo sed -i "" -e "1i\\
${NODENV_ROOT}/shims
" "/etc/paths"
  fi
}

# Install Perl Software with =plenv=

install_perl_sw () {
  if which plenv > /dev/null; then
    p "Installing Perl 5 Software"
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
    sudo sed -i "" -e "1i\\
${PLENV_ROOT}/shims
" "/etc/paths"
  fi
}

# Install Python Software with =pyenv=

install_python_sw () {
  if which pyenv > /dev/null; then
    p "Installing Python 2 & 3 Software"
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

    # Reference: https://github.com/mdhiggins/sickbeard_mp4_automator
    pip install --upgrade "babelfish" "guessit<2" "qtfaststart" "requests" "stevedore==1.19.1" "subliminal<2"
    pip install --upgrade "requests-cache" "requests[security]"

    # Reference: https://github.com/pixelb/crudini
    pip install --upgrade "crudini"

    grep -q "${PYENV_ROOT}" "/etc/paths" || \
    sudo sed -i "" -e "1i\\
${PYENV_ROOT}/shims
" "/etc/paths"
  fi
}

# Install Ruby Software with =rbenv=

install_ruby_sw () {
  if which rbenv > /dev/null; then
    p "Installing Ruby Software"
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

    gem update --system > /dev/null

    trash /usr/local/ruby/versions/2.4.1/bin/rdoc
    trash /usr/local/ruby/versions/2.4.1/bin/ri
    gem update

    gem install bundler

    grep -q "${RBENV_ROOT}" "/etc/paths" || \
    sudo sed -i "" -e "1i\\
${RBENV_ROOT}/shims
" "/etc/paths"
  fi
}

# Define Function =config=

config () {
  config_admin_req
  config_bbedit
  config_desktop
  config_dovecot
  config_emacs
  config_sysprefs
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

# Mark Applications Requiring Administrator Account

_admin_req='Carbon Copy Cloner.app
Charles.app
Composer.app
Dropbox.app
iStat Menus.app
Moom.app
VMware Fusion.app
Wireshark.app'

config_admin_req () {
  printf "%s\n" "${_admin_req}" | \
  while IFS="$(printf '\t')" read app; do
    sudo tag -a "Red, admin" "/Applications/${app}"
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

  base64 -D << EOF > "/Library/Desktop Pictures/Solid Colors/Solid Black.png"
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

      MAILADM="$(ask 'Email: Administrator Email?' 'Set Email' "$(whoami)@$(hostname)")"
      MAILSVR="$(ask 'Email: Server Hostname for DNS?' 'Set Hostname' "$(hostname)")"
      SSL="$(brew --prefix openssl)"
      printf "%s\n" \
        "postmaster_address = '${MAILADM}'" \
        "ssl_cert = <${SSL}/certs/${MAILSVR}/${MAILSVR}.crt" \
        "ssl_key = <${SSL}/certs/${MAILSVR}/${MAILSVR}.key" | \
      tee -a "/usr/local/etc/dovecot/dovecot.conf" > /dev/null

      if test ! -f "/usr/local/etc/dovecot/cram-md5.pwd"; then
        while true; do
          MAILUSR="$(ask 'New Email Account: User Name?' 'Create Account' "$(whoami)")"
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

# Configure iStat Menus

config_istatmenus () {
  test -d "/Applications/iStat Menus.app" && \
  open "/Applications/iStat Menus.app"
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

# Configure System Preferences

config_sysprefs () {
  config_energy
  config_mas
  config_guest
}

# Configure Energy Saver

_energy='-c	displaysleep	20
-c	sleep	0
-c	disksleep	60
-c	womp	1
-c	autorestart	1
-c	powernap	1
-u	displaysleep	2
-u	lessbright	1
-u	haltafter	5
-u	haltremain	-1
-u	haltlevel	-1'

config_energy () {
  printf "%s\n" "${_energy}" | \
  while IFS="$(printf '\t')" read flag setting value; do
    sudo pmset $flag ${setting} ${value}
  done
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
  grep -q $(which zsh) /etc/shells ||
  print "$(which zsh)\n" | \
  sudo tee -a /etc/shells > /dev/null

  chsh -s $(which zsh)
  sudo chsh -s $(which zsh)

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
  e="$(ask 'New macOS Account: Email Address?' 'OK' '')"
  curl --output "/Library/User Pictures/${e}.jpg" --silent \
    "https://www.gravatar.com/avatar/$(md5 -qs ${e}).jpg?s=512"

  g="$(curl --location --silent \
    "https://api.github.com/search/users?q=${e}" | \
    sed -n 's/^.*"url": "\(.*\)".*/\1/p')"
  g="$(curl --location --silent ${g})"

  n="$(printf ${g} | sed -n 's/^.*"name": "\(.*\)".*/\1/p')"
  n="$(ask 'New macOS Account: Real Name?' 'OK' ${n})"

  u="$(printf ${g} | sed -n 's/^.*"login": "\(.*\)".*/\1/p')"
  u="$(ask 'New macOS Account: User Name?' 'OK' ${u})"

  sudo defaults write \
    "/System/Library/User Template/Non_localized/Library/Preferences/.GlobalPreferences.plist" \
    "com.apple.swipescrolldirection" -bool false

  sudo sysadminctl -admin -addUser "${u}" -fullName "${n}" -password - \
    -shell "$(which zsh)" -picture "/Library/User Pictures/${e}.jpg"
}

# Reinstate =sudo= Password

config_rm_sudoers () {
  sudo rm -f "/etc/sudoers.d/wheel" && \
  sudo dscl /Local/Default -delete /Groups/wheel GroupMembership "$(whoami)"

  /usr/bin/read -n 1 -p "Press any key to continue.
" -s
  if run "Log Out Then Log Back In?" "Cancel" "Log Out"; then
    osascript -e 'tell app "loginwindow" to «event aevtrlgo»'
  fi
}

# Define Function =custom=

custom () {
  custom_githome
  custom_atom
  custom_autoping
  custom_dropbox
  custom_emacs
  custom_finder
  custom_getmail
  custom_git
  custom_gnupg
  custom_istatmenus
  custom_meteorologist
  custom_moom
  custom_nvalt
  custom_nzbget
  custom_safari
  custom_sieve
  custom_ssh
  custom_sysprefs
  custom_terminal
  custom_vim
  custom_vlc
  custom_zsh

  which personalize_all
}

# Customize Home

custom_githome () {
  git -C "${HOME}" init

  test -f "${CACHES}/Dropbox/.zshenv" && \
    mkdir -p "${ZDOTDIR:-$HOME}" && \
    cp "${CACHES}/Dropbox/.zshenv" "${ZDOTDIR:-$HOME}" && \
    . "${ZDOTDIR:-$HOME}/.zshenv"

  a=$(ask "Existing Git Home Repository Path or URL" "Add Remote" "")
  if test -n "${a}"; then
    git -C "${HOME}" remote add origin "${a}"
    git -C "${HOME}" fetch origin master
  fi

  if run "Encrypt and commit changes to Git and push to GitHub, automatically?" "No" "Add AutoKeep"; then
    curl --location --silent \
      "https://github.com/ptb/autokeep/raw/master/autokeep.command" | \
      . /dev/stdin 0

    autokeep_remote
    autokeep_push
    autokeep_gitignore
    autokeep_post_commit
    autokeep_launchagent
    autokeep_crypt

    git reset --hard
    git checkout -f -b master FETCH_HEAD
  fi

  chmod -R go= "${HOME}" > /dev/null 2>&1
}

# Customize Atom

_atom='atom-beautify
atom-css-comb
atom-fuzzy-grep
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

# Customize Dropbox

custom_dropbox () {
  test -d "/Applications/Dropbox.app" && \
    open "/Applications/Dropbox.app"
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
    (left . 1201)
    (height . 50)
    (width . 121)
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

  test -d "/Applications/Emacs.app" && \
    open "/Applications/Emacs.app"
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

# Customize getmail

custom_getmail () {
  true
}

# Customize Git

custom_git () {
  true
}

# Customize GnuPG

custom_gnupg () {
  true
}

# Customize iStat Menus

_istatmenus='com.bjango.istatmenus5.extras	MenubarSkinColor	-int	8	
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

custom_istatmenus () {
  defaults delete com.bjango.istatmenus5.extras Time_MenubarFormat > /dev/null 2>&1
  defaults delete com.bjango.istatmenus5.extras Time_DropdownFormat > /dev/null 2>&1
  defaults delete com.bjango.istatmenus5.extras Time_Cities > /dev/null 2>&1
  config_defaults "${_istatmenus}"
}

# Customize Meteorologist

_meteorologist='com.heat.meteorologist	controlsInSubmenu	-string	0	
com.heat.meteorologist	currentWeatherInSubmenu	-string	0	
com.heat.meteorologist	displayCityName	-string	0	
com.heat.meteorologist	displayHumidity	-string	0	
com.heat.meteorologist	displayWeatherIcon	-string	1	
com.heat.meteorologist	extendedForecastIcons	-string	1	
com.heat.meteorologist	extendedForecastInSubmenu	-string	0	
com.heat.meteorologist	extendedForecastSingleLine	-string	1	
com.heat.meteorologist	forecastDays	-int	8	
com.heat.meteorologist	viewExtendedForecast	-string	1	
com.heat.meteorologist	weatherSource_1	-int	3	'

custom_meteorologist () {
  config_defaults "${_meteorologist}"
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
com.manytricks.Moom	Mouse Controls Grid: Columns	-int	16	
com.manytricks.Moom	Mouse Controls Grid: Rows	-int	9	
com.manytricks.Moom	Mouse Controls Include Custom Controls	-bool	true	
com.manytricks.Moom	Mouse Controls Include Custom Controls: Show On Hover	-bool	false	
com.manytricks.Moom	Mouse Controls Auto-Activate Window	-bool	true	
com.manytricks.Moom	Snap	-bool	false	
com.manytricks.Moom	Custom Controls	-array-add	{ Action = 19; "Relative Frame" = "{{0, 0.55555}, {0.375, 0.44444}}"; }	
com.manytricks.Moom	Custom Controls	-array-add	{ Action = 19; "Relative Frame" = "{{0, 0.22222}, {0.375, 0.33333}}"; }	
com.manytricks.Moom	Custom Controls	-array-add	{ Action = 19; "Relative Frame" = "{{0, 0}, {0.25, 0.22222}}"; }	
com.manytricks.Moom	Custom Controls	-array-add	{ Action = 19; "Relative Frame" = "{{0.125, 0}, {0.25, 0.22222}}"; }	
com.manytricks.Moom	Custom Controls	-array-add	{ Action = 19; "Relative Frame" = "{{0.375, 0.44444}, {0.3125, 0.55555}}"; }	
com.manytricks.Moom	Custom Controls	-array-add	{ Action = 19; "Relative Frame" = "{{0.375, 0}, {0.3125, 0.44444}}"; }	
com.manytricks.Moom	Custom Controls	-array-add	{ Action = 19; "Relative Frame" = "{{0.6875, 0.55555}, {0.3125, 0.44444}}"; }	
com.manytricks.Moom	Custom Controls	-array-add	{ Action = 19; "Relative Frame" = "{{0.6875, 0.33333}, {0.3125, 0.22222}}"; }	
com.manytricks.Moom	Custom Controls	-array-add	{ Action = 19; "Relative Frame" = "{{0.6875, 0}, {0.3125, 0.33333}}"; }	
com.manytricks.Moom	Custom Controls	-array-add	{ Action = 1001; "Apply to Overlapping Windows" = 1; Snapshot = ( { "Application Name" = Safari; "Bundle Identifier" = "com.apple.safari"; "Window Frame" = "{{0, 989}, {1199, 789}}"; "Window Subrole" = AXStandardWindow; }, { "Application Name" = Safari; "Bundle Identifier" = "com.apple.safari"; "Window Frame" = "{{0, 396}, {1199, 591}}"; "Window Subrole" = AXStandardWindow; }, { "Application Name" = Finder; "Bundle Identifier" = "com.apple.finder"; "Window Frame" = "{{0, 0}, {799, 394}}"; "Window Subrole" = AXStandardWindow; }, { "Application Name" = Messages; "Bundle Identifier" = "com.apple.ichat"; "Window Frame" = "{{401, 0}, {798, 394}}"; "Window Subrole" = AXStandardWindow; }, { "Application Name" = Emacs; "Bundle Identifier" = "org.gnu.emacs"; "Window Frame" = "{{1201, 806}, {991, 972}}"; "Window Subrole" = AXStandardWindow; }, { "Application Name" = Terminal; "Bundle Identifier" = "com.apple.terminal"; "Window Frame" = "{{1201, 17}, {993, 772}}"; "Window Subrole" = AXStandardWindow; }, { "Application Name" = Emacs; "Bundle Identifier" = "org.gnu.emacs"; "Window Frame" = "{{2201, 996}, {991, 782}}"; "Window Subrole" = AXStandardWindow; }, { "Application Name" = Atom; "Bundle Identifier" = "com.github.atom"; "Window Frame" = "{{2201, 485}, {999, 502}}"; "Window Subrole" = AXStandardWindow; }, { "Application Name" = VLC; "Bundle Identifier" = "org.videolan.vlc"; "Window Frame" = "{{2201, 0}, {999, 483}}"; "Window Subrole" = AXStandardWindow; } ); "Snapshot Screens" = ( "{{0, 0}, {3200, 1800}}" ); }	
com.manytricks.Moom	Configuration Grid: Columns	-int	16	
com.manytricks.Moom	Configuration Grid: Rows	-int	9	
com.manytricks.Moom	SUEnableAutomaticChecks	-bool	true	'

custom_moom () {
  killall Moom > /dev/null 2>&1
  defaults delete com.manytricks.Moom "Custom Controls" > /dev/null 2>&1
  config_defaults "${_moom}"
  test -d "/Applications/Moom.app" && \
    open "/Applications/Moom.app"
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

# Customize NZBGet

# - $7.50/mth: http://www.news.astraweb.com/specials/2mospecial.html
# - €13/100GB: https://www.tweaknews.eu/en/usenet-plans
# - $17/100GB: https://www.newsdemon.com/usenet-access.php
# - $20/200GB: https://billing.blocknews.net/signup.php


_nzbget='ControlIP	127.0.0.1
ControlPort	6789
AuthorizedIP	127.0.0.1
Server1.Level	0
Server1.Host	ssl.astraweb.com
Server1.Port	443
Server1.Encryption	yes
Server1.Connections	6
Server1.Retention	3000
Server2.Level	0
Server2.Host	ssl-us.astraweb.com
Server2.Port	443
Server2.Encryption	yes
Server2.Connections	6
Server2.Retention	3000
Server3.Level	0
Server3.Host	ssl-eu.astraweb.com
Server3.Port	443
Server3.Encryption	yes
Server3.Connections	6
Server3.Retention	3000
Server4.Level	1
Server4.Host	news.tweaknews.eu
Server4.Port	443
Server4.Encryption	yes
Server4.Connections	40
Server4.Retention	2500
Server5.Level	2
Server5.Host	news.newsdemon.com
Server5.Port	563
Server5.Encryption	yes
Server5.Connections	12
Server5.Retention	3303
Server6.Level	2
Server6.Host	us.newsdemon.com
Server6.Port	563
Server6.Encryption	yes
Server6.Connections	12
Server6.Retention	3303
Server7.Level	2
Server7.Host	eu.newsdemon.com
Server7.Port	563
Server7.Encryption	yes
Server7.Connections	12
Server7.Retention	3303
Server8.Level	2
Server8.Host	nl.newsdemon.com
Server8.Port	563
Server8.Encryption	yes
Server8.Connections	12
Server8.Retention	3303
Server9.Level	2
Server9.Host	usnews.blocknews.net
Server9.Port	443
Server9.Encryption	yes
Server9.Connections	16
Server9.Retention	3240
Server10.Level	2
Server10.Host	eunews.blocknews.net
Server10.Port	443
Server10.Encryption	yes
Server10.Connections	16
Server10.Retention	3240
Server11.Level	2
Server11.Host	eunews2.blocknews.net
Server11.Port	443
Server11.Encryption	yes
Server11.Connections	16
Server11.Retention	3240'

custom_nzbget () {
  f="${HOME}/Library/Application Support/NZBGet/nzbget.conf"
  mkdir -p "$(dirname $f)"
  if which crudini > /dev/null; then
    printf "%s\n" "${_nzbget}" | \
    while IFS="$(printf '\t')" read key value; do
      crudini --set "$f" "" "${key}" "${value}"
    done
  fi
  sed -i "" -e "s/ = /=/g" "$f"
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

# Customize Sieve

custom_sieve () {
  cat > "${HOME}/.sieve" << EOF
require ["date", "fileinto", "imap4flags", "mailbox", "relational", "variables"];

setflag "\\\\Seen";

if date :is "date" "year" "1995" { fileinto :create "Archives.1995"; }
if date :is "date" "year" "1996" { fileinto :create "Archives.1996"; }
if date :is "date" "year" "1997" { fileinto :create "Archives.1997"; }
if date :is "date" "year" "1998" { fileinto :create "Archives.1998"; }
if date :is "date" "year" "1999" { fileinto :create "Archives.1999"; }
if date :is "date" "year" "2000" { fileinto :create "Archives.2000"; }
if date :is "date" "year" "2001" { fileinto :create "Archives.2001"; }
if date :is "date" "year" "2002" { fileinto :create "Archives.2002"; }
if date :is "date" "year" "2003" { fileinto :create "Archives.2003"; }
if date :is "date" "year" "2004" { fileinto :create "Archives.2004"; }
if date :is "date" "year" "2005" { fileinto :create "Archives.2005"; }
if date :is "date" "year" "2006" { fileinto :create "Archives.2006"; }
if date :is "date" "year" "2007" { fileinto :create "Archives.2007"; }
if date :is "date" "year" "2008" { fileinto :create "Archives.2008"; }
if date :is "date" "year" "2009" { fileinto :create "Archives.2009"; }
if date :is "date" "year" "2010" { fileinto :create "Archives.2010"; }
if date :is "date" "year" "2011" { fileinto :create "Archives.2011"; }
if date :is "date" "year" "2012" { fileinto :create "Archives.2012"; }
if date :is "date" "year" "2013" { fileinto :create "Archives.2013"; }
if date :is "date" "year" "2014" { fileinto :create "Archives.2014"; }
if date :is "date" "year" "2015" { fileinto :create "Archives.2015"; }
if date :is "date" "year" "2016" { fileinto :create "Archives.2016"; }
if date :is "date" "year" "2017" { fileinto :create "Archives.2017"; }
if date :is "date" "year" "2018" { fileinto :create "Archives.2018"; }
if date :is "date" "year" "2019" { fileinto :create "Archives.2019"; }
if date :is "date" "year" "2020" { fileinto :create "Archives.2020"; }
EOF
}

# Customize SSH

custom_ssh () {
  true
}

# Customize System Preferences

custom_sysprefs () {
  custom_general
  custom_desktop "/Library/Desktop Pictures/Solid Colors/Solid Black.png"
  custom_screensaver
  custom_dock
  custom_dockapps
  # custom_security
  custom_text
  custom_dictation
  custom_mouse
  custom_trackpad
  custom_sound
  custom_loginitems
  custom_siri
  custom_clock
  custom_a11y
}

# Customize General

_general='-globalDomain	AppleAquaColorVariant	-int	6	
-globalDomain	AppleInterfaceStyle	-string	Dark	
-globalDomain	_HIHideMenuBar	-bool	false	
-globalDomain	AppleHighlightColor	-string	0.600000 0.800000 0.600000	
-globalDomain	NSTableViewDefaultSizeMode	-int	1	
-globalDomain	AppleShowScrollBars	-string	Always	
-globalDomain	AppleScrollerPagingBehavior	-bool	false	
-globalDomain	NSCloseAlwaysConfirmsChanges	-bool	true	
-globalDomain	NSQuitAlwaysKeepsWindows	-bool	false	
com.apple.coreservices.useractivityd	ActivityAdvertisingAllowed	-bool	true	-currentHost
com.apple.coreservices.useractivityd	ActivityReceivingAllowed	-bool	true	-currentHost
-globalDomain	AppleFontSmoothing	-int	1	-currentHost'

custom_general () {
  config_defaults "${_general}"
  osascript << EOF
    tell application "System Events"
      tell appearance preferences
        set recent documents limit to 0
        set recent applications limit to 0
        set recent servers limit to 0
      end tell
    end tell
EOF
}

# Customize Desktop Picture

custom_desktop () {
  osascript - "${1}" << EOF 2> /dev/null
    on run { _this }
      tell app "System Events" to set picture of every desktop to POSIX file _this
    end run
EOF
}

# Customize Screen Saver

_screensaver='com.apple.screensaver	idleTime	-int	0	-currentHost
com.apple.dock	wvous-tl-corner	-int	2	
com.apple.dock	wvous-tl-modifier	-int	1048576	
com.apple.dock	wvous-bl-corner	-int	10	
com.apple.dock	wvous-bl-modifier	-int	0	'

custom_screensaver () {
  if test -e "/Library/Screen Savers/BlankScreen.saver"; then
    defaults -currentHost write com.apple.screensaver moduleDict \
      '{
        moduleName = "BlankScreen";
        path = "/Library/Screen Savers/BlankScreen.saver";
        type = 0;
      }'
  fi
  config_defaults "${_screensaver}"
}

# Customize Dock

_dock='com.apple.dock	tilesize	-int	32	
com.apple.dock	magnification	-bool	false	
com.apple.dock	largesize	-int	64	
com.apple.dock	orientation	-string	right	
com.apple.dock	mineffect	-string	scale	
-globalDomain	AppleWindowTabbingMode	-string	always	
-globalDomain	AppleActionOnDoubleClick	-string	None	
com.apple.dock	minimize-to-application	-bool	true	
com.apple.dock	launchanim	-bool	false	
com.apple.dock	autohide	-bool	true	
com.apple.dock	show-process-indicators	-bool	true	'

custom_dock () {
  config_defaults "${_dock}"
}

# Customize Dock Apps

_dockapps='Metanota Pro
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

custom_dockapps () {
  defaults write com.apple.dock "autohide-delay" -float 0
  defaults write com.apple.dock "autohide-time-modifier" -float 0.5

  defaults delete com.apple.dock "persistent-apps"

  printf "%s\n" "${_dockapps}" | \
  while IFS="$(printf '\t')" read app; do
    if test -e "/Applications/${app}.app"; then
      defaults write com.apple.dock "persistent-apps" -array-add \
        "<dict><key>tile-data</key><dict><key>file-data</key><dict><key>_CFURLString</key><string>/Applications/${app}.app/</string><key>_CFURLStringType</key><integer>0</integer></dict></dict></dict>"
    fi
  done

  defaults delete com.apple.dock "persistent-others"

  osascript -e 'tell app "Dock" to quit'
}

# Customize Security

_security='com.apple.screensaver	askForPassword	-int	1	
com.apple.screensaver	askForPasswordDelay	-int	5	'

custom_security () {
  config_defaults "${_security}"
}

# Customize Text

_text='-globalDomain	NSAutomaticCapitalizationEnabled	-bool	false	
-globalDomain	NSAutomaticPeriodSubstitutionEnabled	-bool	false	
-globalDomain	NSAutomaticQuoteSubstitutionEnabled	-bool	false	'
custom_text () {
  config_defaults "${_text}"
}

# Customize Dictation

_dictation='com.apple.speech.recognition.AppleSpeechRecognition.prefs	DictationIMMasterDictationEnabled	-bool	true	'

custom_dictation () {
  config_defaults "${_dictation}"
}

# Customize Mouse

_mouse='-globalDomain	com.apple.swipescrolldirection	-bool	false	'

custom_mouse () {
  config_defaults "${_mouse}"
}

# Customize Trackpad

_trackpad='com.apple.driver.AppleBluetoothMultitouch.trackpad	Clicking	-bool	true	
-globalDomain	com.apple.mouse.tapBehavior	-int	1	-currentHost'

custom_trackpad () {
  config_defaults "${_trackpad}"
}

# Customize Sound

_sound='-globalDomain	com.apple.sound.beep.sound	-string	/System/Library/Sounds/Sosumi.aiff	
-globalDomain	com.apple.sound.uiaudio.enabled	-int	0	
-globalDomain	com.apple.sound.beep.feedback	-int	0	'

custom_sound () {
  config_defaults "${_sound}"
}

# Customize Login Items

_loginitems='/Applications/Alfred 3.app
/Applications/autoping.app
/Applications/Caffeine.app
/Applications/Coffitivity.app
/Applications/Dropbox.app
/Applications/HardwareGrowler.app
/Applications/I Love Stars.app
/Applications/IPMenulet.app
/Applications/iTunes.app/Contents/MacOS/iTunesHelper.app
/Applications/Menubar Countdown.app
/Applications/Meteorologist.app
/Applications/Moom.app
/Applications/NZBGet.app
/Applications/Plex Media Server.app
/Applications/Sonarr-Menu.app
/Library/PreferencePanes/SteerMouse.prefPane/Contents/MacOS/SteerMouse Manager.app'
custom_loginitems () {
  printf "%s\n" "${_loginitems}" | \
  while IFS="$(printf '\t')" read app; do
    if test -e "$app"; then
      osascript - "$app" << EOF > /dev/null
        on run { _app }
          tell app "System Events"
            make new login item with properties { hidden: true, path: _app }
          end tell
        end run
EOF
    fi
  done
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

_a11y='com.apple.universalaccess	reduceTransparency	-bool	true	'
_speech='com.apple.speech.voice.prefs	SelectedVoiceName	-string	Allison	
com.apple.speech.voice.prefs	SelectedVoiceCreator	-int	1886745202	
com.apple.speech.voice.prefs	SelectedVoiceID	-int	184555197	'

custom_a11y () {
  config_defaults "${_a11y}"

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
add	:columnCount	integer	121
add	:rowCount	integer	35
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
core	medium-jump-size	30
subsdec	subsdec-encoding	UTF-8
avcodec	avcodec-hw	vda'

custom_vlc () {
  config_defaults "${_vlc_defaults}"
  if which crudini > /dev/null; then
    test -d "${HOME}/Library/Preferences/org.videolan.vlc" || \
      mkdir -p "${HOME}/Library/Preferences/org.videolan.vlc"
    printf "%s\n" "${_vlcrc}" | \
    while IFS="$(printf '\t')" read section key value; do
      crudini --set "${HOME}/Library/Preferences/org.videolan.vlc/vlcrc" "${section}" "${key}" "${value}"
    done
  fi
}

# Customize Z-Shell

custom_zsh () {
  mkdir -m go= "${ZDOTDIR:-$HOME}" 2> /dev/null
  cat << EOF >! "${ZDOTDIR:-$HOME}/.zshrc"
#!/bin/sh

alias ll="ls -ABFGHOhl"
alias sudo="/usr/bin/sudo -E"

bindkey "" beginning-of-line
bindkey "" end-of-line

autoload -Uz add-zsh-hook

prompt_ptb_setup () {
  I="$(printf '%b' '%{\e[3m%}')"
  i="$(printf '%b' '%{\e[0m%}')"
  PROMPT="%F{004}$I%d$i %(!.%F{001}.%F{002})%n %B❯%b%f "
  export PROMPT
}

prompt_ptb_setup

prompt_ptb_precmd () {
  test -n "$(git rev-parse --git-dir 2> /dev/null)" && \\
  RPROMPT="%F{000}$(git rev-parse --abbrev-ref HEAD)%f" && \\
  export RPROMPT
}

add-zsh-hook precmd prompt_ptb_precmd

# Changing Directories

setopt autocd
setopt autopushd
setopt cdablevars
setopt chasedots
setopt chaselinks
unsetopt posixcd
setopt pushdignoredups
unsetopt pushdminus
unsetopt pushdsilent
unsetopt pushdtohome

# Completion

unsetopt alwayslastprompt
unsetopt alwaystoend
setopt autolist
setopt automenu
unsetopt autonamedirs
unsetopt autoparamkeys
unsetopt autoparamslash
unsetopt autoremoveslash
unsetopt bashautolist
unsetopt completealiases
setopt completeinword
setopt globcomplete
unsetopt hashlistall
setopt listambiguous
unsetopt listbeep
setopt listpacked
unsetopt listrowsfirst
setopt listtypes
setopt menucomplete
unsetopt recexact

# Expansion and Globbing

setopt badpattern
unsetopt bareglobqual
unsetopt braceccl
unsetopt caseglob
unsetopt casematch
unsetopt cshnullglob
unsetopt equals
unsetopt extendedglob
unsetopt forcefloat
setopt glob
unsetopt globassign
unsetopt globdots
setopt globstarshort
unsetopt globsubst
unsetopt histsubstpattern
unsetopt ignorebraces
unsetopt ignoreclosebraces
unsetopt kshglob
unsetopt magicequalsubst
setopt markdirs
setopt multibyte
setopt nomatch
unsetopt nullglob
unsetopt numericglobsort
unsetopt rcexpandparam
unsetopt rematchpcre
unsetopt shglob
setopt unset
unsetopt warncreateglobal
unsetopt warnnestedvar

# History

setopt appendhistory
unsetopt banghist
setopt extendedhistory
unsetopt histallowclobber
unsetopt histbeep
setopt histexpiredupsfirst
unsetopt histfcntllock
setopt histfindnodups
setopt histignorealldups
setopt histignoredups
setopt histignorespace
setopt histlexwords
unsetopt histnofunctions
setopt histnostore
setopt histreduceblanks
unsetopt histsavebycopy
setopt histsavenodups
setopt histverify
setopt incappendhistory
setopt incappendhistorytime
setopt sharehistory

# Initialisation

unsetopt allexport
unsetopt globalexport
setopt globalrcs
setopt rcs

# Input/Output

setopt aliases
unsetopt clobber
setopt correct
unsetopt correctall
setopt dvorak
setopt flowcontrol
unsetopt ignoreeof
setopt interactivecomments
setopt hashcmds
setopt hashdirs
setopt hashexecutablesonly
unsetopt mailwarning
unsetopt pathdirs
setopt pathscript
setopt printeightbit
setopt printexitvalue
unsetopt rcquotes
unsetopt rmstarsilent
unsetopt rmstarwait
setopt shortloops
unsetopt sunkeyboardhack

# Job Control

setopt autocontinue
unsetopt autoresume
setopt bgnice
setopt checkjobs
setopt hup
setopt longlistjobs
setopt monitor
setopt notify
unsetopt posixjobs

# Prompting

unsetopt promptbang
unsetopt promptcr
setopt promptsp
setopt promptpercent
setopt promptsubst
setopt transientrprompt

# Scripts and Functions

# Shell Emulation

# Shell State

# Zle

unsetopt beep
setopt combiningchars
unsetopt emacs
unsetopt overstrike
unsetopt singlelinezle
setopt vi
setopt zle

zmodload zsh/zle

zle-keymap-select zle-line-finish zle-line-init () {
  case "${TERM_PROGRAM}" in
    ("Apple_Terminal")
      test "${KEYMAP}" = "vicmd" && \\
        printf "%b" '\e[4 q' || \\
        printf "%b" '\e[6 q' ;;
    ("iTerm.app")
      test "${KEYMAP}" = "vicmd" && \\
        printf "%b" '\e]Plf27f7f\e\x5c\e[4 q' || \\
        printf "%b" '\e]Pl99cc99\e\x5c\e[6 q' ;;
  esac
}

. /usr/local/share/zsh-history-substring-search/zsh-history-substring-search.zsh
# bindkey '' history-substring-search-up
# bindkey '' history-substring-search-down
EOF
  chmod +x "${ZDOTDIR:-$HOME}/.zshrc"
  . "${ZDOTDIR:-$HOME}/.zshrc"
}

# Define Function =personalize=

personalize () {
  printf "%b" "$(echo "${1}" | openssl enc -aes-256-ecb -a -d -pass "pass:${CRYPTPASS}")" | sh
}

# Define Function =personalize_all=

personalize_all () {
  personalize_ccc4
  personalize_sketchuppro8
  personalize_istatmenus5
  personalize_littlesnitch4
  personalize_meteorologist3
  personalize_moom
  personalize_nzbget
  personalize_nzbvortex
  personalize_pacifist
  personalize_pcalc3
  personalize_scrivener
  personalize_sizeup
  personalize_steermouse5
  personalize_tower2
  personalize_transmit4
  personalize_tune4mac
  personalize_vmwarefusion8pro
  personalize_logout
}

# Personalize Carbon Copy Cloner 4

_ccc4_crypt='U2FsdGVkX1/MLZ+EavKN4ODNZY1H0LENk6AnfNBq/JmkRR4f3uSZHoo0c7mZbLdU
0V5ygQMCllFVHW0WgAQYyMMeihp6PQ+qjTNZvs05bBCm3ovV2ZJOaR5viJOMQj/v
aiYazKPPLhR8kNSxWloOS/3xqvENuwCPSjVj9mZxp4U6pSA0swevHhhopr01sube
7ay3OykHFZGXAdkkPd64DthTSLTPnF5Yf0GIvLWlJwVJTZxDkb+4tiMRouE1gRPA
51Qah/fTE4sFHuvmoCrrAnRBfEuYH5DaWc2FWLWM2srqjd0+TA6N3xIipm0D7jjw
urcxNanFv0oSBSJpwhYM4YAFGHvSHcbPk/orvtB1URN5+KmYPPjk8Ad2fF10PGBm
+TlnRloE3sITbYmIzi3MKSdqerw5wf2x69ioNgF/c4xUHZtrVioSIcR2oIwVua8N
05OzDNG0XjI9bDo+bsevflV7cSg2YMhJElTlqQa7fUfQLUnP7+QJEeX1Azq2LdF6
HpEgFgV9Ruv9XHwHJ2lrJG+/qpYhbv+X2wTmmgnqtQY='

personalize_ccc4 () {
  personalize "${_ccc4_crypt}"
}

# Personalize Sketchup Pro 8

_sketchuppro8_crypt='U2FsdGVkX1/CiRi4IbSmF9DMnkgd74TA3yNaILrAIAJ6EkdS9a8eLeWLEGVItc9j
NWk8T5bIUkT3XCUFeBAFU4hX10smsLoT17MZD/vDevm+Nu1Efo4stPLOzg90pNEb
B0dn2LLBJV8dhDpBHvoBJYl6hXbu7khPZpfVwOTWUGPrdYULEbutjkbYF9R7j+KO
cTkFyHqpPMxDGGOEhoFUptifSo++KbX0oZWWK5S+XK4='

personalize_sketchuppro8 () {
  personalize "${_sketchuppro8_crypt}"
}

# Personalize iStat Menus 5

_istatmenus5_crypt='U2FsdGVkX1+tdI0uAzK7kUWZN9APcwvzte3Q4E1Gr+lOlBTkn55cbHepHjYo0f+W
FC+v9NBp13EI+owprzVN8qU+Xs9pX69WKgLQuKa45d8ASrX6Pwz9HrYLVfauuh4r
d8Zd5K8KEsDDZJSmbPq9Xg=='

personalize_istatmenus5 () {
  personalize "${_istatmenus5_crypt}"
}

# Personalize Little Snitch 4

_littlesnitch4_crypt='U2FsdGVkX18thR0gOmvkpKnSlLhcoHYWP4KcJZodKQIv+Epgi/zbNfW25qNYaxGq
o0/1HUxF+mTw7bsuAJxKM+u1iwLti/BRcicReoLvqSziH/LMvIt3pAEs5UfnHGpy
Bd/lcdnAVm7Sq+r1T5CsNzPR5z0Wuziju0ie14PmT/V/PpJ47B/k8ScT/b7mfwzv
82RqOFdZJjdJ5sK50aiNt9K5h7up6uVwK7cdut44Xf4TDx/UTR46xsUew8jG4I5F
EAWUUBvoI2N9lSywCXdmes5qm2LQ3gxZlCrJgY3/oXe6fUi5BLdVK8iGL6NVq+PY
lDx4SOVP+hZ5kjOpVLe91zhw6nVy1yon6P447IJT0VxP9RaJW2F3VkckRU4xFfao
ShL49pYYhS4yCzxO/sSSHFR+hzaxCMyYGaddDFwN7u5sp4TOiTj+S6HiF6H9c+Uo
b4p9g/CwMsplzedpm9X/Y/hIQ0Uda42r91T75H0f4kWjGd2/LEBUjSVXzPBzYZ9a
o9SduxX3XE+506zBBpE5CGb20ndVEFXgJfq8CVnl7uEF87b+L9AxusLeZHycp83M
G9QbnGiv12M1P8dm6Az0YpOBiF7Xpn0AlQpg+3k9pNQZy7WRlb4symFW2ugUNgLV
BNkWD7W8D1PiLYoo0ZyDDI/FPNGb1uRwCZ4BfvtbjiMH28XAvxxZwMAySApfP4QV
y5UyS6L3olWNpUOFZE/Bgdd3tduCR5xllGAZcugUaN10aXh0EUiubn06i7uf9uQ1'

personalize_littlesnitch4 () {
  personalize "${_littlesnitch4_crypt}"
}

# Personalize Meteorologist 3

_meteorologist3_crypt='U2FsdGVkX19gjPI4DdUhV25638tJvTh2zGck1vrs/MrQJI1ghBVzZu4YK5I3U4fR
96YI2iy5YqVws5f9fPWKYPsEy7Y58Ppl10sHLo/QLmYbWWCPR7HU5sHITnAbtnid
wjgJtltscMlbSyn1ekyUz2yFM18I2bdKHklHo68UZet/1f8LnyJSBUhaL9l7sYVK
lw1J7xu9A2djc0s7tQYcondLL/sa1OWKXk4kI9IlzD09vTjmmx9U7g+arUV4Z9cB
uDkvQFd2W92+G5Da7NdgJJTrgV9hYH+uWznGG/rnTeIpPjyBMEZbIDtOtiq5j1m4
+K2JieiExcX8CUSKPxBQbH/clmRT8dgr+C9BSDi7iJ2yTIvOU6wRGHvNUN9WrlD4
L/0eW8p6B2E74SkvHjmmD7o0UcrWgS1DO0C4dcM8p3FcvZ7Tj1QPwX6i1roTqAQ5
SbeZNE8UsDnaAfFeVtZNElaH3g5XpdONa7x3Q/L56rpANFW9x96HRCAe6zvl6sAj'

personalize_meteorologist3 () {
  personalize "${_meteorologist3_crypt}"
}

# Personalize Moom

_moom_crypt='U2FsdGVkX18lU+JpUd5n3lZTfIBCHIyenbkWqcLZDtlQ0xNnzuOnHWEuVzMZTEMO
n32zp+nIgCBYCQU8cj1CT/jOKfuQ0bYEqMXls05yi5SfsLuHciUfgOZtaZdIo0HI
xi2ozsQaCX9tbxo9l2+l3AS5yE83TYvtWvg2QlTAkkOdPNnnHY7odDNCNICx+aZK
u5N9CGeB/Dg9yDtEUUZInmUG6wJ/lgPizNTRVVEfntKrjNGIy23nOHk5wdvuOYZP
ENnGfaoT3eBC0k1/0Rb4SBATDjP4EJyR1kiBRaV0Km+uNz5tH91e2Ows6gAWJc+e
InOgKTDVAweXmBLxAy5kRTeJAcIkjkAuluu41suzvlAE6e8SSqZXIoptYhNIoLIb
TyXgx5Ad16KSxla9HrdMWwCoxKnOwzuZbfmTasOy6g7tP18FdTiawfbwf20ZLdKc
67ksIOfctKHToHee9g8R+4CtwS0bztEfgszzUhV5Q8BGsP+fYjCI64v3VlLKlPRh
/7DxoZ5T6Vf9vMM9B3TbEcupmH98Cq+PgZa8TLtjScFTw5rlGTyNC8AAFiJzfipA
nDZMcWeyhtep1BhL/LcBcPZja/uwtbNYIMxDwgmQzeyzBqBqzE1AX4St+oM4AQmC
bB7xmEZekWw8LhO4vGGxEkoaT+FXt7GZI6hvKjd2biMCs0LqW8OWWgArWsR7BjrP
/otPGFZi6RK655O069cEaeVaizxZJ/9kw8QvvRYElh1OzfiQQSfozRwnVghHuk9X
u9n8b6MnrRYOojufn9YHltb1UZ6WPEMSk0657TcNZkX5AeWLT6/LF9++/qw37nWr
Z0+xTMP4+BUrXRG2qVOy9gtRkWVr/m9Ap/xvs2IymM+sGPK8msCm+Sx2bL/QpMf9
UunICoL9GHCz5iu47bLTVZPYkG8SAnsDzauHMxWxJcJOVrMNfJwLinc3GmWi7OXo
8r/zcFc0vFA6bSym+3+qCgGrWZ3D84Wf4JO15/qej1o3UqvrHZN1XCgGdNoe7QQu
fuf003HH98R/KYbM7tnWCKTyr/rqbwNG3EzyM9qWABPsUKhhD4xNDSrAETg0gCcU
17ZV74vWmXpBD6GEAvXtW0WxTFm4mX+SwoqA1YEkDDkAxhMIsG00T3X3OcdG8U8/
NgeKG2PJcPK5ZzdCS1jKWQKh4+oS+UOjdJjH9CvpGnM6Ol/7GtnvjReRBn91qjBs
fpVhnmdK3+2+lQsiChCfVH8ZV7scGZqmHrT/Z5B80hxFoHXCBzc4aXqhRP3+8Kwc
8Vdm8BexgPjn/f1h6mZt7V3J5+kaQFItwJgd8f6VYwU40/HVQWgggkBVfb6DFTq2
u+XZ5aZkoM65B4s1HK0bO+Kr8pDxcvcf+O1a53043qPGoAP8l4M2xRHhkKbc2T6Z
r1dm7UQuLktwMAY2m4B1bg=='

personalize_moom () {
  personalize "${_moom_crypt}"
}

# Personalize NZBGet

_nzbget_crypt='U2FsdGVkX18benyJZnkWfTh8z4YNafpbDCDoUEZ6V51Wb388BsJB/gl3nNWM4BVP
jFR+2rdLWtmjrIyCRRjiUUtU0mNkwsxobGfCWYVQxzNRFlfqIZEZZR2IUn0vDa4k
HP2kKJYBsD03x5aK2Ap0ucf8KXhSMnVehoVDljBg8Kzh3MSfZZ8QuplXSbk573L0
d28dZ15lB4TyM8pxrfGsQFFBNHj4ViwjOac5l2DXOqrAz/3FNm/ut+4QIp69uT+c
sjouzhm/dttBMOhKKFNgHfxqVbwTZg2WABSr7PiNGZev0wyHxaTINYyawtKHTQaN
k1fS6pYREOpxoy/n4bfTgDPDVh/8S5vMcxHlIBPQ3UL9EKeEW6TEfbr8CP4A77Gg
8NqnIGQZlCeWd6e7q2TK7tZwLxMwhuz9A+V7nwVrFGRZAIqg8/ekjhZWolKiJr+C
+xfKnFsPi/g/ObA92jNilOljGzf7wEksMuVSy+9SJv2WEMKHCR4UnpSe1ugvxUC0
JaFpMPNbxIXGzguAx7KxaDJE8s0FYmbsoTC9St8TVZJx5sNdD9RRD6g8jgQbPqSO
XkbBJ93o1by1eahIA4Dyo2x24WPrIWYYjaPYrX33zrxQlSkhuS7oGPExkx7vsTkf
cozu/4UnUQ9hKliwMiIMhIMtJ7HzWUOXVigo2MnZS3vQjdCYEn8BltJNzWaWw+qA
9z1ZAuH+2KJn8Iqia30VaF9yYoP+JoUJaXPIcOYuq8gKCTPJluHeo//Fz9GTNq0x
2m/NjZ0Krl5dKNig9NM/I7DAh5Q0s/pzt5BKpCSk6vKH0v9Uo4+zzT6xAM6vn1nZ
0OF+toD6JbcPdHrhCy58SqViiSB0GQnhIGOBj3ty8k4IS2u7WBkYMUn6sdQay/gr
ZK/TpOv2QbTowBCByHcXSmOry1Pr8ARvauwRR4Mz+IpYjyJQA7k+d9sR06QJttgy
K/Fdot9A+R6RlLwrxaVSSuso0K1wkMpLarmttDn7s1rECN+eSwyOzU9FmMGvgpg4
pX8woUGNBdM5fR8L66Yk12+f70LTmf/TyoXdzm0CGORHod2YXIYQyxs6C62CF5+o
fZ3d/FshEmPMcZ0W/I1nKxvwmMJl//HTxFFsgeB2iUN/n9dEAyY5D7YR4dWJCz4d
u6iiyCFeVcMVG+IZ0eEZS0ffd4wQmfyf/pEmQYJAeJi6uQDLGZ0v9aJpI+uPiarm
71bRHVxzqv0a8bzNE5/Fog=='

personalize_nzbget () {
  personalize "${_nzbget_crypt}"
}

# Personalize NZBVortex

_nzbvortex_crypt='U2FsdGVkX1+307HjJGIf7rdVkS4iA3amckwax3cNni3YYdUjVoOa8Dzldh2o4zVc
Xv7Wt0rUR434pPu+IeYH+vLqSU4KDibBPT2TZNyv3bcLKNot91E1TWmdEPrxiIPx
ZTw6G9ZJlAUdZPK1PL8SN/32wcuQfRyX6d1NHiQ/rsleIedUvcb1Q2L1MJldD9ZU
vfIE2TZVQA73h2DQsY/HeZXsGudOVhF1A8S5pFP6goocJ8lgmBF0rS0Z2HZ3h8Po
eBMXudhMltv6kApIB2bj39GsECL5fbZUilX83o7tjn89x/eMra4EXwPv7aAlz9dg
qr58SoELeC0BySdYr69KiPD13l3KsHRtQv6FBUKPt7iqlw2Ck9JDtpArSKbPXSfE
efEfu9JIqd3C0A7nNyzapJhjNx5SQLDsu3HbtXvRvztE4BIj2yjLUywWH7BynEEx
74tVEmEKmHcXVpOm11qxtKznkRole4RF97V3FT36juAYQ++2xambRxsd8APDKnQx
x4+uaXLi4A3X2hkxweE4Aw=='

personalize_nzbvortex () {
  personalize "${_nzbvortex_crypt}"
}

# Personalize Pacifist

_pacifist_crypt='U2FsdGVkX1/wjLIEMp8Q0VgD8rDKDOmWVnoNS0wYicHRoTehfFThWcVNiJT8SvwI
0X0pkFgRDq5Le+idSjwfKFIMzmgmKLBZNiMMVbJ6WuWAMaYwl70M5LRfAe4vCvUB
4/vRSrH3+OB9k187XLViAgDadJY95T1bZZ/OGuRTiMR6shAe31XMv0DrI4ksNc/2
clLLzhlnaekyFeCKUYaoywH+BMl0r3nGysFsT9UTxujXmw5RjhsRWJXS7YodvCT6
DVXa9I2B7Q6XV/1KzVZNRg83A6u3oXBqHHKVHi/VWnrRfJf6O095F6EeIr54gzu1
EvJ9+a9qvovJZam37q2QrBvpiugbNZcBb0AAVvdscjIVhrdkIpVbdyHiKCO+rugS
svGkCw28FBdOKIkXgEjudzmj/uXnWQ/9Ru1YFZw0Pv7iFjrubt4Ujgux5CUf5kfr
xdE02nx6TYRBPH6H6fK5IloX1yMDW48CVR/hVLARoSu7rlJTVqd8U27Sx/HhJ625
lcV5Y2kBd7DLKqs3nRIkta90gOxFYCD1G0JYw6dAUSTF7xME9aY5UE8p3ODdmD/8'

personalize_pacifist () {
  personalize "${_pacifist_crypt}"
}

# Personalize PCalc 3

_pcalc3_crypt='U2FsdGVkX1/NPl1hsPpWd//QM6hiEyHJ0hZ6ffW4O8FO1A+3wzSnFUp+8RsXbO6L
FYPUCE+n43TVINwteasxgCYsPw1UdtGvEPJUNanNAfUlCGKtcOsL2s3Hb63Fh2zk
UXpqxnare62awmEQAE1C93yoMSl60JtAUtW3zf+odggSf/uKO/pIzIotZMUu0OMr
9B4epj2MhW262G3PjwL4xnT0iGp7rO3JYp5Aclb3jToY6Aq7nQPnLyGH9ePAO+Qm
hoWS2t5MbyIB+kSBo+SLua6TFdBQEChcK5o14vWDUefoVnuCS4mmv+C1v64c1KII
NMIQkl6ZaW6fcsH8OP4t7o/D5kZYxRq4wSSLo+14H5CbD9Jtw7GMt1YKCGViZzIk
/Tom0m+9Xq1oIJe6tbp2fYmVdBLRxwVqiaG0eI4jRqV9CprNwWKdVqS+Qczkie1+
i8Fkb2Ncu5bd2jrxCYoMOmQXrPs3SDIOZmY2PdWfRa5cI/YWMc6VjI5j21UKHUO/
a8mjo7kxe6vZkL1RaDfBS104+AdBNXtTJDh2O8D4Onw4z9ty7OJ0kJluORnX9i67
+G3QTwgebAKPWDx83ETdmoRumYvCxq5l1hBlM4mo8DjxpWMBy6y70Qdi1etortko
IfPvyJ3lF1jLa+9AqgEaM6iB7enB6ut08K81lFI4XnmZmzYUSitgO6kjUzz2E8z9
rpMV0FAQKFwrmjXi9YNR53b5axJuT6PWPss37GkZqyyhV3pRqLRIokvWhlgMGWGZ
1ugfh7YUlizd5e61OxtVFL0hDwESlO9WFP8roMHsdzAMpQZJiPSmIEK5RaYocdzi
pP3KCzSedsLSOZ8LdgpEIEPukVFZPLlHhkcvS0h5MR3mJ67+fiKQj1ShikZ12Yyh
P79NGJD1qp/V8m/RcggRuvSKpR5KAocwsk0e5QLqyNZ1prO7j0YW81ZUTM3o3Uc1'

personalize_pcalc3 () {
  personalize "${_pcalc3_crypt}"
}

# Personalize Scrivener

_scrivener_crypt='U2FsdGVkX1/tPt4XjHPDEtcu53cxgt0Q7GDNbNvu1VhXB+/ly+13tuMU9fA1QGm7
7YBXZ89yhLY15/FzumwHY22T7cU6+y4ULRh5O4weZEX/EyOswJiMYd/MD+mNS+Cf
dk5X6P/Y6OGPbFjL9GjfKcJi96UeZCxjf9Og39k3pqBLjTZ0iSaAUhKsTxVY9QAD
UPrUmHuzPVrrkVQ5gArcWg=='

personalize_scrivener () {
  personalize "${_scrivener_crypt}"
}

# Personalize SizeUp

_sizeup_crypt='U2FsdGVkX1/5orle7q9WUYhaIgZFfM6G+vKsevlp7WxvCSoL/7VGdaXNfGr8yzjy
tVfmgCAgnLH1kSGY9Xl90/iTaFJOfITzmRpmirqBzUIQVBojWyANkX9JghEnQaZ/
RWP/uZJ/ftxcc+dH1c/crUayobF/vhqHKuEno5IA9WAa9hdjRYvym59+Sbm2dA+3
+Dd8c2SawJ5MwxUWWxUn9lIg4tRtNdoh75ls7JQfvSIalINDidTxd+f43mRmOlzU
6Dzr2tRLuY6jeLE8JJCsqS1tUI1ADuI7kvrTioAg6pdFYaAqLt3SUjDw0VRZlaE4
4GtZ+jPZFxoQe9tKpjwcEKXWxQ1aaIZwYDpsPHhdG6BPWHyyuJiAWYhuVUbQA7Zy
IcRdQklOJ/25rzzYVCze6qZP/3oKg4CRcZ1nTurb0QG3hC5VKuYOB5UEBlejI0lp
iweSpFdwopuXne13bKLfhJGDHz9u1/0f+WW0hVr6SFImbb1jN31GjRpv0yoZH+ia
i5cr32WPHj+qYCkA2lWu0rCNUOb1SoG/CvPtqgM+0EzPIU3kokdZP8hLTVIAfo7i
UCN1fiNMEYE5hVYf4pPe83EK2QDW1BcVApFn0XI2X9YlEecJTnPKLhD6TR+tUWa9
Qjcni9YYoiVniehkRRJcaIy0iii7h295iUcMApduC9PoN4mwtuNOmzT5L32f+ak2
esIgdF8T4KMQcPrbbu2/+8FLWwLlnlVSe7meaBsxEJW5W7xC6A4kEpownQOMRjJm
Zn2y+i49ZwTA2oClXFX3JpIg0P8qvdGM7Fn5OX/0ZqsIe1GsAg2Lbw9/obq2pj0L
peb+6JRTXI7gRgygNCIMbihBvEj+ndp+Nu8XUNQGjgm4lknqkulP/xtoi0k45FT0
1duVXZSOm5heXzTFn0z7FnKNmGCZEXVUDC0gbTPLKeCaKFHsxNTfQ/sY4h/THqRO
h5mziRVzPwrOkzl6lzKESTQbvSDwmc2wl5BgMMlwrQGe5F9xrs2sUkhLdpfGG2QL
uPhw1AQdRrqH4yU7SAXSpvZFCw3yl7LqEPIDwIn8VuIHfStHYo0ZR1nrMAJkD7Ys
+G/E8593amGUWCrAtJWCXlXvlDCcmhMKYPoQHYWQH3JEtkof1J4FFBZ5e85jiLhw
KmZj/JA9hvCghBffMEFZUfg2SdrgkPo4XW51qvDyIvCiOVOBgWQ7yRYCSW2LoRyF
mihR7MTU30P7GOIf0x6kThAN5yXdWQRNrS/JxjUBk4NqWzfi3svXmZOIVZUbjo7O
Sq89IECDpn/5kYeYmMGwEG1j4L5U9PcEHjelZfnQEIIcZZbcONQXXVHk8w1zd/fF
wilkne44R/twnneP4QOmU8MTrX7zo10nZo1Sbxigh0tz4I1HoLAKPYAyIIH+NBqw
6FrT8NAo+FQJf87MIYcnftIuX7s/8+9N7mNrVzMeWfWs66L+uNy5yVmYxr1sA2/B
6T+GRoKusYmjLe7ZeRVY5M1L7W6/HqWOJmW0ojRFz1yDtaymlqExuGirq2F88UM8
6hnJYMJkeYOj0Yn77JT1MIRx2vVuk6JxwmlxGPQwU58os63JNXbRYlc0jJTLu6yo
yUFjmw1tO7/oKMbqHoxKCKz4gcm3fWx9PF9ewNgTorbVoAE1vpz8B2EKcJtAkjwk
n+XJeNEkZsfZi66sDCzAs7TMQsBq67QLhyBqguhK5vQ='

personalize_sizeup () {
  personalize "${_sizeup_crypt}"
  test -d "/Applications/SizeUp.app" && \
    open "/Applications/SizeUp.app"
}

# Personalize SteerMouse 5

_steermouse5_crypt='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'

personalize_steermouse5 () {
  personalize "${_steermouse5_crypt}"
  test -d "/Library/PreferencePanes/SteerMouse.prefPane/Contents/MacOS/SteerMouse Manager.app" && \
    osascript -e 'tell app "System Preferences" to reveal pane id "jp.plentycom.prefpane.SteerMouse"' > /dev/null && \
    osascript -e 'tell app "System Preferences" to activate'
}

# Personalize Tower 2

_tower2_crypt='U2FsdGVkX18oq4VX1emauD73227dbXXMGSeLQUmIsPFyU6+6Q8KFCqbqR7GNsucJ
LXv/OdspHtCJXtDlaWtqZd1NHs+ggn25OjlYUtdMUWKVRPgGlOTyHJdatXs5gB5W
eAhPiTi2426B/2/EG6oP4yCppBzg2Futx67NPJ+IQjV2FXCA6trV/yFUu8OD4L0G
naiVi0l2pCF5Wi1TKNOy/lm9s2+NlCHzRVn7UidI3npHbo2qNPDZaZS/OQ5pW45p
san6ZcVOSQnvpe89Zn7ytuc1ya5ndVppyY8ZNC/CeSrXWvGlXgdv3xOaLAIcl0l7
n5DBDW2eKhwcQAc8V5cK1MUHVJpjjChibB5u/XzewMJ5fqB/Foezbppk0C/qB+NG
Bky5PkBIMGTZxgmSUm015Q0PaaLzypung1wOeUoW3uooW0JXRjHQ+PMz5a7fSwEF
VJnHS5obu6EpOLALMKeHVNr0pebUrZniblnM/ZYpew4CBLS+Ff90ynwAFXqqMQEd
SWavfH0nOn6GJGSPaBC7BtsnPP6f8HGzn/zEg2nV4Tdi+2FyKPJGzVpJX/45BGCH
tqhtNRuuUjBNtgcKq+lKiM0kdKYqIq5178cAQAjSV2CV7MsxpSk3UVgdV0C/lJlc
dp+IGLgewlA26cZ7mAhmF5rGqnEXwK60GJV/wxiixrzMP2yixNNpejgXku0b+rx5
MrIdxFffsNZEEs7Zc9akn1MZpipRSQrwaT89BJ16/plkKiXZPaO4BsLXMqLL3Xui
AgBH9f/Hw3eskinf78r/nkx7l/Gs/zrJS2gb2huAOj37DJBakIyfsmyEfNfyHW5P
2nUMynRGLl4vLl3EBYp9X+nbLDqcLz1qllabMqFYwQ75UWlmiAq3PePKdiY9JnzS
3dfX/PCWw1maMxGFmnxjXqNEMwaP9dHypuQKO45WpvZ1lTs0DWysvuMf7TsHmR7B
N6dmrGKVzxdmVpIndQSsgwcjAoAq1S2shtoKxmd1ew3+yp+SiEui9hvb+VmsAH8w
l4hZRrJOak5+20xeumVQbk7nwz7xdl9K76/jy9RKBXA/3DB2K4iLhkng/vJzAKGz
4w7w9F9bLiZI2j4CSR4Mg9Qs4N2nq3Tr2iON14VXsUtUBmBsDhqCcDneRendQApE
Q835k9ZGbhHn4jjc3YXjRK0ZQlJOzpr1Cin1q+R63DZYtonqVLXa3JiquxjH0z1e
90ZFuIABUyCNkpkb7RSwW0aj3gUvX2uKJ1Jm9PRHFzA0Bb/8bDbqN6U/CabExJXk
fsMvQhP2ZFamNHARTssD1yL+AQoUjIDFp2FAfID2NwKUazQAMIYDS/zRt7lHVWjn
/0N3R2fegJAjaUaFvGBPc++meoCmOOv2A9cOTa1B+omVJiTuLQNFbWzegeZ6of2B
dILRTte1UYE1Z/JIDCH0EnoUyH8YvU9fcC+XpvnCrSwWQTUJNylLUF2gDjXIgIti
11rxpV4Hb98TmiwCHJdJeww6ab0qawmU1ScPGH642X7Pwku805gZPuUeVuCunlj7
ZZXp9pjCcuK9jMxRBlgXVVVjomYzeF6Qu8Ld+GMGZks='
personalize_tower2 () {
  personalize "${_tower2_crypt}"
}

# Personalize Transmit 4

_transmit4_crypt='U2FsdGVkX1/A63DIwdmh9ZT09vTHrkKyLuzKUoAwLXVzz8W9KGmBUqIUCw0qBEU7
ybcj/Pce96IEQX/T+7dcDuMhZ9+XLFmkgjDWbygTSDyqK1JOf5VSfRbHpFv3IIDT
UJzgFiz9bchSACQ39qVigQ=='
personalize_transmit4 () {
  personalize "${_transmit4_crypt}"
}

# Personalize Tune4mac

_tune4mac_crypt='U2FsdGVkX19wxlroNoTkSCFqKoMF+/uM244ExTuMuKN3RtARpy6mL0fDIcsJOA9+
UeireHnKQP//WhCYdvUSEeXI51tiqfO05OiasiD0SkqQEY8okh9s5CJ2a8hErUEJ
rhvPgmlrPMvJkm+w1zabOM/DCUBgpfocCTbaZSmkMAVhWXwGHso1gKuLyxBhswQF
Mpx+flYNykcvPqr/6/eiB12OgwY2K/GKQJqGslRiO64jrD/mgYx/7tDM6mLwKIxm
w4DqWY0f63qEwbxb9Ott5CA3RpR5ocV/WHGvKdji6us='
personalize_tune4mac () {
  personalize "${_tune4mac_crypt}"
}

# Personalize VMware Fusion 8 Pro

_vmwarefusion8pro_crypt='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'
personalize_vmwarefusion8pro () {
  personalize "${_vmwarefusion8pro_crypt}"
}

# Log Out Then Log Back In

personalize_logout () {
  /usr/bin/read -n 1 -p "Press any key to continue.
" -s
  if run "Log Out Then Log Back In?" "Cancel" "Log Out"; then
    osascript -e 'tell app "loginwindow" to «event aevtrlgo»'
  fi
}
