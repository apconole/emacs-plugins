#!/bin/sh
#
# This program is Free Software
# Licensed under the GPL

set -x

if which yum 2>/dev/null; then
    PACKAGER=`which yum`
    REQD_GIT_BUILD_PACKAGES="curl-devel expat-devel gettext-devel openssl-devel perl-devel zlib-devel"
fi
if which apt-get 2>/dev/null; then
    PACKAGER=`which apt-get`
    REQD_GIT_BUILD_PACKAGES="libcurl4-gnutls-dev libexpat1-dev gettext libz-dev libssl-dev"
fi

if [ "X$PACKAGER" == "X" ]; then
    echo "Unable to determine packager. failing (set PACKAGER variable manually)"
    exit 1
fi

if ! which git 2>/dev/null; then
    echo "First, getting a base git..."
    sudo $PACKAGER install git
fi

mkdir -p ${HOME}/bin

# Get the initial prereqs
GIT_VER=$(git --version | cut -d" " -f3 )
NEED_GIT_VER="1.9.4"
CHECK_VER=$(echo -e "${GIT_VER}\n${NEED_GIT_VER}" | sort -V | head -n 1)
if [ "$CHECK_VER" != "1.9.4" ]; then
    echo "Upgrading git..."

    sudo $PACKAGER install $REQD_GIT_BUILD_PACKAGES asciidoc xmlto
    mkdir -p ${HOME}/git
    pushd ${HOME}/git
    if [ ! -d git ]; then
        git clone git://git.kernel.org/pub/scm/git/git.git
    fi
    cd git
    make && make install PREFIX='$HOME'
    popd

    echo "Upgraded - ensure ${HOME}/bin appears first in your path..."
fi

