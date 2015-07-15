#!/bin/bash
# CHANGE LIST:
#   - 2015-07-14, by wuxi, create.
#
# usage:
#   $ sh ./install.sh
#

set -o pipefail

MYEL_ROOT=$(cd `dirname $0` && pwd)
REQUIRED_EMACS_MAJOR_VER=24
REQUIRED_GO_MAJOR_VER=1
REQUIRED_GO_MINOR_VER=3
TS=`date +%Y%m%d_%H%M%S`

# usage: echo_info "msg"
function echo_info {
    echo -e "[\033[1;34mINFO\033[0m] $@"
}

# usage: echo_err "error msg"
function echo_err {
    echo -e "[\033[31mERROR\033[0m] $@"
}

# usage: echo_succ "error msg"
function echo_succ {
    echo -e "[\033[1;32mSUCC\033[0m] $@"
}

# usage: check_ret $? "msg"
function check_ret {
    local ret=$1
    local msg=$2
    if [ "x$ret" != "x0" ]; then
        echo_err "$msg failed: RET=$ret"
        exit $ret
    fi
    echo_succ "$msg ok"
}

# usage: update_gopath1st_env <bashrc_file_path> <GOPATH_1ST>
# eg: update_gopath1st_env /Users/wuxi/.profile /Users/wuxi/gocode
function update_gopath1st_env {
    local rcfile=$1
    local GOPATH_1ST=$2
    sed -i$TS '/GOPATH_1ST/ d' $rcfile
    echo "export GOPATH_1ST=$GOPATH_1ST" >> $rcfile
    echo_info "append GOPATH_1ST into $rcfile"
    source rcfile
}

## check emacs version
echo_info "check emacs version..."
EMACS=`which emacs`
check_ret $? "emacs installed"
EMACS_MAJOR_VER=`emacs --version|head -1|cut -d' ' -f3|cut -d'.' -f1`
if [ -z "$EMACS_MAJOR_VER" ]; then
    echo_err "can't get emacs major version"
    exit 255
fi
[ $EMACS_MAJOR_VER -ge $REQUIRED_EMACS_MAJOR_VER ]
check_ret $? "emacs major version(=$EMACS_MAJOR_VER) should >= $REQUIRED_EMACS_MAJOR_VER"
echo_info "$EMACS $EMACS_MAJOR_VER"


## check go version and env:
echo_info "check go version and env..."
GO=`which go`
check_ret $? "go installed"
GOBINDIR=`dirname $GO`
GO_VER=`go version|head -1|cut -d' ' -f3|sed 's/go//'`
GO_MAJOR_VER=`echo $GO_VER|cut -d'.' -f1`
GO_MINOR_VER=`echo $GO_VER|cut -d'.' -f2`
if [ -z "$GO_MAJOR_VER" ] || [ -z "$GO_MINOR_VER" ]; then
    echo_err "can't get go major/minor version: GO_VER=$GO_VER"
    exit 255
fi
if [ $GO_MAJOR_VER -lt $REQUIRED_GO_MAJOR_VER ]; then
    echo_err "go major version(=$GO_MAJOR_VER) should >= $REQUIRED_GO_MAJOR_VER"
    exit 255
fi
[ $GO_MAJOR_VER -gt $REQUIRED_GO_MAJOR_VER ] || [ $GO_MINOR_VER -ge $REQUIRED_GO_MINOR_VER ]
check_ret $? "go ver(=$GO_VER) should >= $REQUIRED_GO_MAJOR_VER.$REQUIRED_GO_MINOR_VER"

[ ! -z "$GOPATH" ]
check_ret $? "check GOPATH(=$GOPATH)"
GOPATH_1ST=`echo $GOPATH|cut -d':' -f1`
echo_info "$GO $GO_VER GOPATH_1ST=$GOPATH_1ST"
#update_gopath1st_env $HOME/.profile $GOPATH_1ST
#if [ -e $HOME/.bashrc ]; then
#    update_gopath1st_env $HOME/.bashrc $GOPATH_1ST
#fi


## prepare emacs env:
echo_info "prepare emacs env..."
bakdir=$HOME/bak_$TS

if [ -e $HOME/.emacs ]; then
    mkdir -p $bakdir
    mv $HOME/.emacs $bakdir/_emacs
    echo_info "backup .emacs into $bakdir"
fi

if [ -d $HOME/.emacs.d/ ]; then
    mkdir -p $bakdir
    mv $HOME/.emacs.d/ $bakdir/_emacs.d
    echo_info "backup .emacs.d into $bakdir"
fi
mkdir -p $HOME/.emacs.d/


## install go plugins:
if [ -x $GOPATH_1ST/bin/godef ]; then
    echo_info "godef exists, pass"
else
    echo_info "install godef..."
    go get -v -u code.google.com/p/rog-go/exp/cmd/godef
    check_ret $? "install godef"
fi

if [ -x $GOPATH_1ST/bin/gocode ]; then
    echo_info "gocode exists, pass"
else
    echo_info "install gocode..."
    go get -v -u github.com/nsf/gocode
    check_ret $? "install gocode"
fi
cp $GOPATH_1ST/src/github.com/nsf/gocode/emacs/go-autocomplete.el $HOME/.emacs.d/

if [ -x $GOPATH_1ST/bin/oracle ]; then
    echo_info "go oracle exists, pass"
else
    echo_info "install go oracle..."
    #go get golang.org/x/tools/cmd/oracle
    go get -v golang.org/x/tools/cmd/oracle
    check_ret $? "install go oracle"
fi
cp $GOPATH_1ST/bin/oracle $GOBINDIR/


## install .emacs and .el plugins
sed "s#__GOPATH_1ST__#$GOPATH_1ST#g" $MYEL_ROOT/_emacs > $HOME/.emacs
#cp $MYEL_ROOT/_emacs $HOME/.emacs
cp -r $MYEL_ROOT/_emacs.d/* $HOME/.emacs.d/

echo_info "install .el plugins from emacs package..."
emacs --batch --script $MYEL_ROOT/install.el
check_ret $? "install .el plugins"


echo_succ "all done, have fun!"
