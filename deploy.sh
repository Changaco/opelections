#!/bin/bash

set -e

appname="opelections"
domain="opelections.oy.lc"
ghc_version="7.10.2"
libdir="lib/ghc-$ghc_version"
sandbox_libdir=".cabal-sandbox/lib/x86_64-linux-ghc-$ghc_version"

remote="opelections@changaco.oy.lc"

# Build
rm -rf static/{combined,tmp} 2>/dev/null
cabal clean
cabal install

# Upload the app and its dependencies
homedir="$(ssh $remote 'echo $HOME')"
appsdir="$homedir/apps"
ssh $remote "mkdir -p $appsdir; mkdir $appsdir/$appname.lock"
trap "ssh $remote 'rmdir $appsdir/$appname.lock'" EXIT
logdir="$homedir/logs/$appname"
destdir="$(ssh $remote "mkdir -p $appsdir && mktemp -d -p $appsdir $appname.XXXXX")"
remote_libs="$(ssh $remote "mkdir -p $homedir/$libdir && cd $homedir/$libdir && md5sum *.so" 2>/dev/null | sort)"
tmpdir="$(mktemp -p . -d)"
mkdir -p "$tmpdir/"{bin,$libdir}
install -m 755 -D {.cabal-sandbox,$tmpdir}/bin/$appname
cp -a {.,$tmpdir}/config
rsync -a static --exclude static/css --exclude static/js $tmpdir
cp $sandbox_libdir/*/*-ghc$ghc_version.so "$tmpdir/$libdir"
for f in "$tmpdir/$libdir"/*.so "$tmpdir/bin/$appname"; do
    rpath="$(
        objdump -p "$f" | grep RPATH | \
        sed -r -e 's/\s*RPATH\s+//' -e "s|$(pwd)/$sandbox_libdir/[^:]+|$homedir/$libdir|g" | \
        tr ':' '\n' | grep -v $(pwd) | sort -u | tr '\n' ':' | sed 's/:$//'
    )"
    out="$(chrpath -r "$rpath" "$f" 2>&1)"
    [ $? -ne 0 ] && echo "$out" && exit 1
done
local_libs="$(cd "$tmpdir/$libdir" && md5sum *.so | sort)"
libs_diff="$(diff --unified=-1 <(echo "$remote_libs") <(echo "$local_libs") || true)"
if [ "$libs_diff" != "" ]; then
    libs_already_uploaded="$(echo "$libs_diff" | tail -n +4 | grep -G '^ ' | sed -r "s|^ \S+\s+||")"
else
    libs_already_uploaded="$(echo "$local_libs" | sed -r 's/\s*\S+\s+//')"
fi
[[ $libs_already_uploaded ]] && (cd "$tmpdir/$libdir" && rm $libs_already_uploaded)
tar -C $tmpdir -caf $appname.tar.xz .
rm -r "$tmpdir"
scp $appname.tar.xz $remote:$destdir
rm $appname.tar.xz

# Run the app
ssh $remote "bash -l" <<EOF
    set -e

    # Unpack
    cd $destdir
    tar -xavf $appname.tar.xz
    rm $appname.tar.xz
    libs="\$(find $destdir/$libdir -name '*.so')"
    [[ \$libs ]] && mv \$libs $homedir/$libdir # FIXME this is unsafe

    # Get a port number
    old_dir="\$(basename "\$(cd $appsdir/$appname 2>/dev/null && pwd -P)")"
    old_pid="\$(cat $appsdir/$appname/pid 2>/dev/null)"
    old_port="\$(cat $appsdir/$appname/port 2>/dev/null)"
    new_port=\$(((old_port + 1) % 10 + 8850))
    echo \$new_port >$destdir/port

    # Try to launch the app
    mkdir -p "$logdir"
    logfile="$logdir/yesod.log"
    ./bin/$appname Production --port \$new_port &>>"\$logfile" &
    new_pid=\$!
    echo \$new_pid >$destdir/pid
    tail -f "\$logfile" &
    tail_pid=\$!
    i=0
    while true; do
        let ++i
        sleep 1s
        if out="\$(curl -f http://localhost:\$new_port/ 2>/dev/null)"; then
            kill \$tail_pid
            break
        fi
        [ \$i -gt 9 ] && { kill \$tail_pid \$new_pid; echo "\$out"; exit 1; }
    done
    disown

    # Tell nginx to switch to the new port
    chmod g+rX -R $destdir  # allow nginx to serve the static files
    sed -e "s|DOMAIN|$domain|g" -e "s|PORT|\$new_port|g" \
        -e "s|LOGDIR|$logdir|g" -e "s|APPDIR|$destdir|g" \
        <$destdir/config/nginx.conf.in >$destdir/config/nginx.conf
    rm $appsdir/$appname 2>/dev/null
    ln -s $destdir $appsdir/$appname
    /usr/local/bin/nginx-reload

    # Kill the old process and remove the lock
    [ "\$old_pid" != "" ] && kill \$old_pid
    [ "\$old_dir" != "" ] && rm -r $appsdir/\$old_dir

    echo "Done"
    exit 0
EOF
