set -e # exit immediately on any failure

rm -r docs
mkdir docs
cp result/bin/example.jsexe/rts.js docs
cp result/bin/example.jsexe/lib.js docs
cp result/bin/example.jsexe/out.js docs
cp result/bin/example.jsexe/runmain.js docs
cp -r lib/* docs

APP_PATH="file:///"
APP_PATH+="$(pwd)"
APP_PATH+="/docs/index.html"

echo $APP_PATH
