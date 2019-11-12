#!/bin/sh

set -e

git stash

boot production

git checkout master
mv target/index.html ./index.html
mv target/main.js ./main.js
mv target/style.css ./style.css

echo "sphere.erdos.dev" > ./CNAME

git add index.html main.js style.css CNAME

git commit --amend -m "release"

git push origin master --force

git checkout development

git stash pop
