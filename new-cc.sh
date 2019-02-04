#!/usr/bin/env bash

# every once in a while I want to have a fresh start, so I would reinstall
# everything from scratch
# this is the script for it
# use the script in pwd of where checkout-components are intended to be used

source ~/.dotfiles/helpers.sh

# clone checkout-components-ny and config git remote
bot "script is running, go get a cup of coffee now...."
if [[ ! -d "./checkout-components-ny" ]]; then
  running "cloning checkout-components-ny"
  git clone git@gecgithub01.walmart.com:R-Transaction/checkout-components-ny.git ./checkout-components-ny
  ok

  running "cloning checkout"
  git clone git@gecgithub01.walmart.com:R-Transaction/checkout.git ./checkout
  ok
fi

bot "configuring checkout-components-ny...."
function has_upstream() {
  git remote -v | grep upstream
}

pushd checkout-components-ny > /dev/null 2>&1

if [[ ! $has_upstream ]]; then
  running "setting up git remote for checkout-components..."
  git remote rename origin upstream
  git remote add origin git@gecgithub01.walmart.com:vn0yvfr/checkout-components-ny.git
  ok
fi

running "reinstalling node_modules..."
npx lerna clean --yes
rm -rf ./node_modules
npm i
ok

pushd packages/checkout-core 2>&1
running "installing babel-cli for core..."
npm i babel-cli
ok
popd > /dev/null 2>&1

pushd packages/checkout-fulfillment 2>&1
running "installing babel-cli for fulfillment..."
npm i babel-cli
ok
popd > /dev/null 2>&1

popd > /dev/null 2>&1

bot "configuring checkout...."
pushd checkout > /dev/null 2>&1

if [[ ! $has_upstream ]]; then
  running "setting up git remote for checkout..."
  git remote rename origin upstream
  git remote add origin git@gecgithub01.walmart.com:vn0yvfr/checkout.git
  ok
fi

running "reinstalling node_modules..."
rm -rf ./node_modules
npm i
ok

popd > /dev/null 2>&1

bot "all done!"
