#!/usr/bin/env bash

# the goal is to automatically create one branch for master, two branches for
# soft-freeze branches, and two for hard-freeze branches, if specifies

echo What\'s the fix\'s scope?
read scopename
echo What\'s the fix name?
read branchname

echo Creating master branch on top of upstream master

git fetch upstream master
git branch $scopename-$branchname-master upstream/master

echo should I create branches for soft-freeze versions?
read yes_or_no

if [[ $yes_or_no =~ ^[Yy]$ ]]; then
  echo What\'s the soft-freeze version in the format of xx.x.x?
  read softversion
  git fetch upstream Release/$softversion/ios
  git branch $scopename-$branchname-$softversion-ios upstream/Release/$softversion/ios
  git fetch upstream Release/$softversion/android
  git branch $scopename-$branchname-$softversion-android upstream/Release/$softversion/android
fi

echo should I create branches for hard-freeze versions?
read yes_or_no

if [[ $yes_or_no =~ ^[Yy]$ ]]; then
  echo What\'s the hard-freeze version in the format of xx.x.x?
  read hardversion
  git fetch upstream Release/$hardversion/ios
  git branch $scopename-$branchname-$hardversion-ios upstream/Release/$hardversion/ios
  git fetch upstream Release/$hardversion/android
  git branch $scopename-$branchname-$hardversion-android upstream/Release/$hardversion/android
fi

