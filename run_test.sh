#! /bin/sh

X=$1

stack build && stack runghc test/Homework/Week${X}Spec.hs
