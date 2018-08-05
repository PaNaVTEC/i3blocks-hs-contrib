#!/bin/bash

mkdir -p ./bin
ghc -o ./bin/Volume Volume.hs
ghc -o ./bin/DateTime DateTime.hs
ghc -o ./bin/Docker Docker.hs
ghc -o ./bin/Memory Memory.hs
ghc -o ./bin/OpenVpn OpenVpn.hs
ghc -o ./bin/Wifi Wifi.hs
