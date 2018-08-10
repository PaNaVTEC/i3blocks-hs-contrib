#!/bin/bash

mkdir -p ./bin
ghc -o ./bin/Volume Volume.hs
ghc -o ./bin/DateTime DateTime.hs
ghc -o ./bin/Docker Docker.hs Common.hs
ghc -o ./bin/Memory Memory.hs Common.hs
ghc -o ./bin/OpenVpn OpenVpn.hs Common.hs
ghc -o ./bin/Wifi Wifi.hs
ghc -o ./bin/Bitcoin Bitcoin.hs
ghc -o ./bin/Battery Battery.hs
ghc -o ./bin/Cpu Cpu.hs Common.hs
ghc -o ./bin/AirplaneMode AirplaneMode.hs Common.hs
