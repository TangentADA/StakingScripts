#!/bin/bash

#addr=$1
addr=wallets/user.address

echo "address : $addr"

cardano-cli address key-hash --payment-verification-key-file wallets/user.stake.vkey --out-file user.pkh


cabal exec make-user-datum $(cat user.pkh)
