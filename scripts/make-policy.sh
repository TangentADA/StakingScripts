#!/bin/bash

#addr=$1
addr=wallets/user.address

echo "address : $addr"

cardano-cli address key-hash --payment-verification-key-file wallets/user.stake.vkey --out-file user.pkh


cabal exec make-policy stake.policy $(cat user.pkh)

cardano-cli address build --payment-script-file stake.policy --testnet-magic 1097911063  --out-file script.addr

echo "done"