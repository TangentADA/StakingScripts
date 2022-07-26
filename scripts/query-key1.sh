#!/bin/bash

cardano-cli query utxo \
   $MAGIC \
   --address $(cat testnet/user.address)

