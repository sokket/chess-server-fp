#!/bin/bash
stack build --copy-bins --local-bin-path ./
docker login registry.typex.one
docker build . -t registry.typex.one/chess-server:v2.8
docker push registry.typex.one/chess-server:v2.8
kubectl patch -n chess deployments.apps chess-game-chess-server -p \
  '{"spec":{"template":{"spec":{"containers":[{"name":"chess-server","image":"registry.typex.one/chess-server:v2.8"}]}}}}'
watch -n 1 kubectl get pods -n chess