#!/usr/bin/env bash

set -x

function command_exists() {
    command -v "$1" &> /dev/null
}

command_exists node_exporter || brew install node_exporter
command_exists prometheus || brew install prometheus
command_exists grafana || brew install grafana
