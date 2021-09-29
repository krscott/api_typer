#!bash

# exit when any command fails
set -e
trap 'echo ''; echo Error at $(basename "$0"):${LINENO}: $BASH_COMMAND' ERR

# set working directory to this script's directory
cd "${0%/*}"

cargo test

mkdir -p test/
cargo run --example devtest -- -o test/spec.yml
cargo run --example cli -- -vv test/spec.yml -e test/ApiTypes.elm -r test/api_types.rs -t test/apiTypes.ts
