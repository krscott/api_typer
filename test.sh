#!bash

# exit when any command fails
set -e
trap 'echo ''; echo Error at $(basename "$0"):${LINENO}: $BASH_COMMAND' ERR

# set working directory to this script's directory
cd "${0%/*}"

cargo test

mkdir -p test/
cargo run --bin devtest -- -o test/spec.yml
cargo run -- -vv test/spec.yml -e test/ApiTypes.elm -r test/api_types.rs -t test/apiTypes.ts

# if this overflows, debug with
# rustup default stable-gnu
# cargo build
# rust-lldb -- target/debug/main.exe (args)
