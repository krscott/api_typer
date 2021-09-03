# api_typer
Generate compatible type definitions in multiple languages that can be serialized between each other.

Also generates some useful helper functions.

## Supported Languages
* Rust
  * Generates serde decorators
* TypeScript
  * Generates rust-like match functions for each Enum type
* Elm
  * Generates JSON encoder/decoder functions
  * *Note: some type combinations (e.g. non-String Map keys) are not implemented*

## Example
See the `mod tests` sections of each `src/*_typer.rs` file for examples.

