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
* Python
  * Derives from `pydantic.BaseModel` to provide serialization and verification

## Use
Typical use case: In `build.rs`, create (or deserialize from a config file) an `ApiType`
and write the outputs to your source directories.

## Should I use this?
Probably not anymore. I think the right way to do this is to generate JSON schemas from your types (e.g. [schemars](https://docs.rs/schemars/latest/schemars/)), then [transform](https://transform.tools/json-to-zod) it into your needed language or protocol.

## Example
See the `mod tests` sections of each `src/*_typer.rs` file for examples.

See `test.sh` for an example of generating files from a yml spec file.
