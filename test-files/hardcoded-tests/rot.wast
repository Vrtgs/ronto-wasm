(module $rot.wasm
  (func $rotl (export "rotl") (param i32 i32) (result i32)
    local.get 0
    local.get 1
    i32.rotl
  )

  (func $rotr (export "rotr") (param i32 i32) (result i32)
    local.get 0
    local.get 1
    i32.rotr
  )

  (func $rotl64 (export "rotl64") (param i64 i64) (result i64)
    local.get 0
    local.get 1
    i64.rotl
  )

  (func $rotr64 (export "rotr64") (param i64 i64) (result i64)
    local.get 0
    local.get 1
    i64.rotr
  )
)
