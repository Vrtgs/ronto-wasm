(module $fib.wasm
  (func $implicit_select (export "implicit_select") (param i64 i64 i32) (result i64)
    local.get 0
    local.get 1
    local.get 2
    select
  )

  (func $explicit_select (export "explicit_select") (param f64 f64 i32) (result f64)
    ;; val1
    local.get 0
    local.get 1
    local.get 2
    select (result f64)
  )
)
