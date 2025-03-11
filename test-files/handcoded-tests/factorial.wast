(module $factorial.wasm
  (func $fac (export "fac") (param f64) (result f64)
    local.get 0
    f64.const 1
    f64.lt
    if
      f64.const 1
      return
    end
    local.get 0
    local.get 0
    f64.const 1
    f64.sub
    call $fac
    f64.mul
    )
)
