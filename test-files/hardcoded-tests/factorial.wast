(module $factorial.wasm
  (func $fac (export "factorial") (param i32) (result i64)
    local.get 0
    i32.const 1
    i32.lt_u
    if (result i64)
      i64.const 1
      return
    else
      local.get 0
      i64.extend_i32_u
      local.get 0
      i32.const 1
      i32.sub
      call $fac
      i64.mul
    end
    )
)
