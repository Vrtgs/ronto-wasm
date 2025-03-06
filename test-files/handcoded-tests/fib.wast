(module $fib.wasm
  (type (func (param i32) (result i64)))
  (func $fib (export "fib") (type 0) (param $x i32) (result i64)
    local.get $x
    i32.const 1
    i32.le_u
    if ;; label = @1
      i64.const 1
      return
    end
    local.get $x
    i32.const 1
    i32.sub
    call $fib
    local.get $x
    i32.const 2
    i32.sub
    call $fib
    i64.add
    return
  )
)
