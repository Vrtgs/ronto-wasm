(module
  (import "env" "input_int" (func $inp (result f64)))
  (import "env" "log"       (func $log (param  f64)))

  (type (func (param i32) (result i64)))
  (func $fib (type 0) (param $x i32) (result i64)
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

  (func $main (export "main")
      call $inp
      i32.trunc_f64_s
      call $fib
      f64.convert_i64_u
      call $log
  )

  (start $main)
)
