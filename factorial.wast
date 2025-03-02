(module
  (import "env" "input_int" (func $inp (result f64)))
  (import "env" "log"       (func $log (param  f64)))

  (func $fac (export "fac") (param f64) (result f64)
    local.get 0
    f64.const 1
    f64.lt
    if (result f64)
      f64.const 1
    else
      local.get 0
      local.get 0
      f64.const 1
      f64.sub
      call $fac
      f64.mul
    end)

  (func $main (export "main")
    call $inp
    call $fac
    call $log)
    
  (start $main)
)
