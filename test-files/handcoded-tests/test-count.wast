(module
  (type (;0;) (func (param f64)))
  (type (;1;) (func))
  (import "env" "log" (func $log (;0;) (type 0)))
  (start 1)
  (func (;1;) (type 1)
    (local $i i32)
    loop ;; label = @1
      local.get $i
      i32.const 1
      i32.add
      local.set $i
      local.get $i
      f64.convert_i32_u
      call $log
      local.get $i
      i32.const 25
      i32.lt_s
      br_if 0 (;@1;)
    end
  )
)
