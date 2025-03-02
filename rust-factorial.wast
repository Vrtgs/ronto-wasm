(module $lifetime-bc4e7575bc38d2d8.wasm
  (import "env" "input_int" (func $inp (result f64)))
  (import "env" "log"       (func $log (param  f64)))

  (type (;0;) (func (param i32) (result i64)))
  (memory (;0;) 16)
  (global $__stack_pointer (;0;) (mut i32) i32.const 1048576)
  (global (;1;) i32 i32.const 1048576)
  (global (;2;) i32 i32.const 1048576)
  (export "memory" (memory 0))
  (export "rust_factorial" (func $rust_factorial))
  (export "__data_end" (global 1))
  (export "__heap_base" (global 2))
  (func $rust_factorial (;0;) (type 0) (param i32) (result i64)
    (local i64 i64)
    local.get 0
    i64.extend_i32_u
    local.set 1
    i64.const 1
    local.set 2
    block ;; label = @1
      loop ;; label = @2
        local.get 1
        i64.const 2
        i64.lt_u
        br_if 1 (;@1;)
        local.get 2
        local.get 1
        i64.mul
        local.set 2
        local.get 1
        i64.const -1
        i64.add
        local.set 1
        br 0 (;@2;)
      end
    end
    local.get 2
  )

  (func $main (export "main")
    call $inp
    i32.trunc_f64_s
    call $rust_factorial
    f64.convert_i64_u
    call $log
)
  (@producers
    (processed-by "rustc" "1.85.0 (4d91de4e4 2025-02-17)")
  )
  (@custom "target_features" (after code) "\04+\0amultivalue+\0fmutable-globals+\0freference-types+\08sign-ext")
)
