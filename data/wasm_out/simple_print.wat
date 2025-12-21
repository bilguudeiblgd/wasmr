(module
  (type (;0;) (func (param i32 i32 i32 i32) (result i32)))
  (type (;1;) (func (param i32 i32)))
  (type (;2;) (func (param i32) (result i32 i32)))
  (type (;3;) (func (param f32) (result i32 i32)))
  (type (;4;) (func (param f64) (result i32 i32)))
  (type (;5;) (array (mut i32)))
  (type (;6;) (func (param (ref 5) (ref 5)) (result (ref 5))))
  (type (;7;) (func))
  (type (;8;) (func))
  (import "wasi_snapshot_preview1" "fd_write" (func (;0;) (type 0)))
  (memory (;0;) 1)
  (export "memory" (memory 0))
  (export "main" (func 6))
  (export "_start" (func 7))
  (func (;1;) (type 1) (param i32 i32)
    i32.const 1024
    local.get 0
    i32.store
    i32.const 1028
    local.get 1
    i32.store
    i32.const 1
    i32.const 1024
    i32.const 1
    i32.const 1032
    call 0
    drop
  )
  (func (;2;) (type 2) (param i32) (result i32 i32)
    (local i32 i32 i32 i32)
    local.get 0
    local.set 4
    local.get 0
    i32.const 0
    i32.lt_s
    local.set 1
    block ;; label = @1
      local.get 1
      i32.eqz
      br_if 0 (;@1;)
      i32.const 0
      local.get 4
      i32.sub
      local.set 4
    end
    i32.const 11
    local.set 2
    i32.const 0
    local.set 3
    block ;; label = @1
      loop ;; label = @2
        local.get 2
        i32.const 1
        i32.sub
        local.tee 2
        local.get 4
        i32.const 10
        i32.rem_s
        i32.const 48
        i32.add
        i32.store8
        local.get 3
        i32.const 1
        i32.add
        local.set 3
        local.get 4
        i32.const 10
        i32.div_s
        local.tee 4
        br_if 0 (;@2;)
      end
    end
    block ;; label = @1
      local.get 1
      i32.eqz
      br_if 0 (;@1;)
      local.get 2
      i32.const 1
      i32.sub
      local.tee 2
      i32.const 45
      i32.store8
      local.get 3
      i32.const 1
      i32.add
      local.set 3
    end
    local.get 2
    local.get 3
  )
  (func (;3;) (type 3) (param f32) (result i32 i32)
    (local i32 i32 i32 i32 i32 i32)
    local.get 0
    i32.trunc_f32_s
    local.set 1
    local.get 0
    local.get 1
    f32.convert_i32_s
    f32.sub
    f32.abs
    f32.const 0x1.9p+6 (;=100;)
    f32.mul
    i32.trunc_f32_s
    local.set 2
    local.get 1
    call 2
    local.set 4
    local.set 3
    i32.const 50
    local.get 3
    local.get 4
    memory.copy
    i32.const 50
    local.get 4
    i32.add
    local.set 5
    local.get 5
    i32.const 46
    i32.store8
    local.get 5
    i32.const 1
    i32.add
    local.set 5
    local.get 5
    local.get 2
    i32.const 10
    i32.div_u
    i32.const 48
    i32.add
    i32.store8
    local.get 5
    i32.const 1
    i32.add
    local.set 5
    local.get 5
    local.get 2
    i32.const 10
    i32.rem_u
    i32.const 48
    i32.add
    i32.store8
    local.get 4
    i32.const 3
    i32.add
    local.set 6
    i32.const 50
    local.get 6
  )
  (func (;4;) (type 4) (param f64) (result i32 i32)
    (local i32 i32 i32 i32)
    local.get 0
    i32.trunc_f64_s
    local.set 1
    local.get 0
    local.get 1
    f64.convert_i32_s
    f64.sub
    f64.const 0x1.9p+6 (;=100;)
    f64.mul
    i32.trunc_f64_s
    local.set 2
    local.get 1
    call 2
    local.set 4
    local.set 3
    i32.const 100
    local.get 3
    local.get 4
    memory.copy
    i32.const 100
    local.get 4
    i32.add
    local.tee 3
    i32.const 46
    i32.store8
    local.get 3
    i32.const 1
    i32.add
    local.set 3
    local.get 2
    i32.const 0
    i32.lt_s
    if ;; label = @1
      i32.const 0
      local.get 2
      i32.sub
      local.set 2
    end
    local.get 3
    local.get 2
    i32.const 10
    i32.div_s
    i32.const 48
    i32.add
    i32.store8
    local.get 3
    i32.const 1
    i32.add
    local.get 2
    i32.const 10
    i32.rem_s
    i32.const 48
    i32.add
    i32.store8
    i32.const 100
    local.get 4
    i32.const 3
    i32.add
  )
  (func (;5;) (type 6) (param (ref 5) (ref 5)) (result (ref 5))
    (local i32 i32 i32 (ref 5))
    local.get 0
    array.len
    local.set 3
    local.get 3
    array.new_default 5
    local.set 5
    i32.const 0
    local.set 2
    block ;; label = @1
      loop ;; label = @2
        local.get 2
        local.get 3
        i32.ge_u
        br_if 1 (;@1;)
        local.get 0
        local.get 2
        array.get 5
        local.get 1
        local.get 2
        array.get 5
        i32.add
        local.set 4
        local.get 5
        local.get 2
        local.get 4
        array.set 5
        local.get 2
        i32.const 1
        i32.add
        local.set 2
        br 0 (;@2;)
      end
    end
    local.get 5
  )
  (func (;6;) (type 7)
    i32.const 42
    call 2
    call 1
    i32.const 12
    i32.const 10
    i32.store8
    i32.const 12
    i32.const 1
    call 1
  )
  (func (;7;) (type 8)
    call 6
  )
)
