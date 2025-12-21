(module
  (type (;0;) (func (param i32 i32 i32 i32) (result i32)))
  (type (;1;) (func (param i32 i32)))
  (type (;2;) (func (param i32) (result i32 i32)))
  (type (;3;) (array (mut i32)))
  (type (;4;) (func (param (ref 3) (ref 3)) (result (ref 3))))
  (type (;5;) (func))
  (rec
    (type (;6;) (sub (func (param (ref 7)) (result i32))))
    (type (;7;) (sub (struct (field (ref 6)))))
  )
  (type (;8;) (struct (field (mut i32))))
  (type (;9;) (sub 7 (struct (field (ref 6)) (field (ref 8)))))
  (type (;10;) (func (result (ref 7))))
  (type (;11;) (func))
  (import "wasi_snapshot_preview1" "fd_write" (func (;0;) (type 0)))
  (memory (;0;) 1)
  (export "increment" (func 5))
  (export "make_counter" (func 6))
  (export "memory" (memory 0))
  (export "main" (func 4))
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
  (func (;3;) (type 4) (param (ref 3) (ref 3)) (result (ref 3))
    (local i32 i32 i32 (ref 3))
    local.get 0
    array.len
    local.set 3
    local.get 3
    array.new_default 3
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
        array.get 3
        local.get 1
        local.get 2
        array.get 3
        i32.add
        local.set 4
        local.get 5
        local.get 2
        local.get 4
        array.set 3
        local.get 2
        i32.const 1
        i32.add
        local.set 2
        br 0 (;@2;)
      end
    end
    local.get 5
  )
  (func (;4;) (type 5)
    (local (ref 7))
    call 6
    local.set 0
    local.get 0
    local.get 0
    struct.get 7 0
    call_ref 6
    call 2
    call 1
    i32.const 12
    i32.const 10
    i32.store8
    i32.const 12
    i32.const 1
    call 1
    local.get 0
    local.get 0
    struct.get 7 0
    call_ref 6
    call 2
    call 1
    i32.const 12
    i32.const 10
    i32.store8
    i32.const 12
    i32.const 1
    call 1
    local.get 0
    local.get 0
    struct.get 7 0
    call_ref 6
    call 2
    call 1
    i32.const 12
    i32.const 10
    i32.store8
    i32.const 12
    i32.const 1
    call 1
  )
  (func (;5;) (type 6) (param (ref 7)) (result i32)
    (local (ref 9))
    local.get 0
    ref.cast (ref 9)
    local.set 1
    local.get 1
    struct.get 9 1
    local.get 1
    struct.get 9 1
    struct.get 8 0
    i32.const 1
    i32.add
    struct.set 8 0
    local.get 1
    struct.get 9 1
    struct.get 8 0
    return
  )
  (func (;6;) (type 10) (result (ref 7))
    (local (ref 8))
    i32.const 0
    struct.new 8
    local.set 0
    ref.func 5
    local.get 0
    struct.new 9
    return
  )
  (func (;7;) (type 11)
    call 4
  )
)
