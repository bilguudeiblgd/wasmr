(module
  (type (;0;) (func (param i32 i32 i32 i32) (result i32)))
  (type (;1;) (array (mut i32)))
  (type (;2;) (func (param (ref 1) (ref 1)) (result (ref 1))))
  (type (;3;) (func))
  (import "wasi_snapshot_preview1" "fd_write" (func (;0;) (type 0)))
  (memory (;0;) 1)
  (export "memory" (memory 0))
  (export "_start" (func 2))
  (func (;1;) (type 2) (param (ref 1) (ref 1)) (result (ref 1))
    (local i32 i32 i32 (ref 1))
    local.get 0
    array.len
    local.set 3
    local.get 3
    array.new_default 1
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
        array.get 1
        local.get 1
        local.get 2
        array.get 1
        i32.add
        local.set 4
        local.get 5
        local.get 2
        local.get 4
        array.set 1
        local.get 2
        i32.const 1
        i32.add
        local.set 2
        br 0 (;@2;)
      end
    end
    local.get 5
  )
  (func (;2;) (type 3)
    (local i32 i32 i32 i32)
    i32.const 12
    i32.const 10
    i32.store8
    i32.const 42
    local.set 0
    local.get 0
    i32.const 0
    i32.lt_s
    local.set 1
    block ;; label = @1
      local.get 1
      i32.eqz
      br_if 0 (;@1;)
      i32.const 0
      local.get 0
      i32.sub
      local.set 0
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
        local.get 0
        i32.const 10
        i32.rem_s
        i32.const 48
        i32.add
        i32.store8
        local.get 3
        i32.const 1
        i32.add
        local.set 3
        local.get 0
        i32.const 10
        i32.div_s
        local.tee 0
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
    i32.const 16
    local.get 2
    i32.store
    i32.const 20
    local.get 3
    i32.store
    i32.const 1
    i32.const 16
    i32.const 1
    i32.const 32
    call 0
    drop
    i32.const 16
    i32.const 12
    i32.store
    i32.const 20
    i32.const 1
    i32.store
    i32.const 1
    i32.const 16
    i32.const 1
    i32.const 32
    call 0
    drop
    i32.const 0
    drop
  )
)
