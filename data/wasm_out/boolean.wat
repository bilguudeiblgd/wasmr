(module
  (type (;0;) (func (param i32 i32 i32 i32) (result i32)))
  (type (;1;) (array (mut i32)))
  (type (;2;) (func (param (ref 1) (ref 1)) (result (ref 1))))
  (type (;3;) (func (param i32 i32) (result i32)))
  (type (;4;) (func (param i32 i32) (result i32)))
  (import "wasi_snapshot_preview1" "fd_write" (func (;0;) (type 0)))
  (memory (;0;) 1)
  (export "memory" (memory 0))
  (export "boolean_or" (func 2))
  (export "boolean_and" (func 3))
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
  (func (;2;) (type 3) (param i32 i32) (result i32)
    local.get 0
    local.get 1
    i32.or
    return
  )
  (func (;3;) (type 4) (param i32 i32) (result i32)
    local.get 0
    local.get 1
    i32.and
    return
  )
)
