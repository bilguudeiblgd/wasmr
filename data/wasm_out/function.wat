(module
  (type (;0;) (func (param i32) (result i32)))
  (type (;1;) (func (param i32 i32 i32 i32) (result i32)))
  (type (;2;) (func (result i32)))
  (export "square" (func 0))
  (export "lincomb" (func 1))
  (export "main" (func 2))
  (func (;0;) (type 0) (param i32) (result i32)
    local.get 0
    local.get 0
    i32.mul
    return
  )
  (func (;1;) (type 1) (param i32 i32 i32 i32) (result i32)
    local.get 0
    local.get 2
    i32.mul
    local.get 1
    local.get 3
    i32.mul
    i32.add
    return
  )
  (func (;2;) (type 2) (result i32)
    (local i32 i32)
    i32.const 0
    i32.const 0
    i32.div_s
    drop
    i32.const 0
    drop
    i32.const 0
    drop
    i32.const 0
    drop
    i32.const 0
    drop
    i32.const 0
    drop
    i32.const 0
    drop
    i32.const 0
    drop
    i32.const 0
    drop
    i32.const 0
    drop
    i32.const 0
    drop
    i32.const 9
    call 0
    local.set 0
    i32.const 2
    i32.const 3
    i32.const 5
    i32.const 7
    call 1
    local.tee 1
  )
)
