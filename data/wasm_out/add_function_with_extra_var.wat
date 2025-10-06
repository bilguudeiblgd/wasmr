(module
  (type (;0;) (func (param i32 i32) (result i32)))
  (export "add" (func 0))
  (func (;0;) (type 0) (param i32 i32) (result i32)
    (local i32)
    i32.const 1
    local.set 2
    local.get 0
    local.get 1
    i32.add
    local.get 2
    i32.add
    return
  )
)
