(module
  (type (;0;) (func (param i32 i32) (result i32)))
  (type (;1;) (func (param f32 f32) (result f32)))
  (export "add" (func 0))
  (export "add_float" (func 1))
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
  (func (;1;) (type 1) (param f32 f32) (result f32)
    local.get 0
    local.get 1
    f32.add
    return
  )
)
