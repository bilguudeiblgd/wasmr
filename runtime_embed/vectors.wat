(module
    (type $int_vec (array (mut i32)))
    (func $vector_add (param $arr1 (ref $int_vec)) (param $arr2 (ref $int_vec)) (result (ref $int_vec))
        (local $i i32) (local $n i32) (local $tmp i32) (local $result_arr (ref $int_vec))
        ;; store array length
        local.get $arr1
        array.len
        local.set $n

        ;; allocate result array
        local.get $n
        array.new_default $int_vec
        local.set $result_arr

        ;; initialize i
        i32.const 0
        local.set $i

        block $exit
            loop $loop
                ;; if i >= n break
                local.get $i
                local.get $n
                i32.ge_u
                br_if $exit

                ;; tmp = arr1[i] + arr2[i]
                local.get $arr1
                local.get $i
                array.get $int_vec

                local.get $arr2
                local.get $i
                array.get $int_vec

                i32.add
                local.set $tmp

                ;; res[i] = tmp
                local.get $result_arr
                local.get $i
                local.get $tmp
                array.set $int_vec

                ;; i++
                local.get $i
                i32.const 1
                i32.add
                local.set $i

                br $loop
            end
        end
        local.get $result_arr
    )

    (func (export "test_add_sum") (result i32)
        i32.const 1
        ;; array = [2,3]
        i32.const 2
        i32.const 3
        array.new_fixed $type 2
        call

    )
)