(module
  ;; import WASI fd_write: (fd, *iovs, iovcnt, *nwritten) -> errno
  (import "wasi_snapshot_preview1" "fd_write"
    (func $fd_write (param i32 i32 i32 i32) (result i32)))

  ;; 1 page = 64 KiB
  (memory 1)

  ;; Layout we'll use in memory:
  ;;   [0..3]   iov[0].buf_ptr  (i32)  -> 8
  ;;   [4..7]   iov[0].buf_len  (i32)  -> 12
  ;;   [8..19]  "hello world\n" (12 bytes)
  ;;   [20..23] nwritten (i32)  -> output from fd_write

  ;; Put the string at offset 8
  (data (i32.const 8) "hello world\n")

  ;; _start is the WASI entrypoint wasmtime runs by default
  (func (export "_start")
    ;; iov[0].buf_ptr = 8
    i32.const 0          ;; addr
    i32.const 8          ;; value
    i32.store

    ;; iov[0].buf_len = 12
    i32.const 4          ;; addr
    i32.const 12         ;; value
    i32.store

    ;; call fd_write(1 /*stdout*/, &iov, 1, &nwritten)
    i32.const 1          ;; fd = stdout
    i32.const 0          ;; &iov at 0
    i32.const 1          ;; iov count
    i32.const 20         ;; &nwritten
    call $fd_write
    drop                  ;; ignore errno for this toy
  )
)

