# Building the Compiler for WebAssembly (Browser)

This document explains how to compile the R-like language compiler to WebAssembly so it can run in a web browser, similar to Compiler Explorer.

## Prerequisites

1. Install Rust and cargo (if not already installed)
2. Install wasm-pack:
   ```bash
   cargo install wasm-pack
   ```

3. Add the wasm32 target:
   ```bash
   rustup target add wasm32-unknown-unknown
   ```

## Building for WASM

To build the compiler as a WebAssembly module:

```bash
wasm-pack build --target web
```

This will create a `pkg/` directory containing:
- `rty_compiler_bg.wasm` - The compiled WebAssembly binary
- `rty_compiler.js` - JavaScript bindings
- `rty_compiler.d.ts` - TypeScript definitions

## Using in a Web Application

### HTML Example

Create an `index.html` file:

```html
<!DOCTYPE html>
<html>
<head>
    <title>R-like Compiler</title>
    <style>
        body {
            font-family: monospace;
            max-width: 1200px;
            margin: 0 auto;
            padding: 20px;
        }
        textarea {
            width: 100%;
            height: 300px;
            font-family: monospace;
            margin-bottom: 10px;
        }
        button {
            padding: 10px 20px;
            font-size: 16px;
        }
        pre {
            background: #f5f5f5;
            padding: 15px;
            overflow-x: auto;
            border: 1px solid #ddd;
        }
        .error {
            color: red;
        }
    </style>
</head>
<body>
    <h1>R-like Language Compiler</h1>

    <h2>Source Code:</h2>
    <textarea id="source">x <- 42
y <- x * 2
print(y)</textarea>

    <button onclick="compile()">Compile to WAT</button>

    <h2>Output (WAT):</h2>
    <pre id="output"></pre>

    <script type="module">
        import init, { compile_string_to_wat } from './pkg/rty_compiler.js';

        async function run() {
            await init();

            window.compile = function() {
                const source = document.getElementById('source').value;
                const output = document.getElementById('output');

                try {
                    const wat = compile_string_to_wat(source);
                    output.textContent = wat;
                    output.className = '';
                } catch (error) {
                    output.textContent = 'Error: ' + error;
                    output.className = 'error';
                }
            };
        }

        run();
    </script>
</body>
</html>
```

### Serving the Application

You need to serve the files over HTTP (not file://). Use any simple HTTP server:

```bash
# Using Python 3
python3 -m http.server 8000

# Using Node.js http-server
npx http-server

# Using Rust's miniserve
cargo install miniserve
miniserve .
```

Then open `http://localhost:8000` in your browser.

## API Reference

### `compile_string_to_wat(source: string): string`

Compiles R source code to WAT (WebAssembly Text format).

**Parameters:**
- `source`: R source code as a string

**Returns:**
- WAT text format as a string on success
- Throws an error string on compilation failure (parse error, type error, etc.)

**Example:**
```javascript
try {
    const wat = compile_string_to_wat(`
        add <- function(a: int, b: int): int {
            return(a + b)
        }
        result <- add(10, 20)
        print(result)
    `);
    console.log(wat);
} catch (error) {
    console.error('Compilation failed:', error);
}
```

## Advanced: Compiler Explorer-like Interface

For a full compiler explorer experience, you can:

1. Add a split-pane view with source on the left, WAT on the right
2. Add syntax highlighting using libraries like Prism.js or CodeMirror
3. Add real-time compilation on text change (with debouncing)
4. Add download buttons for WAT/WASM files
5. Add example snippets dropdown

Example with CodeMirror:

```html
<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.65.2/codemirror.min.css">
<script src="https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.65.2/codemirror.min.js"></script>

<script>
const editor = CodeMirror.fromTextArea(document.getElementById('source'), {
    lineNumbers: true,
    mode: 'r',
    theme: 'default'
});

editor.on('change', debounce(() => {
    compile();
}, 500));
</script>
```

## Known Issues

1. **Runtime Dependencies**: The current runtime library has circular dependencies on the `max` function which uses name mangling for overload resolution. This may cause compilation errors for some programs. This is being addressed in future updates.

2. **File I/O**: The WASM version only supports string-based compilation (no file system access). All runtime functions must be embedded or provided as strings.

## Building for Production

For production builds with optimizations:

```bash
wasm-pack build --target web --release
```

This creates smaller, optimized WASM binaries suitable for production deployment.

## Troubleshooting

### "Cannot find module" errors
Make sure you're serving the files over HTTP and the import path is correct.

### WASM instantiation failed
Check browser console for CORS errors. Files must be served from the same origin.

### Memory issues
For large programs, you may need to increase WASM memory limits. This can be configured in the Rust code using `#[wasm_bindgen(start)]` and custom memory allocation.