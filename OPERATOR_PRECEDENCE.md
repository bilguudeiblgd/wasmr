# Operator Precedence in Rty_compiler

## Implementation Location
The operator precedence is implemented via recursive descent parsing in `src/parser/expressions.rs`.

## Precedence Levels (Lowest to Highest)

### 1. Logical OR and AND (`|`, `&`)
**Parser**: `parse_bool()` at src/parser/expressions.rs:12-34
- Both operators at same level (left-to-right associativity)
- Lowest precedence in the system

```r
a | b & c    # Parses as: a | (b & c)
# Note: Both at same level, but AND is checked after OR in the loop
```

### 2. Comparison and Range Operators
**Parser**: `parse_comparison()` at src/parser/expressions.rs:36-50
- Operators: `<`, `<=`, `>`, `>=`, `==`, `!=`, `:` (sequence/range)
- All at the same precedence level

```r
1 + 2 < 3 * 4    # Parses as: (1 + 2) < (3 * 4)
1:10 + 5         # Parses as: 1:(10 + 5)
```

### 3. Addition and Subtraction (`+`, `-`)
**Parser**: `parse_term()` at src/parser/expressions.rs:77-99
- Left-to-right associativity

```r
10 - 5 - 2     # Parses as: (10 - 5) - 2 = 3
2 + 3 * 4      # Parses as: 2 + (3 * 4) = 14
```

### 4. Multiplication, Division, and Modulo (`*`, `/`, `%`)
**Parser**: `parse_factor()` at src/parser/expressions.rs:101-130
- Left-to-right associativity
- Higher than addition/subtraction

```r
10 - 8 / 2     # Parses as: 10 - (8 / 2) = 6
20 / 4 * 2     # Parses as: (20 / 4) * 2 = 10
```

### 5. Function Calls and Array Indexing (`()`, `[]`)
**Parser**: `parse_call()` at src/parser/expressions.rs:132-183
- Left-to-right associativity (postfix operators)
- Can be chained: `f(x)[1]` or `vec[1](2)`

```r
f(x + 1)       # Function call on result of addition
vec[i * 2]     # Index with arithmetic expression
f(x)(y)        # Higher-order function call
```

### 6. Unary Operators (`!`, `-`, `+`)
**Parser**: `parse_unary()` at src/parser/expressions.rs:185-215
- Right-to-left associativity (prefix operators)
- Can be chained: `!!x`, `--x`

```r
-2 * 3         # Parses as: (-2) * 3 = -6
!x & y         # Parses as: (!x) & y
```

### 7. Primary Expressions
**Parser**: `parse_primary()` at src/parser/expressions.rs:217-343
- Literals: numbers, strings, booleans
- Identifiers
- Grouping with parentheses `(expr)`
- Function definitions
- If expressions
- Highest precedence (atomic expressions)

```r
(2 + 3) * 4    # Parentheses override precedence
function(x: int) { x * 2 }  # Function literal
```

## Precedence Table Summary

| Precedence | Operators | Associativity | Description |
|------------|-----------|---------------|-------------|
| 1 (lowest) | `\|`, `&` | Left-to-right | Logical OR and AND |
| 2 | `<`, `<=`, `>`, `>=`, `==`, `!=`, `:` | Left-to-right | Comparison and range |
| 3 | `+`, `-` | Left-to-right | Addition and subtraction |
| 4 | `*`, `/`, `%` | Left-to-right | Multiplication, division, modulo |
| 5 | `()`, `[]` | Left-to-right | Function call and indexing |
| 6 | `!`, `-`, `+` | Right-to-left | Unary operators |
| 7 (highest) | literals, `(...)` | N/A | Primary expressions |

## Examples with Verified Results

```r
# Multiplication before addition
2 + 3 * 4       # = 14 (not 20)

# Division before subtraction
10 - 8 / 2      # = 6 (not 1)

# Arithmetic before comparison
5 + 3 > 2 * 4   # = FALSE (8 > 8)

# Unary before binary
-2 * 3          # = -6 (not -(2*3))

# Function calls bind tightly
f(x) + g(y)     # = (f(x)) + (g(y))

# Parentheses override precedence
(2 + 3) * 4     # = 20 (not 14)
```

## Comparison with R

The precedence closely follows standard R precedence, with a few notes:

1. **Range operator (`:`)** is at comparison level (same as R)
2. **Logical operators (`|`, `&`)** are at lowest level (R has `||` and `&&` at different levels, but Rty only has single versions)
3. **No exponentiation operator** (`^` or `**`) in Rty yet
4. **No pipe operators** (`|>`, `%>%`) - not implemented

## Implementation Notes

- The precedence is determined by the **calling order** in the recursive descent parser
- Lower precedence operators call higher precedence parsers
- Each level is responsible for consuming its operators in a loop
- Associativity is determined by whether the right-hand side calls the same level (left) or a higher level (right)
- Unary operators recurse to themselves for right-associativity

## Testing Precedence

To verify precedence is working correctly, see `data/test_operator_precedence.R`:

```bash
MY_FILE=data/test_operator_precedence.R cargo run --bin Rty_compiler
wasmtime -W gc=y -W function-references=y out/test_operator_precedence.wasm
```

Expected output:
```
14
6
FALSE
```