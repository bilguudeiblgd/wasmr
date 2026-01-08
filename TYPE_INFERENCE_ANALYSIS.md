# Type Inference Limitations in Rty_compiler

Based on analysis of the type inference implementation in `/src/ir/`, here are programs where type inference **cannot** determine types, requiring explicit annotations.

## Summary of Type Inference Rules

1. **Number literals**: `42L` → `int`, `3.14` → `double`
2. **Variables**: Type from scope lookup
3. **Function definitions**: Return type defaults to `Type::Any` if not annotated
4. **Binary operations**: Numeric types unified (Int < Double)
5. **Built-in `c()`**: Element type inferred through unification, empty `c()` defaults to `vector<int>`
6. **Function calls**: Type from function's return type annotation

## Scenarios Where Type Inference Fails or Produces Ambiguity

### 1. Functions Without Return Type Annotations
**Problem**: Functions default to `Type::Any` return type, losing all type safety.

```r
# Type inference FAILS: Return type defaults to Any
identity <- function(x: int) {
    return(x)  # Type system doesn't verify this
}

result <- identity(42L)  # result has type Any, not int!
```

**Fix**: Explicit return type annotation required:
```r
identity <- function(x: int): int {
    return(x)
}
```

### 2. Empty `c()` Type Ambiguity
**Problem**: Empty `c()` defaults to `vector<int>`, but you might need `vector<double>`.

```r
# Defaults to vector<int>
empty_vec <- c()

# Later you cannot extend it with doubles - you must reassign
# empty_vec <- c(3.14)  # This creates a NEW vector<double>
```

**Fix**: Use `vec()` with explicit mode:
```r
empty_vec <- vec(length=0L, mode="double")  # vector<double>
```

### 3. Higher-Order Function Parameters
**Problem**: Cannot infer function parameter types without explicit annotation.

```r
# FAILS: Cannot infer type of 'f'
# apply_twice <- function(f, x: int) {
#     return(f(f(x)))  # What is f's signature?
# }
```

**Fix**: Explicit function type required:
```r
apply_twice <- function(f: int -> int, x: int): int {
    return(f(f(x)))
}
```

### 4. Polymorphic/Generic Functions
**Problem**: No support for type parameters - each type needs a separate function.

```r
# Cannot write: identity<T>(x: T): T

# Instead, must duplicate for each type:
identity_int <- function(x: int): int { return(x) }
identity_double <- function(x: double): double { return(x) }
```

**Limitation**: No generics/parametric polymorphism in the type system.

### 5. Recursive Functions
**Problem**: Recursive calls see `Type::Any` if return type is not annotated.

```r
# Type inference incomplete: factorial returns Any
factorial <- function(n: int) {
    if (n <= 1L) {
        return(1L)
    } else {
        # factorial's return type isn't determined yet - defaults to Any
        return(n * factorial(n - 1L))
    }
}
```

**Fix**: Explicit return type:
```r
factorial <- function(n: int): int {
    if (n <= 1L) {
        return(1L)
    } else {
        return(n * factorial(n - 1L))
    }
}
```

### 6. Conditional Branches with Incompatible Types
**Problem**: Type inference tries to unify branches, but complex types may fail.

```r
# What is the type of result?
result <- if (TRUE) {
    42L  # int
} else {
    3.14  # double
}
# Type inference unifies to double (numeric promotion)
# But this only works for numeric types!
```

**Limitation**: Type unification only works for numeric types (int/double/logical).
Cannot unify incompatible complex types (e.g., `vector<int>` vs `vector<double>`).

### 7. Vector of Functions with Different Signatures
**Problem**: Cannot create heterogeneous vectors of functions.

```r
func1 <- function(x: int): int { return(x * 2L) }
func2 <- function(x: int): double { return(x * 2.0) }

# FAILS: What is the element type?
# funcs <- c(func1, func2)  # (int -> int) vs (int -> double)
```

**Limitation**: No support for union types or existential types.

### 8. Function Composition
**Problem**: Cannot infer composed function types without explicit annotations.

```r
# Cannot infer types of f and g
# compose <- function(f, g, x: int) {
#     return(f(g(x)))
# }
```

**Fix**: All function types must be explicit:
```r
compose <- function(f: double -> int, g: int -> double, x: int): int {
    return(f(g(x)))
}
```

### 9. Nested Vector Types
**Problem**: Vector of vectors requires explicit annotation.

```r
inner <- c()  # Defaults to vector<int>
# outer <- c(inner)  # Is this vector<vector<int>> or vector<int>?
```

**Fix**: Explicit type annotation needed:
```r
# Type system doesn't support vector<vector<T>> easily
# Would need explicit construction
```

### 10. Mixed Type Arithmetic in Vectors
**Problem**: Vector element types must match after construction.

```r
vec_int <- c(1L, 2L, 3L)      # vector<int>
vec_double <- c(1.0, 2.0)     # vector<double>

# These work (creates vector<double>):
result1 <- vec_int + vec_double  # Unifies to vector<double>

# But indexing gives scalars:
mixed <- c(vec_int[1L], vec_double[1L])  # c(1L, 1.0) -> vector<double>
```

**Note**: `c()` performs type unification on its arguments.

## Root Causes

1. **No Hindley-Milner or bidirectional type inference**: Types flow in one direction (bottom-up), no constraint solving
2. **No generics/type parameters**: Cannot express polymorphic types like `identity<T>(x: T): T`
3. **No union types**: Cannot express `int | double` or heterogeneous collections
4. **Function types default to `Any`**: Conservative default loses type information
5. **No local type inference for parameters**: All parameter types must be annotated
6. **No type classes/traits**: Cannot express "any numeric type" abstractly

## Comparison with Other Type Systems

- **TypeScript**: Has generics, union types, structural typing - much more inference
- **Rust**: Has parametric polymorphism, trait bounds - precise inference through constraints
- **ML/Haskell**: Hindley-Milner inference - minimal annotations needed
- **Rty**: Simple unification for numerics only - explicit annotations required for most cases

## Recommendations

1. **Always annotate return types** for functions (especially recursive ones)
2. **Explicitly annotate function parameters** when used as arguments
3. **Use explicit casts** (`as.integer`, `as.double`) instead of relying on implicit conversion
4. **Avoid empty `c()`** - use `vec()` with explicit mode parameter
5. **Write type-specific functions** instead of trying to write generic ones
6. **Test with explicit types first** before relying on inference

## Future Improvements

To improve type inference, consider:

1. **Parametric polymorphism** (generics): `function<T>(x: T): T`
2. **Bidirectional type checking**: Types flow both up and down the AST
3. **Constraint-based inference**: Collect constraints, solve later
4. **Union types**: Express `int | double` explicitly
5. **Type classes**: Express "any numeric" constraint
6. **Return type inference** from function body analysis
