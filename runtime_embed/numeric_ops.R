# Numeric utility functions
# These are basic numeric operations that don't fit in vector_ops
#
# The compiler automatically dispatches max(a, b) to the correct overload
# based on argument types using name mangling.
#
# Naming scheme: system_name___arg1__arg2__arg3
#   - THREE underscores (___) between function name and first arg
#   - TWO underscores (__) between arguments

# ============================================================================
# MAX operations for scalars
# ============================================================================

# max(int, int) -> int
system_max___int__int <- function(a: int, b: int): int {
    result <- a
    if(b > a) {
        result <- b
    }
    return(result)
}

# max(double, double) -> double
system_max___double__double <- function(a: double, b: double): double {
    result <- a
    if(b > a) {
        result <- b
    }
    return(result)
}
