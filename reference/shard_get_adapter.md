# Get Adapter for an Object

Retrieves the registered adapter for an object's class. Checks all
classes in the object's class hierarchy, returning the first matching
adapter.

## Usage

``` r
shard_get_adapter(x)
```

## Arguments

- x:

  An R object.

## Value

The adapter list if one is registered for any of the object's classes,
or NULL if no adapter is registered.

## Examples

``` r
if (FALSE) { # \dontrun{
# Check if an adapter is registered for an object
adapter <- shard_get_adapter(my_object)
if (!is.null(adapter)) {
  children <- adapter$children(my_object)
}
} # }
```
