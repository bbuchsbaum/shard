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
shard_get_adapter(1:10)
#> NULL
```
