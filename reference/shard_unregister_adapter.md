# Unregister an Adapter

Removes a previously registered adapter for a class. After
unregistration, objects of this class will use default traversal
behavior during deep sharing.

## Usage

``` r
shard_unregister_adapter(class)
```

## Arguments

- class:

  A character string naming the class to unregister.

## Value

Invisibly returns the removed adapter, or NULL if no adapter was
registered for this class.

## Examples

``` r
if (FALSE) { # \dontrun{
# Remove adapter for MyData class
old_adapter <- shard_unregister_adapter("MyData")
} # }
```
