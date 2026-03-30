# Register an Adapter for Class-Specific Traversal

Registers a custom adapter for a specific class. When deep sharing
encounters an object of this class, it will use the adapter's
`children()` function to extract shareable components instead of generic
traversal.

## Usage

``` r
shard_register_adapter(class, adapter)
```

## Arguments

- class:

  A character string naming the class to register the adapter for.

- adapter:

  A list containing:

  class

  :   Character string matching the `class` parameter.

  children

  :   Function taking an object and returning a named list of child
      objects to traverse.

  replace

  :   Function taking the original object and a named list of
      (potentially shared) children, returning a reconstructed object.

  path_prefix

  :   Optional character string prefix for child paths in the sharing
      plan (default: class name).

## Value

Invisibly returns the previous adapter for this class (if any), or NULL
if no adapter was registered.

## Examples

``` r
shard_list_adapters()
#> character(0)
```
