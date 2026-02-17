# Adapter Registry for Class-Specific Deep Sharing

Register custom traversal logic for specific classes during deep sharing
operations. Adapters allow fine-grained control over how objects are
decomposed and reconstructed.

## Details

The adapter registry provides a way to customize how specific classes
are handled during deep sharing. Instead of generic slot traversal for
S4 objects or element-wise traversal for lists, you can provide custom
functions to:

1.  Extract the shareable children from an object (`children`)

2.  Reconstruct the object from shared children (`replace`)

This is useful for:

- Complex S4 objects where only certain slots should be shared

- S3 objects with internal structure that differs from list structure

- Objects with accessors that should be used instead of direct slot
  access

## See also

[`share`](https://bbuchsbaum.github.io/shard/reference/share.md) for the
main sharing function that uses adapters.
