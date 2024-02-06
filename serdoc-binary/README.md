serdoc
======

Unified serialization with semi-automatic documentation

Introduction
------------
SerDoc provides:

- A unified interface for serialization formats ("codecs"), in the form of a
  'Serializable' typeclass.
- A mini-EDSL (`FieldInfo`) for describing serialization formats as first-class
  data structures, and a typeclass (`HasInfo`) to link them to codecs and
  serializable Haskell types.
- Building blocks and utility code for implementing `Codec`, `Serializable`,
  and `HasInfo` for existing or new serialization formats.

It also includes an implementation of these typeclasses for [the `binary`
package](https://hackage.haskell.org/package/binary).

Components
----------
SerDoc is split up into two sub-projects:

- `serdoc-core`, which provides the typeclasses and building blocks
- `serdoc-binary` (this library), which provides instances for `binary`
