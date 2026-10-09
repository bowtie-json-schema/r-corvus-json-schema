# r-corvus-json-schema

A [Bowtie](https://github.com/bowtie-json-schema/bowtie) test harness for
[corvusjsonschema](https://corvus-dotnet.r-universe.dev/corvusjsonschema), the R package of
[Corvus.JsonSchema](https://github.com/corvus-dotnet/Corvus.JsonSchema), which is native code over the
corvus-json-schema Rust crate.

Its image is published to `ghcr.io/bowtie-json-schema/r-corvus-json-schema` and run via
`bowtie run -i r-corvus-json-schema`.

The harness compiles each case's schema with the case's `registry` as the document resolver and validates each
instance. For `annotations` output it evaluates at the verbose level and reports each annotation with its instance
location and `#…` keyword location. A request is never turned into R values, which cannot tell `1` from `[1]` or `{}`
from `[]`. The harness finds where each value lies in the request line and hands the schema, the registry's documents
and the instances to the package as the JSON text Bowtie wrote, so they reach the evaluator unchanged.

The image installs the package's latest release from [R-universe](https://corvus-dotnet.r-universe.dev), which serves
it built for Linux, so the build needs no Rust toolchain. The `IMPLEMENTATION_VERSION` build argument pins another
release, which is built from source (the `r-v<version>` tag of Corvus.JsonSchema). R-universe builds the package for
the current R release, so the base image's R version, which Dependabot keeps current, selects the build.
