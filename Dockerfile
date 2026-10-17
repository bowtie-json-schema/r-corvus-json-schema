# The package, as released (src-r in Corvus.JsonSchema), built for R 4.6, the R of the image below. Its version is
# this tag, which Dependabot keeps up to date; the suffix is the R it is built for.
FROM ghcr.io/corvus-dotnet/corvus-json-schema-r:0.1.2-r4.6 AS corvus

FROM rocker/r-ver:4.6.1
# The binary package installs without a Rust toolchain.
RUN --mount=type=bind,from=corvus,source=/corvus-json-schema,target=/tmp/corvus \
    R CMD INSTALL /tmp/corvus/corvusjsonschema_*.tar.gz \
 && Rscript -e 'library(corvusjsonschema); stopifnot(is_valid_json(compile_schema("true"), "1"))'
COPY bowtie_corvus_json_schema.R /usr/src/harness/
CMD ["Rscript", "--vanilla", "/usr/src/harness/bowtie_corvus_json_schema.R"]
