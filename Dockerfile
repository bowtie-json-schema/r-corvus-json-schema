FROM rocker/r-ver:4.6.1 AS base

# Installs the package into /opt/library. R-universe serves the latest release only, built for Linux (on the Ubuntu
# release named below, for the current and the coming R, for x86_64 and aarch64). Its build needs nothing newer than
# this image has, so the normal build installs it as it is and needs no Rust toolchain.
FROM base AS package
# Optionally pin the package's version so `build-all` can rebuild historical versions. An empty value (the normal
# build) takes the latest release. Any other version is built from source (tagged r-v<version> in Corvus.JsonSchema),
# with a Rust toolchain that stays in this stage.
ARG IMPLEMENTATION_VERSION=""
ARG R_UNIVERSE=https://corvus-dotnet.r-universe.dev
ARG R_UNIVERSE_DISTRIBUTION=resolute
ENV RUSTUP_HOME=/opt/rustup CARGO_HOME=/opt/cargo CARGO_BUILD_JOBS=4
RUN set -eu \
 && mkdir -p /opt/library \
 && binaries="$R_UNIVERSE/bin/linux/$R_UNIVERSE_DISTRIBUTION-$(uname -m)/$(Rscript -e 'cat(sub("^(\\d+\\.\\d+).*", "\\1", as.character(getRversion())))')" \
 && latest=$(Rscript -e 'cat(available.packages(repos = commandArgs(TRUE))["corvusjsonschema", "Version"])' "$binaries") \
 && version="${IMPLEMENTATION_VERSION:-$latest}" \
 && if [ "$version" = "$latest" ]; then \
      Rscript -e 'install.packages("corvusjsonschema", repos = commandArgs(TRUE), lib = "/opt/library")' "$binaries"; \
    else \
      apt-get update \
      && apt-get install -y --no-install-recommends build-essential ca-certificates curl \
      && curl -sSf https://sh.rustup.rs | sh -s -- -y --profile minimal \
      && mkdir /tmp/source \
      && curl -sSfL "https://codeload.github.com/corvus-dotnet/Corvus.JsonSchema/tar.gz/refs/tags/r-v$version" \
        | tar -xz -C /tmp/source --strip-components=2 --wildcards '*/src-r/corvusjsonschema' \
      && PATH="$CARGO_HOME/bin:$PATH" R CMD INSTALL --library=/opt/library /tmp/source/corvusjsonschema; \
    fi \
 && Rscript -e 'library(corvusjsonschema, lib.loc = "/opt/library"); stopifnot(packageVersion("corvusjsonschema") == commandArgs(TRUE), is_valid_json(compile_schema("true"), "1"))' "$version"

FROM base
COPY --from=package /opt/library/corvusjsonschema /usr/local/lib/R/site-library/corvusjsonschema
COPY bowtie_corvus_json_schema.R /usr/src/harness/
CMD ["Rscript", "--vanilla", "/usr/src/harness/bowtie_corvus_json_schema.R"]
