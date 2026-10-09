# A Bowtie (https://github.com/bowtie-json-schema/bowtie) harness for the corvusjsonschema R package.
#
# It speaks IHOP (one JSON request per line on standard input, one response per line on standard output).
# - `start` reports the implementation and its dialects.
# - `dialect` sets the dialect for schemas without `$schema`.
# - `run` compiles the case's schema with the case's `registry` as the document resolver and validates each instance
#   (for `annotations` output, at the verbose level, reporting each annotation with its instance location and `#...`
#   keyword location).
# - `stop` exits.
#
# A request is never parsed into R values. R has no value for much of what the test suite tells apart (`1` and `[1]`,
# `{}` and `[]`, a member that is null and one that is absent, an integer beyond 2^53, `1.0` and `1`, a string with a
# NUL in it), so the harness finds where each value lies in the request line and hands the schema, the registry's
# documents and the instances to the package as the JSON text Bowtie wrote, byte for byte. Only the strings the
# harness itself reads (member names, the command, the dialect, the registry's URIs) are decoded.
library(corvusjsonschema)

DIALECTS <- c(
  "https://json-schema.org/draft/2020-12/schema" = "draft2020-12",
  "https://json-schema.org/draft/2019-09/schema" = "draft2019-09",
  "http://json-schema.org/draft-07/schema#" = "draft7",
  "http://json-schema.org/draft-06/schema#" = "draft6",
  "http://json-schema.org/draft-04/schema#" = "draft4"
)

# ---------------------------------------------------------------------------------------------------------------------
# Reading. Where the values of a request line lie

# The bytes of a line of JSON, with the places of its structural characters (the brackets, braces, commas and colons
# outside strings), the character at each, and the nesting depth after each. Everything is found with whole-vector
# arithmetic, so a long line (a registry of every remote schema) costs a few passes and no loop.
scan_line <- function(line) {
  bytes <- charToRaw(line)
  code <- as.integer(bytes)
  index <- seq_along(code)
  # A quote ends or begins a string unless an odd number of backslashes runs up to it.
  run <- index - cummax(index * (code != 92L))
  quotes <- which(code == 34L)
  quotes <- quotes[c(0L, run)[quotes] %% 2L == 0L]
  if (length(quotes) %% 2L != 0L) {
    stop("The request is not JSON: a string does not end")
  }
  # Strings open at the odd quotes and close at the even ones.
  edge <- integer(length(code) + 1L)
  opening <- seq_along(quotes) %% 2L == 1L
  edge[quotes[opening]] <- 1L
  edge[quotes[!opening] + 1L] <- -1L
  outside <- cumsum(edge)[index] == 0L
  at <- which(outside & (code == 123L | code == 125L | code == 91L | code == 93L | code == 44L | code == 58L))
  char <- code[at]
  depth <- cumsum((char == 123L | char == 91L) - (char == 125L | char == 93L))
  list(bytes = bytes, code = code, at = at, char = char, depth = depth)
}

# The span (first and last byte) of a value, without the white space around it. An empty span has last < first.
trimmed <- function(doc, first, last) {
  code <- doc$code
  while (first <= last && (code[first] == 32L || code[first] == 9L || code[first] == 10L || code[first] == 13L)) {
    first <- first + 1L
  }
  while (last >= first && (code[last] == 32L || code[last] == 9L || code[last] == 10L || code[last] == 13L)) {
    last <- last - 1L
  }
  c(first, last)
}

# The JSON text of a span, exactly as it was written.
text_of <- function(doc, span) {
  text <- rawToChar(doc$bytes[span[1L]:span[2L]])
  Encoding(text) <- "UTF-8"
  text
}

# The separators (commas, and in an object colons) directly inside the array or object that opens at a byte, and
# where it closes.
inside <- function(doc, first, opener) {
  if (doc$code[first] != opener) {
    stop(sprintf("The request is not as expected: no %s at byte %d", intToUtf8(opener), first))
  }
  k <- match(first, doc$at)
  level <- doc$depth[k]
  rest <- seq.int(k + 1L, length(doc$at))
  close <- rest[match(level - 1L, doc$depth[rest])]
  within <- rest[rest < close]
  within <- within[doc$depth[within] == level]
  list(
    close = doc$at[close],
    commas = doc$at[within[doc$char[within] == 44L]],
    colons = doc$at[within[doc$char[within] == 58L]]
  )
}

# The members of the object whose span is given, as the decoded names and the span of each value.
members_of <- function(doc, span) {
  found <- inside(doc, span[1L], 123L)
  starts <- c(span[1L], found$commas) + 1L
  ends <- c(found$commas, found$close) - 1L
  if (length(found$colons) != length(starts)) {
    if (length(found$colons) == 0L && trimmed(doc, starts, ends)[2L] < starts) {
      return(list(names = character(), spans = list()))
    }
    stop("The request is not JSON: an object is malformed")
  }
  list(
    names = vapply(seq_along(starts), function(i) decoded(doc, trimmed(doc, starts[i], found$colons[i] - 1L)),
                   character(1L)),
    spans = lapply(seq_along(starts), function(i) trimmed(doc, found$colons[i] + 1L, ends[i]))
  )
}

# The span of the value of a member (of two with one name, the last), or NULL when there is none.
member <- function(members, name) {
  found <- which(members$names == name)
  if (length(found) > 0L) members$spans[[found[length(found)]]]
}

# The span of the value of a member that must be there.
required <- function(members, name) {
  span <- member(members, name)
  if (is.null(span)) {
    stop(sprintf("The request is not as expected: no \"%s\"", name))
  }
  span
}

# The spans of the elements of the array whose span is given.
elements_of <- function(doc, span) {
  found <- inside(doc, span[1L], 91L)
  starts <- c(span[1L], found$commas) + 1L
  ends <- c(found$commas, found$close) - 1L
  spans <- lapply(seq_along(starts), function(i) trimmed(doc, starts[i], ends[i]))
  if (length(spans) == 1L && spans[[1L]][2L] < spans[[1L]][1L]) list() else spans
}

# The value of a JSON string, given its span (quotes included). U+0000 and a surrogate without its pair, which an R
# string cannot hold, become U+FFFD. The harness decodes only names, commands and URIs, where neither belongs.
decoded <- function(doc, span) {
  code <- doc$code
  if (span[2L] <= span[1L] || code[span[1L]] != 34L || code[span[2L]] != 34L) {
    stop("The request is not as expected: a string was expected")
  }
  if (span[2L] == span[1L] + 1L) {
    return("")
  }
  inner <- doc$bytes[(span[1L] + 1L):(span[2L] - 1L)]
  if (!any(inner == as.raw(92L))) {
    text <- rawToChar(inner)
    Encoding(text) <- "UTF-8"
    return(text)
  }
  hex <- function(at) strtoi(rawToChar(inner[at:(at + 3L)]), 16L)
  pieces <- character()
  i <- 1L
  n <- length(inner)
  while (i <= n) {
    if (inner[i] != as.raw(92L)) {
      j <- i
      while (j < n && inner[j + 1L] != as.raw(92L)) {
        j <- j + 1L
      }
      piece <- rawToChar(inner[i:j])
      i <- j + 1L
    } else {
      escape <- rawToChar(inner[i + 1L])
      i <- i + 2L
      if (escape == "u") {
        point <- hex(i)
        i <- i + 4L
        if (point >= 0xD800L && point <= 0xDBFFL && i + 5L <= n && rawToChar(inner[i:(i + 1L)]) == "\\u" &&
              hex(i + 2L) >= 0xDC00L && hex(i + 2L) <= 0xDFFFL) {
          point <- 0x10000L + (point - 0xD800L) * 0x400L + (hex(i + 2L) - 0xDC00L)
          i <- i + 6L
        }
        if (is.na(point) || point == 0L || (point >= 0xD800L && point <= 0xDFFFL)) {
          point <- 0xFFFDL
        }
        piece <- intToUtf8(point)
      } else {
        piece <- switch(escape, b = "\b", f = "\f", n = "\n", r = "\r", t = "\t", escape)
      }
    }
    pieces[length(pieces) + 1L] <- piece
  }
  text <- paste(pieces, collapse = "")
  Encoding(text) <- "UTF-8"
  text
}

# ---------------------------------------------------------------------------------------------------------------------
# Writing. Responses as JSON text, in ASCII

# A string as a JSON string. Everything outside printable ASCII is written as a \u escape, so a response is the same
# bytes whatever the locale.
quoted <- function(text) {
  text <- enc2utf8(text)
  if (!validUTF8(text)) {
    text <- iconv(text, "UTF-8", "UTF-8", sub = "byte")
  }
  points <- utf8ToInt(text)
  plain <- points >= 32L & points < 127L & points != 34L & points != 92L
  if (all(plain)) {
    return(paste0('"', text, '"'))
  }
  out <- character(length(points))
  out[plain] <- intToUtf8(points[plain], multiple = TRUE)
  low <- !plain & points < 0x10000L
  out[low] <- sprintf("\\u%04x", points[low])
  high <- points >= 0x10000L
  out[high] <- sprintf("\\u%04x\\u%04x", 0xD800L + (points[high] - 0x10000L) %/% 0x400L,
                       0xDC00L + (points[high] - 0x10000L) %% 0x400L)
  paste0('"', paste(out, collapse = ""), '"')
}

# A number as JSON, which is the shortest text that reads back as the same double.
number <- function(x) {
  for (digits in 15:17) {
    text <- sprintf("%.*g", digits, x)
    if (as.numeric(text) == x) {
      break
    }
  }
  text
}

# An annotation's value as JSON. The package gives it as R values of its own making. They are NULL for null, a vector
# of length one for a scalar, a list without names for an array and a list with names for an object.
value_json <- function(value) {
  if (is.null(value)) {
    "null"
  } else if (is.list(value)) {
    items <- vapply(value, value_json, character(1L), USE.NAMES = FALSE)
    if (is.null(names(value))) {
      paste0("[", paste(items, collapse = ","), "]")
    } else {
      names <- vapply(names(value), quoted, character(1L), USE.NAMES = FALSE)
      paste0("{", paste(sprintf("%s:%s", names, items), collapse = ","), "}")
    }
  } else if (is.na(value)) {
    "null"
  } else if (is.logical(value)) {
    if (value) "true" else "false"
  } else if (is.integer(value)) {
    as.character(value)
  } else if (is.double(value)) {
    number(value)
  } else {
    quoted(value)
  }
}

# The members of an errored case or result. R keeps no traceback for a condition that was caught, so the condition's
# class stands in for one.
errored <- function(error) {
  sprintf('"errored":true,"context":{"message":%s,"traceback":%s}', quoted(conditionMessage(error)),
          quoted(class(error)[1L]))
}

# Percent-encodes text as a URI fragment does (upper-case hex, UTF-8).
percent_encoded <- function(text) {
  bytes <- charToRaw(enc2utf8(text))
  out <- rawToChar(bytes, multiple = TRUE)
  encode <- !(bytes %in% charToRaw("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~!$&'()*+,;=:@/?"))
  out[encode] <- sprintf("%%%02X", as.integer(bytes[encode]))
  paste(out, collapse = "")
}

# The annotations of a verbose evaluation (a row for each, with the instance location, the keyword as a JSON-pointer
# token, the schema location as a `#...` fragment and the value; the same in every Corvus implementation) as Bowtie
# lists them. Each has its keyword unescaped, and as its keyword location the schema location's fragment followed by
# `/` and the keyword token, percent-encoded as the fragment is. They are in the order of instance location, then
# keyword, then schema location, and where one keyword at one schema location annotated one instance location more
# than once, the last value is the one reported.
annotations_json <- function(annotations) {
  count <- nrow(annotations)
  if (count == 0L) {
    return("[]")
  }
  instance <- annotations$instance_location
  token <- annotations$keyword
  schema <- annotations$schema_location
  rows <- order(instance, token, schema, method = "radix")
  following <- rows[-1L]
  leading <- rows[-count]
  rows <- rows[c(instance[following] != instance[leading] | token[following] != token[leading] |
                   schema[following] != schema[leading], TRUE)]
  items <- vapply(rows, function(row) {
    sprintf('{"keyword":%s,"instanceLocation":%s,"keywordLocation":%s,"annotation":%s}',
            quoted(gsub("~0", "~", gsub("~1", "/", token[row], fixed = TRUE), fixed = TRUE)),
            quoted(instance[row]),
            quoted(paste0(schema[row], "/", percent_encoded(token[row]))),
            value_json(annotations$value[[row]]))
  }, character(1L))
  paste0("[", paste(items, collapse = ","), "]")
}

# ---------------------------------------------------------------------------------------------------------------------
# The commands

strip_fragment <- function(uri) {
  sub("#.*$", "", uri)
}

start <- function() {
  os_release <- "/proc/sys/kernel/osrelease"
  implementation <- c(
    language = "r",
    name = "corvus-json-schema",
    version = as.character(packageVersion("corvusjsonschema")),
    homepage = "https://github.com/corvus-dotnet/Corvus.JsonSchema",
    documentation = "https://corvus-dotnet.r-universe.dev/corvusjsonschema",
    issues = "https://github.com/corvus-dotnet/Corvus.JsonSchema/issues",
    source = "https://github.com/corvus-dotnet/Corvus.JsonSchema",
    os = tolower(Sys.info()[["sysname"]]),
    os_version = if (file.exists(os_release)) trimws(readLines(os_release, n = 1L, warn = FALSE)) else "",
    language_version = as.character(getRversion())
  )
  fields <- paste0(vapply(names(implementation), quoted, character(1L)), ":",
                   vapply(implementation, quoted, character(1L)), collapse = ",")
  sprintf('{"version":1,"implementation":{%s,"dialects":[%s]}}', fields,
          paste(vapply(names(DIALECTS), quoted, character(1L)), collapse = ","))
}

run <- function(doc, request, dialect) {
  seq <- text_of(doc, required(request, "seq"))
  case <- members_of(doc, required(request, "case"))
  output <- member(request, "output")
  annotations <- !is.null(output) && decoded(doc, output) == "annotations"

  # The registry's documents stay in the request line until the schema refers to one.
  registry <- member(case, "registry")
  registry <- if (is.null(registry)) list(names = character(), spans = list()) else members_of(doc, registry)
  registry$names <- strip_fragment(registry$names)
  resolver <- function(uri) {
    document <- member(registry, strip_fragment(uri))
    if (!is.null(document)) text_of(doc, document)
  }

  results <- tryCatch({
    validator <- compile_schema(text_of(doc, required(case, "schema")), default_dialect = dialect, resolver = resolver)
    vapply(elements_of(doc, required(case, "tests")), function(test) {
      tryCatch({
        instance <- text_of(doc, required(members_of(doc, test), "instance"))
        if (annotations) {
          evaluation <- evaluate_json(validator, instance, level = "verbose")
          sprintf('{"valid":%s,"annotations":%s}', if (evaluation$valid) "true" else "false",
                  annotations_json(evaluation$annotations))
        } else if (is_valid_json(validator, instance)) {
          '{"valid":true}'
        } else {
          '{"valid":false}'
        }
      }, error = function(e) sprintf("{%s}", errored(e))) # an error for one instance does not stop the others
    }, character(1L))
  }, error = function(e) e) # every compilation failure is reported for the case
  if (inherits(results, "error")) {
    sprintf('{"seq":%s,%s}', seq, errored(results))
  } else {
    sprintf('{"seq":%s,"results":[%s]}', seq, paste(results, collapse = ","))
  }
}

main <- function() {
  started <- FALSE
  dialect <- "draft2020-12"
  input <- file("stdin", open = "rb")
  on.exit(close(input))
  repeat {
    line <- readLines(input, n = 1L, warn = FALSE)
    if (length(line) == 0L) {
      break
    }
    if (!nzchar(trimws(line))) {
      next
    }
    doc <- scan_line(line)
    request <- members_of(doc, trimmed(doc, 1L, length(doc$bytes)))
    cmd <- decoded(doc, required(request, "cmd"))
    if (cmd != "start" && !started) {
      stop("Not started")
    }
    response <- switch(cmd,
      start = {
        version <- text_of(doc, required(request, "version"))
        if (!identical(suppressWarnings(as.numeric(version)), 1)) {
          stop(sprintf("Unsupported IHOP version %s", version))
        }
        started <- TRUE
        start()
      },
      dialect = {
        uri <- decoded(doc, required(request, "dialect"))
        if (uri %in% names(DIALECTS)) {
          dialect <- DIALECTS[[uri]]
          '{"ok":true}'
        } else {
          '{"ok":false}'
        }
      },
      run = run(doc, request, dialect),
      stop = break,
      stop(sprintf("Unknown command %s", quoted(cmd)))
    )
    cat(response, "\n", sep = "")
    flush(stdout())
  }
}

main()
