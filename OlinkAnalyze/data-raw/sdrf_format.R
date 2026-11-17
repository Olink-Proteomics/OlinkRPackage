## code to prepare internal datasets from SDRF YAML templates
## based on https://r-pkgs.org/data.html#sec-data-sysdata
##
## Reads local templates under data-raw/sdrf/:
##   - base.yaml                 (v1.1.0; root)
##   - sample-metadata.yaml      (v1.0.0; extends base@>=1.1.0)
##   - affinity-proteomics.yaml  (v1.0.0; extends sample-metadata@>=1.0.0)
##
## Builds:
##   - sdrf_base_template / sdrf_sample_metadata_template / sdrf_affinity_template
##   - sdrf_column_dict: merged columns for affinity-proteomics
##     (base → sample-metadata → affinity; child overrides same-name columns)
##
## Requires the yaml package at data-raw build time.
## Wired into sysdata.R (rebuild R/sysdata.rda after YAML changes).

# helpers ----

#' Resolve path to an SDRF template YAML under data-raw/sdrf/
#'
#' @param file File name, e.g. "affinity-proteomics.yaml"
#' @return Absolute path to an existing file
#' @noRd
sdrf_yaml_path <- function(file) {
  candidates <- c(
    file.path("data-raw", "sdrf", file),
    file.path("sdrf", file),
    system.file("data-raw", "sdrf", file,
                package = "OlinkAnalyze",
                mustWork = FALSE)
  )
  hit <- candidates[file.exists(candidates)][1L]
  if (is.na(hit) || !nzchar(hit)) {
    stop(
      "Cannot find SDRF YAML '", file, "'. Tried:\n  - ",
      paste(candidates, collapse = "\n  - "),
      call. = FALSE
    )
  }
  normalizePath(hit, winslash = "/", mustWork = TRUE)
}

#' Parse an extends string such as "sample-metadata@>=1.0.0"
#'
#' @param extends Character scalar or NULL
#' @return NULL or list(name, op, version) where op is e.g. ">="
#' @noRd
sdrf_parse_extends <- function(extends) {
  if (is.null(extends) || !nzchar(extends)) {
    return(NULL)
  }
  extends <- as.character(extends)
  # name@opVERSION  e.g. sample-metadata@>=1.0.0
  m <- regexec(
    pattern = "^([A-Za-z0-9_-]+)@([<>!=]+)([0-9]+(?:\\.[0-9]+){0,3})$",
    text = extends
  )
  parts <- regmatches(extends, m)[[1L]]
  if (length(parts) != 4L) {
    stop("Cannot parse extends value: '", extends, "'.", call. = FALSE)
  }
  list(
    name = parts[[2L]],
    op = parts[[3L]],
    version = parts[[4L]]
  )
}

#' Check a template version against an extends constraint
#'
#' @param version Template version string, e.g. "1.0.0"
#' @param op Comparison operator, e.g. ">="
#' @param required Required version string, e.g. "1.0.0"
#' @return Logical
#' @noRd
sdrf_version_satisfies <- function(version, op, required) {
  v <- numeric_version(version)
  r <- numeric_version(required)
  switch(
    EXPR = op,
    ">=" = v >= r,
    ">"  = v > r,
    "<=" = v <= r,
    "<"  = v < r,
    "==" = v == r,
    "="  = v == r,
    stop("Unsupported version operator: '", op, "'.", call. = FALSE)
  )
}

#' Map official SDRF header to an internal snake_case key
#'
#' @param name Character scalar, official SDRF column header
#' @return Character scalar key
#' @noRd
sdrf_name_to_key <- function(name) {
  stopifnot(is.character(name), length(name) == 1L)
  key <- name
  if (grepl(pattern = "\\[.+\\]", x = name)) {
    key <- sub(
      pattern = "^[^\\[]+\\[(.+)\\]$",
      replacement = "\\1",
      x = name
    )
  }
  key <- tolower(key)
  key <- gsub(pattern = "[^a-z0-9]+", replacement = "_", x = key)
  key <- gsub(pattern = "^_|_$", replacement = "", x = key)
  key
}

#' Extract example values nested under column validators
#'
#' @param validators List of validator entries from YAML (or NULL)
#' @return Character vector of examples (possibly empty)
#' @noRd
sdrf_validator_examples <- function(validators) {
  if (is.null(validators) || length(validators) == 0L) {
    return(character())
  }
  ex <- lapply(
    X = validators,
    FUN = function(v) {
      params <- v[["params"]]
      if (is.null(params)) {
        return(character())
      }
      out <- params[["examples"]]
      if (is.null(out)) {
        return(character())
      }
      as.character(unlist(out, use.names = FALSE))
    }
  )
  unique(unlist(ex, use.names = FALSE))
}

#' Whether a column is multi-cardinality
#'
#' @param col Parsed column list entry
#' @return Logical scalar
#' @noRd
sdrf_col_is_multi <- function(col) {
  card <- col[["cardinality"]]
  if (!is.null(card) && identical(as.character(card), "multiple")) {
    return(TRUE)
  }
  validators <- col[["validators"]]
  if (is.null(validators) || length(validators) == 0L) {
    return(FALSE)
  }
  names_v <- vapply(
    X = validators,
    FUN = function(v) {
      nm <- v[["validator_name"]]
      if (is.null(nm)) NA_character_ else as.character(nm)
    },
    FUN.VALUE = character(1L)
  )
  # single_cardinality_validator means NOT multi
  if (any(names_v == "single_cardinality_validator", na.rm = TRUE)) {
    return(FALSE)
  }
  FALSE
}

#' Build column_name_dict-like tibble from one SDRF template YAML
#'
#' @param yaml Parsed YAML list from yaml::read_yaml()
#' @return A tibble
#' @noRd
sdrf_columns_from_yaml <- function(yaml) {
  cols <- yaml[["columns"]]
  if (is.null(cols) || length(cols) == 0L) {
    stop(
      "SDRF YAML '", yaml[["name"]], "' has no 'columns' entries.",
      call. = FALSE
    )
  }

  template_name <- yaml[["name"]]
  if (is.null(template_name)) {
    template_name <- NA_character_
  }

  col_name <- vapply(
    X = cols,
    FUN = function(cl) as.character(cl[["name"]]),
    FUN.VALUE = character(1L)
  )
  col_key <- vapply(X = col_name, FUN = sdrf_name_to_key, FUN.VALUE = character(1L))

  if (anyDuplicated(col_key)) {
    stop(
      "Duplicate col_key values in '", template_name, "': ",
      paste(unique(col_key[duplicated(col_key)]), collapse = ", "),
      call. = FALSE
    )
  }

  col_requirement <- vapply(
    X = cols,
    FUN = function(cl) {
      req <- cl[["requirement"]]
      if (is.null(req)) NA_character_ else as.character(req)
    },
    FUN.VALUE = character(1L)
  )

  col_description <- vapply(
    X = cols,
    FUN = function(cl) {
      desc <- cl[["description"]]
      if (is.null(desc)) NA_character_ else as.character(desc)
    },
    FUN.VALUE = character(1L)
  )

  ontology_accession <- vapply(
    X = cols,
    FUN = function(cl) {
      acc <- cl[["ontology_accession"]]
      if (is.null(acc)) NA_character_ else as.character(acc)
    },
    FUN.VALUE = character(1L)
  )

  col_default <- vapply(
    X = cols,
    FUN = function(cl) {
      d <- cl[["default"]]
      if (is.null(d)) NA_character_ else as.character(d)
    },
    FUN.VALUE = character(1L)
  )

  allow_not_applicable <- vapply(
    X = cols,
    FUN = function(cl) {
      x <- cl[["allow_not_applicable"]]
      if (is.null(x)) NA else as.logical(x)
    },
    FUN.VALUE = logical(1L)
  )

  allow_not_available <- vapply(
    X = cols,
    FUN = function(cl) {
      x <- cl[["allow_not_available"]]
      if (is.null(x)) NA else as.logical(x)
    },
    FUN.VALUE = logical(1L)
  )

  col_names <- lapply(
    X = seq_along(cols),
    FUN = function(i) {
      unique(c(col_name[[i]], col_key[[i]]))
    }
  )
  names(col_names) <- col_key

  col_examples <- lapply(
    X = cols,
    FUN = function(cl) sdrf_validator_examples(cl[["validators"]])
  )
  names(col_examples) <- col_key

  validators <- lapply(
    X = cols,
    FUN = function(cl) {
      v <- cl[["validators"]]
      if (is.null(v)) list() else v
    }
  )
  names(validators) <- col_key

  col_multi <- vapply(X = cols, FUN = sdrf_col_is_multi, FUN.VALUE = logical(1L))

  dplyr::tibble(
    col_key = col_key,
    col_names = col_names,
    col_requirement = col_requirement,
    col_source = rep(x = as.character(template_name), times = length(cols)),
    col_miss = col_requirement != "required",
    col_multi = col_multi,
    col_description = col_description,
    ontology_accession = ontology_accession,
    col_default = col_default,
    allow_not_applicable = allow_not_applicable,
    allow_not_available = allow_not_available,
    col_class = rep(x = "character", times = length(cols)),
    col_class_check = rep(x = FALSE, times = length(cols)),
    col_examples = col_examples,
    validators = validators
  )
}

#' Build template-level metadata list from parsed SDRF YAML
#'
#' @param yaml Parsed YAML list
#' @param path Path to the YAML file
#' @return Named list
#' @noRd
sdrf_template_from_yaml <- function(yaml, path) {
  list(
    name = yaml[["name"]],
    version = yaml[["version"]],
    description = yaml[["description"]],
    documentation = yaml[["documentation"]],
    extends = yaml[["extends"]],
    usable_alone = yaml[["usable_alone"]],
    layer = yaml[["layer"]],
    mutually_exclusive_with = {
      mex <- yaml[["mutually_exclusive_with"]]
      if (is.null(mex)) character() else as.character(unlist(mex, use.names = FALSE))
    },
    validators = yaml[["validators"]],
    contributors = {
      ctb <- yaml[["contributors"]]
      if (is.null(ctb)) character() else as.character(unlist(ctb, use.names = FALSE))
    },
    url = paste0(
      "https://sdrf.quantms.org/templates/",
      yaml[["name"]],
      ".html"
    ),
    yaml_path = path
  )
}

#' Load one local SDRF YAML by template name
#'
#' @param name Template name without extension, e.g. "sample-metadata"
#' @return list(yaml, path, template, columns)
#' @noRd
sdrf_load_template <- function(name) {
  path <- sdrf_yaml_path(paste0(name, ".yaml"))
  yaml <- yaml::read_yaml(path)
  if (is.null(yaml[["name"]]) || !identical(as.character(yaml[["name"]]), name)) {
    stop(
      "YAML file '", path, "' has name '", yaml[["name"]],
      "' but was loaded as '", name, "'.",
      call. = FALSE
    )
  }
  if (is.null(yaml[["version"]])) {
    stop("YAML '", name, "' is missing a 'version' field.", call. = FALSE)
  }
  list(
    name = name,
    path = path,
    yaml = yaml,
    template = sdrf_template_from_yaml(yaml = yaml, path = path),
    columns = sdrf_columns_from_yaml(yaml)
  )
}

#' Merge parent then child column tibbles; child wins on duplicate col_key
#'
#' @param parent_cols Tibble from parent template (or NULL)
#' @param child_cols Tibble from child template
#' @return Tibble
#' @noRd
sdrf_merge_columns <- function(parent_cols, child_cols) {
  if (is.null(parent_cols) || nrow(parent_cols) == 0L) {
    return(child_cols)
  }
  overlap <- intersect(parent_cols$col_key, child_cols$col_key)
  if (length(overlap) > 0L) {
    parent_cols <- parent_cols[!parent_cols$col_key %in% overlap, , drop = FALSE]
  }
  dplyr::bind_rows(parent_cols, child_cols)
}

#' Resolve extends for a loaded template against locally available templates
#'
#' Only follows parents that exist under data-raw/sdrf/. Missing parents
#' (e.g. base) are recorded as unresolved warnings.
#'
#' @param child Loaded template list from sdrf_load_template()
#' @param available Named list of loaded templates
#' @return list(columns, chain, unresolved)
#' @noRd
sdrf_resolve_extends <- function(child, available) {
  chain <- child$name
  unresolved <- character()
  columns <- child$columns

  extends_raw <- child$yaml[["extends"]]
  parent_spec <- sdrf_parse_extends(extends_raw)

  while (!is.null(parent_spec)) {
    parent_name <- parent_spec$name
    if (!parent_name %in% names(available)) {
      unresolved <- c(
        unresolved,
        paste0(
          parent_name, "@", parent_spec$op, parent_spec$version,
          " (required by ", chain[[length(chain)]], "; YAML not present locally)"
        )
      )
      break
    }

    parent <- available[[parent_name]]
    if (!sdrf_version_satisfies(
      version = parent$yaml[["version"]],
      op = parent_spec$op,
      required = parent_spec$version
    )) {
      stop(
        "Template '", child$name, "' requires ",
        parent_name, "@", parent_spec$op, parent_spec$version,
        " but local '", parent_name, "' is version ",
        parent$yaml[["version"]], ".",
        call. = FALSE
      )
    }

    chain <- c(parent_name, chain)
    columns <- sdrf_merge_columns(parent$columns, columns)

    parent_spec <- sdrf_parse_extends(parent$yaml[["extends"]])
  }

  list(
    columns = columns,
    chain = chain,
    unresolved = unresolved
  )
}

# read local SDRF YAMLs ----

if (!requireNamespace("yaml", quietly = TRUE)) {
  stop(
    "Package 'yaml' is required to build SDRF format objects from ",
    "data-raw/sdrf/*.yaml. Install it with install.packages(\"yaml\").",
    call. = FALSE
  )
}

## Load all templates present under data-raw/sdrf/
sdrf_base <- sdrf_load_template("base")
sdrf_sample_metadata <- sdrf_load_template("sample-metadata")
sdrf_affinity <- sdrf_load_template("affinity-proteomics")

sdrf_available <- list(
  "base" = sdrf_base,
  "sample-metadata" = sdrf_sample_metadata,
  "affinity-proteomics" = sdrf_affinity
)

## Template metadata (cite local versions)
sdrf_base_template <- sdrf_base$template
sdrf_sample_metadata_template <- sdrf_sample_metadata$template
sdrf_affinity_template <- sdrf_affinity$template
sdrf_affinity_template$technology_type_olink <-
  "protein expression profiling by antibody array"
sdrf_affinity_template$quantification_unit_olink <- "NPX"

## Resolve affinity → sample-metadata@>=1.0.0 → base@>=1.1.0
sdrf_affinity_resolved <- sdrf_resolve_extends(
  child = sdrf_affinity,
  available = sdrf_available
)

sdrf_affinity_template$resolved_chain <- sdrf_affinity_resolved$chain
sdrf_affinity_template$unresolved_extends <- sdrf_affinity_resolved$unresolved
sdrf_affinity_template$local_dependencies <- lapply(
  X = sdrf_available,
  FUN = function(tpl) {
    list(
      version = tpl$template$version,
      yaml_path = tpl$path,
      extends = tpl$template$extends
    )
  }
)

if (length(sdrf_affinity_resolved$unresolved) > 0L) {
  warning(
    "SDRF inheritance incomplete; missing local YAML(s):\n  - ",
    paste(sdrf_affinity_resolved$unresolved, collapse = "\n  - "),
    call. = FALSE
  )
}

## Merged column dict for the usable affinity-proteomics template
sdrf_column_dict <- sdrf_affinity_resolved$columns

## Layer-only dicts (useful for debugging / future)
sdrf_base_column_dict <- sdrf_base$columns
sdrf_sample_metadata_column_dict <- sdrf_sample_metadata$columns

rm(
  sdrf_yaml_path,
  sdrf_parse_extends,
  sdrf_version_satisfies,
  sdrf_name_to_key,
  sdrf_validator_examples,
  sdrf_col_is_multi,
  sdrf_columns_from_yaml,
  sdrf_template_from_yaml,
  sdrf_load_template,
  sdrf_merge_columns,
  sdrf_resolve_extends,
  sdrf_base,
  sdrf_sample_metadata,
  sdrf_affinity,
  sdrf_available,
  sdrf_affinity_resolved
)
