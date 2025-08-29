#' Return the vector of net types contained in an ITN-parameter file
#' @param itn_params_path Path to an RDS created by MINTer
#' @export
available_net_types <- function(itn_params_path = NULL) {
  if (is.null(itn_params_path)) {
    itn_params_path <- system.file("extdata", "itn_dn0.rds", package = "MINTer")
  }
  stopifnot(file.exists(itn_params_path))
  unique(readRDS(itn_params_path)$net_type)
}

#' Define bednet types and create spline fits
#'
#' @param net_types Character vector of net types to process
#' @param itn_params_path Path to RDS file containing ITN parameters. If NULL, uses bundled data.
#' @param strict Allows for future expansion of dataset (defaul FALSE to use only available nets in RDS)
#' @return Named list of smooth.spline objects, one for each net type
#' @export
define_bednet_types <- function(net_types,
                                itn_params_path = NULL,
                                strict = FALSE) {

  if (is.null(itn_params_path)) {
    itn_params_path <- system.file("extdata", "itn_dn0.rds", package = "MINTer")
  }
  if (!file.exists(itn_params_path)) {
    stop("ITN-parameter file not found: ", itn_params_path)
  }

  itn_params <- readRDS(itn_params_path)
  have_types <- unique(itn_params$net_type)

  missing <- setdiff(net_types, have_types)

  if (length(missing) > 0) {
    msg <- paste0("No data for net type: ", paste(missing, collapse = ", "))
    if (strict)
      stop(msg, call. = FALSE)
    warning(msg, ". They will be ignored.", call. = FALSE)
    net_types <- setdiff(net_types, missing)
  }

  splines <- setNames(
    lapply(net_types, function(nt) {
      d <- itn_params[itn_params$net_type == nt, ]
      smooth.spline(d$resistance, d$dn0)
    }),
    net_types
  )
  splines
}

#' Convert resistance level to dn0 using spline fit
#'
#' @param spline_fit A smooth.spline object
#' @param resistance_level Numeric resistance level to predict dn0 for
#' @return Predicted dn0 value
#' @export
resistance_to_dn0 <- function(spline_fit, resistance_level) {
  predict(spline_fit, x = resistance_level)$y
}

#' Calculate weighted average dn0 across net types
#'
#' @param splines Named list of smooth.spline objects for each net type
#' @param usage_values Numeric vector of usage proportions for each net type
#' @param resistance_level Numeric resistance level to calculate dn0 for
#' @return Weighted average dn0 value
#' @export
resistance_to_overall_dn0 <- function(splines,
                                      usage_values,
                                      resistance_level) {

  net_types <- names(splines)

  if (!all(names(usage_values) %in% net_types)) {
    stop("usage_values contains net types that are absent from 'splines'")
  }
  if (length(usage_values) != length(net_types)) {
    stop("usage_values must supply one weight for each spline")
  }

  dn0_values <- vapply(net_types, function(nt) {
    predict(splines[[nt]], x = resistance_level)$y
  }, numeric(1))

  weighted.mean(dn0_values, usage_values[net_types])
}

#' Calculate overall dn0 in a single call
#'
#' Supply the net-type usage mix as named arguments and get
#'   * **dn0** - the weighted-average dn0 at the requested resistance level, and  
#'   * **itn_use** - the total usage proportion of pyrethroid-based ITNs.  
#'
#' @param resistance_level Numeric resistance level.
#' @param ...              Named numeric values giving the usage proportion for
#'                         each net type, e.g. `pyrethroid_only = 0.4`.
#' @param itn_params_path  Optional RDS file with ITN parameters (defaults to
#'                         the bundled example).
#' @param strict           Forwarded to `define_bednet_types()`.  If `TRUE`,
#'                         execution stops when any requested net type is
#'                         missing from the parameter file (even after the
#'                         initial name check).
#' @export
calculate_overall_dn0 <- function(resistance_level,
                                  ...,
                                  itn_params_path = NULL,
                                  strict = FALSE) {

  usage_values <- c(...)
  if (length(usage_values) == 0)
    stop("You must supply at least one `<net_type> = <value>` pair", call. = FALSE)

  if (is.null(names(usage_values)) || any(names(usage_values) == ""))
    stop("Every usage value has to be *named* with a valid net type", call. = FALSE)

  if (!is.numeric(usage_values))
    stop("Usage values must be numeric", call. = FALSE)

  # --- NEW: normalize short names to canonical RDS names ---------------------
  canonical_map <- c(
    "py_only"            = "pyrethroid_only",
    "pyrethroid_only"    = "pyrethroid_only",
    "py_pbo"             = "pyrethroid_pbo",
    "pyrethroid_pbo"     = "pyrethroid_pbo",
    "py_pyrrole"         = "pyrethroid_pyrrole",
    "pyrethroid_pyrrole" = "pyrethroid_pyrrole",
    "py_ppf"             = "pyrethroid_ppf",
    "pyrethroid_ppf"     = "pyrethroid_ppf"
  )

  orig_names <- names(usage_values)
  lower_orig <- tolower(orig_names)
  canonical  <- unname(canonical_map[lower_orig])

  if (anyNA(canonical)) {
    bad <- unique(orig_names[is.na(canonical)])
    stop("Unknown net type(s): ", paste(bad, collapse = ", "), call. = FALSE)
  }

  names(usage_values) <- canonical
  # --------------------------------------------------------------------------

  # Validate against what’s actually in the RDS
  valid_types <- tolower(available_net_types(itn_params_path))
  unknown <- setdiff(tolower(names(usage_values)), valid_types)
  if (length(unknown) > 0) {
    stop("Net types not present in ITN parameter file: ",
         paste(unknown, collapse = ", "),
         call. = FALSE)
  }

  splines <- define_bednet_types(names(usage_values),
                                 itn_params_path = itn_params_path,
                                 strict = strict)

  dn0_val <- resistance_to_overall_dn0(splines,
                                       usage_values,
                                       resistance_level)

  # Keep canonical logic: pyrethroid-based nets start with "pyrethroid"
  itn_use_val <- sum(
    usage_values[grepl("^pyrethroid", names(usage_values), ignore.case = TRUE)],
    na.rm = TRUE
  )

  list(
    dn0     = dn0_val,
    itn_use = itn_use_val
  )
}
