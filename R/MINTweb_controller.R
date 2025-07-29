#' Calculate overall dn0 in a single call
#'
#' Supply the net‑type usage mix as named arguments and get the weighted
#' average **dn0** at the requested resistance level.
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
#'
#' @return A numeric scalar (overall dn0).
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

  valid_types <- available_net_types(itn_params_path)
  unknown      <- setdiff(names(usage_values), valid_types)

  if (length(unknown) > 0) {
    stop("Unknown net type(s): ",
         paste(unknown, collapse = ", "),
         call. = FALSE)
  }

  splines <- define_bednet_types(names(usage_values),
                                 itn_params_path = itn_params_path,
                                 strict = strict)

  resistance_to_overall_dn0(splines,
                            usage_values,
                            resistance_level)
}
