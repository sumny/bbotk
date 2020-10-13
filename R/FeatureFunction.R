#' @title Feature
#'
#' @description
#'
#' @export
Feature = R6Class("Feature",
  public = list(

    #' @field id (`character(1)`).
    id = NULL,

    #' @field feature_function ([bbotk::Objective])
    feature_function = NULL,    

    # @field niche_boundaries
    niche_boundaries = NULL,

    #' @field surrogate ([Surrogate]).
    surrogate = NULL,  # FIXME: don't like this design but it is this way in AquisitionFunction in mlr3mbo so we stay with it for now

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param id (`character(1)`).
    #' @param feature_function ([bbotk::Objective]).
    #' @param surrogate ([Surrogate]).
    initialize = function(id, feature_function, niche_boundaries, surrogate) {
      self$id = assert_string(id)
      self$feature_function = assert_r6(feature_function, classes = "Objective")
      self$niche_boundaries = assert_r6(niche_boundaries, "NicheBoundaries")
      self$surrogate = assert_r6(surrogate, "Surrogate")
    },

    #' @description
    #' Evaluates all input values in `xdt`.
    #'
    #' @param xdt [data.table::data.table]
    #'
    #' @return `data.table` \cr
    #' The column has to have the same name as the id of the acq_fun, because we
    #' renamed the id of the codomain
    eval_dt = function(xdt) {
      self$feature_function$eval_dt(xdt)
    }
  )
)

NicheBoundary = R6Class("NicheBoundary",
  public = list(

    #' @field id (`character(1)`).
    id = NULL,

    initialize = function(id, niche_boundary) {
      self$id = assert_string(id)
      private$.niche_boundary = assert_niche_boundary(niche_boundary)
      private$.feature_function_ids = assert_character(names(niche_boundary), any.missing = FALSE, min.len = 1L, unique = TRUE)
    }
  ),

  private = list(
    .feature_function_ids = NULL,
    .niche_boundary = NULL
  ),

  active = list(
    feature_function_ids = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.feature_function_ids)) {
        stop("feature_function_ids is read-only.")
      }
      private$.feature_function_ids
    },

    niche_boundary = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.niche_boundary)) {
        stop("niche_boundary is read-only.")
      }
      private$.niche_boundary
    }
  )
)

NicheBoundaries = R6Class("NicheBoundaries",
  # FIXME: needs a simple container for the resulting boundaries
  # FIXME: should add a "rest" niche based on no matches
  public = list(

    #' @field id (`character(1)`).
    id = NULL,

    niches = NULL,  # FIXME: make private

    feature_function_ids = NULL,  # FIXME: make private

    initialize = function(id, niche_boundaries) {
      self$id = assert_string(id)
      private$.niche_boundaries = assert_niche_boundaries(niche_boundaries)
      self$niches = map_chr(niche_boundaries, "id")
      names(private$.niche_boundaries) = self$niches
      self$feature_function_ids = niche_boundaries[[1L]]$feature_function_ids
    },

    # FIXME: return niche as a factor with appropriate levels
    get_niche = function(gval) {  # FIXME: this should move to the Feature class?
      private$.get_niche(gval)
    },

    get_niche_dt = function(gvaldt) {  # FIXME: regarding get_niche etc. split up similar as in Objective
      data.table(niche = map_chr(transpose_list(gvaldt), private$.get_niche))
    }
  ),

  private = list(
    .niche_boundaries = NULL,

    .get_niche = function(gval) {
      assert_list(gval, types = "numeric", any.missing = FALSE, len = length(self$feature_function_ids))
      assert_names(names(gval), type = "strict", subset.of = self$feature_function_ids)
      gval = gval[self$feature_function_ids]  # FIXME: reorder necessary?
      # FIXME: the following is way to complex
      is_in_niche = map_lgl(self$niche_boundaries, function(niche) {
        all(pmap_lgl(list(niche$niche_boundary, gval), function(interval, point) interval[1L] <= point && point < interval[2L]))
      })
      self$niches[is_in_niche]
    }
  ),

  active = list(
    niche_boundaries = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.niche_boundaries)) {
        stop("niche_boundaries is read-only.")
      }
      private$.niche_boundaries
    }
  )
)

check_niche_boundary = function(x) {
  # FIXME: boundaries w.r.t a niche must be ok w.r.t. the domain of the feature functions
  # FIXME: boundaries must be sorted (lower, upper)
  structure_check = check_list(x, types = "numeric", any.missing = FALSE, min.len = 1L, names = "strict", null.ok = FALSE)
  if (!isTRUE(structure_check)) {
    return(structure_check)  # early exit
  }
  lengths = map_int(x, length)
  if (!all(lengths == 2L)) {
    return("All elements must be numeric intervals of length 2")  # early exit
  }
  return(TRUE)
}

assert_niche_boundary = function(x, .var.name = vname(x), add = NULL) {
  if (missing(x)) {
    stop(sprintf("argument \"%s\" is missing, with no default", .var.name))
  }
  res = check_niche_boundary(x)
  makeAssertion(x, res, .var.name, add)
}

check_niche_boundaries = function(x) {
  structure_check = check_list(x, types = "NicheBoundary", any.missing = FALSE, min.len = 2L, unique = TRUE, names = "unnamed", null.ok = FALSE)
  if (!isTRUE(structure_check)) {
    return(structure_check)
  }
  ids = map_chr(x, "id")
  ids_check = check_character(ids, unique = TRUE)
  if (!isTRUE(ids_check)) {
    return(ids_check)
  }
  names(x) = ids
  feature_function_ids = setDT(map(x, "feature_function_ids"))
  if (NROW(unique(t(feature_function_ids))) > 1L) {
    return("Niches must be defined on the same feature functions in the same order")
  }
  # FIXME: check that intervals defining niches are disjunct on the feature function(s)
  return(TRUE)
}

assert_niche_boundaries = function(x, .var.name = vname(x), add = NULL) {
  if (missing(x)) {
    stop(sprintf("argument \"%s\" is missing, with no default", .var.name))
  }
  res = check_niche_boundaries(x)
  makeAssertion(x, res, .var.name, add)
}
