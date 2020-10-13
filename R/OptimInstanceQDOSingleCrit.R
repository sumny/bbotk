#' @title Optimization Instance for quality diversity optimization with budget and archive
#'
#' @description
#' Wraps a single-criteria [Objective] function with extra services for
#' convenient evaluation. Inherits from [OptimInstanceQDO].
#'
#' * Automatic storing of results in an [Archive] after evaluation.
#' * Automatic checking for termination. Evaluations of design points are
#'   performed in batches. Before a batch is evaluated, the [Terminator] is
#'   queried for the remaining budget. If the available budget is exhausted, an
#'   exception is raised, and no further evaluations can be performed from this
#'   point on.
#'
#' @template param_xdt
#' @template param_search_space
#' @export
OptimInstanceQDOSingleCrit = R6Class("OptimInstanceQDOSingleCrit",
  inherit = OptimInstanceQDO,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param objective ([Objective]).
    #' @param terminator ([Terminator]).
    initialize = function(objective, feature, search_space = NULL, terminator) {
      if (objective$codomain$length > 1) {
        stop("Codomain > 1")
      }
      super$initialize(objective, feature, search_space, terminator)
    },

    #' @description
    #' The [Optimizer] object writes the best found point
    #' and estimated performance value here. For internal use.
    #'
    #' @param y (`numeric(1)`)\cr
    #' Optimal outcome.
    assign_result = function(xdt, y, g) {
      # FIXME: We could have one way that just lets us put a 1xn DT as result directly.
      assert_data_table(xdt, nrows = 1L)
      assert_names(names(xdt), must.include = self$search_space$ids())
      assert_number(y)
      assert_numeric(g)
      assert_names(names(y), permutation.of = self$objective$codomain$ids())
      assert_names(names(g), permutation.of = self$feature$feature_function$codomain$ids())
      x_domain = transform_xdt_to_xss(xdt, self$search_space)[[1L]]
      private$.result = cbind(xdt, x_domain = list(x_domain), t(y), g) # t(y) so the name of y stays
    }
  )
)

