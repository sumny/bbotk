#' @title Logging object for objective function evaluations QDO
#'
#' @description
#' Container around a [data.table::data.table] which stores all performed
#' function calls of the Objective and Feature.
#'
#' @section Technical details:
#'
#' The data is stored in a private `.data` field that contains a
#' [data.table::data.table] which logs all performed function calls of the
#' [Objective] and [Feature].
#' This [data.table::data.table] is accessed with the public `$data()` method. New
#' values can be added with the `$add_evals()` method. This however is usually
#' done through the evaluation of the [OptimInstance] by the [Optimizer].
#'
#' @template param_search_space
#' @template param_xdt
#' @template param_ydt
#' @export
ArchiveQDO = R6Class("ArchiveQDO",
  public = list(

    #' @field search_space ([paradox::ParamSet])\cr
    #' Search space of objective.
    search_space = NULL,

    #' @field codomain_obj ([paradox::ParamSet])\cr
    #' Codomain of objective function.
    codomain_obj = NULL,

    #' @field codomain_ft ([paradox::ParamSet])\cr
    #' Codomain of feature function.
    codomain_ft = NULL,

    #' @field niches (`character()`)\cr
    #' Labels of the niches of the feature function.
    niches = NULL,

    #' @field start_time ([POSIXct]).
    start_time = NULL,

    #' @field check_values (`logical(1)`)
    check_values = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param codomain_obj ([paradox::ParamSet])\cr
    #' Specifies codomain of objective function.
    #' Most importantly the tags of each output "Parameter" define whether it should
    #' be minimized or maximized.  The default is to minimize each component.
    #' @param codomain_ft ([paradox::ParamSet])\cr
    #' Specifies codomain of feature function.
    #' @param niches (`character()`)\cr
    #' Labels of the niches of the feature function.
    #' @param check_values (`logical(1)`)\cr
    #' Should x-values that are added to the archive be checked for validity?
    initialize = function(search_space, codomain_obj, codomain_ft, niches, check_values = TRUE) {
      self$search_space = assert_param_set(search_space)
      self$codomain_obj = assert_param_set(codomain_obj)
      self$codomain_ft = assert_param_set(codomain_ft)
      self$niches = niches  # FIXME: assert necessary?
      self$check_values = assert_flag(check_values)
      private$.data = data.table()
    },

    #' @description
    #' Adds function evaluations to the archive table.
    #'
    #' @param xss_trafoed (`list()`)\cr
    #' Transformed point(s) in the *domain space*.
    #' @param gdt ([data.table::data.table()])\cr
    #' Feature function values(s).
    #' @param niche ([data.table::data.table()])\cr
    #' Niche labels.
    #' Transformed point(s) in the *domain space*.
    add_evals = function(xdt, xss_trafoed, ydt, gdt, niche) {
      assert_data_table(xdt)
      assert_data_table(ydt)
      assert_data_table(gdt)
      assert_data_table(niche, types = "character")
      assert_data_table(ydt[, self$cols_y, with = FALSE], any.missing = FALSE)
      assert_data_table(gdt[, self$cols_g, with = FALSE], any.missing = FALSE)
      assert_list(xss_trafoed)
      if (self$check_values) {
        self$search_space$assert_dt(xdt[, self$cols_x, with = FALSE])
      }
      xygndt = cbind(xdt, ydt, gdt, niche)
      assert_subset(c(self$search_space$ids(), self$codomain_obj$ids(), self$codomain_ft$ids(), "niche"), colnames(xygndt))
      xygndt[, "x_domain" := list(xss_trafoed)]
      xygndt[, "timestamp" := Sys.time()]
      batch_nr = private$.data$batch_nr
      batch_nr = if (length(batch_nr)) max(batch_nr) + 1L else 1L
      xygndt[, "batch_nr" := batch_nr]
      private$.data = rbindlist(list(private$.data, xygndt), fill = TRUE, use.names = TRUE)
    },

    #' @description
    #' Returns the best scoring evaluation for each niche. For single-crit optimization,
    #' the solution that minimizes / maximizes the objective function.
    #' For multi-crit optimization, the Pareto set / front.
    #'
    #' @param m (`integer()`)\cr
    #' Take only batches `m` into account. Default is all batches.
    #' @param j (`character`)\cr
    #' Take only niches `j` into account. Default is all niches.
    #'
    #' @return [data.table::data.table()].
    best = function(m = NULL, j = NULL) {
      if (self$n_batch == 0L) {
        stop("No results stored in archive")
      }

      m = if (is.null(m)) {
        seq_len(self$n_batch)
      } else {
        assert_integerish(m, lower = 1L, upper = self$n_batch, coerce = TRUE)
      }
      batch_nr = NULL # CRAN check

      j = if (is.null(j)) {
        names(self$niches)
      } else {
        assert_string(j)
      }

      tab = private$.data[batch_nr %in% m & niche %in% j, ]

      max_to_min = mult_max_to_min(self$codomain_obj)
      if (self$codomain_obj$length == 1L) {
        setorderv(tab, cols = self$codomain_obj$ids(), order = max_to_min, na.last = TRUE)
        res = unique(tab, by = "niche")
      } else {
        # FIXME:
        ymat = t(as.matrix(tab[, self$cols_y, with = FALSE]))
        ymat = max_to_min * ymat
        res = tab[!is_dominated(ymat)]
      }

      return(res)
    },

    #' @description
    #' Returns a [data.table::data.table] which contains all performed
    #' [Objective] function calls.
    #'
    #' @param unnest (`character()`)\cr
    #' Set of column names for columns to unnest via [mlr3misc::unnest()].
    #' Unnested columns are stored in separate columns instead of list-columns.
    #'
    #' @return [data.table::data.table()].
    data = function(unnest = NULL) {
      if (is.null(unnest)) {
        return(copy(private$.data))
      }
      unnest(copy(private$.data), unnest, prefix = "{col}_")
    },

    #' @description
    #' Helper for print outputs.
    format = function() {
      sprintf("<%s>", class(self)[1L])
    },

    #' @description
    #' Printer.
    #'
    #' @param ... (ignored).
    print = function() {
      catf(format(self))
      print(private$.data)
    },

    #' @description
    #' Clear all evaluation results from archive.
    clear = function() {
      private$.data = data.table()
    }
  ),

  # FIXME: niches
  active = list(

    #' @field n_evals (`integer(1)`)\cr
    #' Number of evaluations stored in the archive.
    n_evals = function() nrow(private$.data),

    #' @field n_batch (`integer(1)`)\cr
    #' Number of batches stored in the archive.
    n_batch = function() {
      if (is.null(private$.data$batch_nr)) {
        0L
      } else {
        max(private$.data$batch_nr)
      }
    },

    #' @field cols_x (`character()`).
    #' Column names of search space parameters.
    cols_x = function() self$search_space$ids(),

    #' @field cols_y (`character()`).
    #' Column names of objective function codomain parameters.
    cols_y = function() self$codomain_obj$ids(),

    #' @field cols_g (`character()`).
    #' Column names of feature function codomain parameters.
    cols_g = function() self$codomain_ft$ids()
    # idx_unevaled = function() private$.data$y
  ),

  private = list(
    .data = NULL
  )
)
