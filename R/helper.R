terminated_error = function(optim_instance) {
  msg = sprintf(
    fmt = "Objective (obj:%s, term:%s) terminated",
    optim_instance$objective$id,
    format(optim_instance$terminator)
  )

  set_class(list(message = msg, call = NULL),
    c("terminated_error", "error", "condition"))
}

#' @title Calculate which points are dominated
#' @description
#' Calculates which points are not dominated,
#' i.e. points that belong to the Pareto front.
#'
#' @param ymat (`matrix()`) \cr
#'   A numeric matrix. Each column (!) contains one point.
#' @useDynLib bbotk c_is_dominated
#' @export
is_dominated = function(ymat) {
  assert_matrix(ymat, mode = "double")
  .Call(c_is_dominated, ymat, PACKAGE = "bbotk")
}

#' @title Calculates the transformed x-values
#' @description
#' Transforms a given `data.table()` to a list with transformed x values.
#' If no trafo is defined it will just convert the `data.table()` to a list.
#' Mainly for internal usage.
#'
#' @template param_xdt
#' @template param_search_space
#' @return `list()`.
#' @keywords internal
#' @export
transform_xdt_to_xss = function(xdt, search_space) {
  design = Design$new(
    search_space,
    xdt[, search_space$ids(), with = FALSE],
    remove_dupl = FALSE
  )
  design$transpose(trafo = TRUE, filter_na = TRUE)
}

#' @title Default optimization function
#' @description
#' Used internally in the [Optimizer].
#' Brings together the private `.optimize()` method and the private `.assign_result()` method.
#'
#' @param inst [OptimInstance]
#' @param self [Optimizer]
#' @param private (`environment()`)
#'
#' @return [data.table::data.table]
#'
#' @keywords internal
#' @export
optimize_default = function(inst, self, private) {

  assert_instance_properties(self, inst)
  inst$archive$start_time = Sys.time()
  # start optimization
  lg$info("Starting to optimize %i parameter(s) with '%s' and '%s'",
    inst$search_space$length, self$format(), inst$terminator$format(with_params = TRUE))
  tryCatch({
    private$.optimize(inst)
  }, terminated_error = function(cond) {
  })
  private$.assign_result(inst)
  lg$info("Finished optimizing after %i evaluation(s)", inst$archive$n_evals)
  lg$info("Result:")
  lg$info(capture.output(print(
    inst$result, lass = FALSE, row.names = FALSE, print.keys = FALSE)))
  return(inst$result)
}

#' @title Default assign_result function
#' @description
#' Used internally in the [Optimizer].
#' It is the default way to determine the result by simply obtaining the best performing result from the archive.
#'
#' @param inst [OptimInstance]
#'
#' @keywords internal
#' @export
assign_result_default = function(inst) {
  res = inst$archive$best()

  xdt = res[, inst$search_space$ids(), with = FALSE]

  if (inherits(inst, "OptimInstanceMultiCrit")) {
    ydt = res[, inst$objective$codomain$ids(), with = FALSE]
    inst$assign_result(xdt, ydt)
  } else {
    # unlist keeps name!
    y = unlist(res[, inst$objective$codomain$ids(), with = FALSE])
    inst$assign_result(xdt, y)
  }

  invisible(NULL)
}

#' @title Multiplication vector for output
#' @description
#' Returns a numeric vector with values -1 and 1.
#' If you multiply this vector with an outcome of `codomain` it will be turned into a minimization problem.
#'
#' @param codomain [ParamSet]
#'
#' @return 'numeric()'
#'
#' @keywords internal
#' @export
mult_max_to_min = function(codomain) {
  # FIXME: we now have different tags
  ifelse(map_lgl(codomain$tags, has_element, "minimize"), 1, -1)
}


get_private = function(x) {
    x[[".__enclos_env__"]][["private"]]
}

#' @title Get start values for optimizers.
#' @description
#' Returns a named numeric vector with start
#' values for optimizers.
#'
#' @param search_space [ParamSet].
#' @param type (`character(1)`)\cr
#' `random` start values or `center` of search space?
#'
#' @return named 'numeric()'
#'
#' @keywords internal
search_start = function(search_space, type = "random") {
  assert_choice(type, c("random", "center"))
  if(type == "random") {
    unlist(generate_design_random(search_space, 1)$data[1,])
  } else if (type == "center") {
    if(!all(search_space$storage_type == "numeric")) {
      stop("Cannot generate center values of non-numeric parameters.")
    }
    (search_space$upper + search_space$lower) / 2
  }
}