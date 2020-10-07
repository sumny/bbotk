#' @title Optimization via Covariance Matrix Adaptation Evolution Strategy
#'
#' @include Optimizer.R
#' @name mlr_optimizers_cmaes
#'
#' @description
#' `OptimizerCmaes` class that implements CMA-ES. Calls
#' [adagio::pureCMAES()] from package \CRANpkg{adagio}.
#'
#' @templateVar id cmaes
#' @template section_dictionary_optimizers
#'
#' @section Parameters:
#' \describe{
#' \item{`par`}{`numeric()`}
#' \item{`sigma`}{`numeric(1)`}
#' }
#'
#' For the meaning of the control parameters, see [adagio::pureCMAES()]. Note
#' that we have removed all control parameters which refer to the termination of
#' the algorithm and where our terminators allow to obtain the same behavior.
#'
#' @export
#' @examples
#' library(paradox)
#' library(data.table)
#'
#' domain = ParamSet$new(list(ParamDbl$new("x", lower = -1, upper = 1)))
#'
#' search_space = ParamSet$new(list(ParamDbl$new("x", lower = -1, upper = 1)))
#'
#' codomain = ParamSet$new(list(ParamDbl$new("y", tags = "minimize")))
#'
#' objective_function = function(xs) {
#'   list(y = as.numeric(xs)^2)
#' }
#'
#' objective = ObjectiveRFun$new(fun = objective_function,
#'                               domain = domain,
#'                               codomain = codomain)
#' terminator = trm("evals", n_evals = 2)
#' instance = OptimInstanceSingleCrit$new(objective = objective,
#'                              search_space = search_space,
#'                              terminator = terminator)
#'
#' optimizer = opt("cmaes", par = 1)
#'
#' # Modifies the instance by reference
#' optimizer$optimize(instance)
#'
#' # Returns best scoring evaluation
#' instance$result
#'
#' # Allows access of data.table of full path of all evaluations
#' instance$archive$data()
OptimizerCmaes = R6Class("OptimizerCmaes",
  inherit = Optimizer,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(list(
        ParamUty$new("par", tags = "required"),
        ParamDbl$new("sigma", default = 0.5)
      ))
      super$initialize(
        param_set = ps,
        param_classes = "ParamDbl",
        properties = "single-crit",
        packages = "adagio"
      )
    }
  ),

  private = list(
    .optimize = function(inst) {
      pv = self$param_set$values
      pv$stopeval = .Machine$integer.max # make sure pureCMAES does not stop
      pv$stopfitness = -Inf

      invoke(adagio::pureCMAES, fun = inst$objective_function,
             lower = inst$search_space$lower, upper = inst$search_space$upper,
             .args = pv)
    }
  )
)

mlr_optimizers$add("cmaes", OptimizerCmaes)