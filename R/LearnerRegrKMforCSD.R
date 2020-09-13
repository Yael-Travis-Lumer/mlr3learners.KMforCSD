#' @title Regression KMforCSD Learner
#'
#' @name mlr_learners_regr.MKforCSD
#'
#' @description
#' A [mlr3::LearnerRegr] implementing MKforCSD from package
#'   \pkg{KMforCSD}.
#' Calls [KMforCSD::KMforCSD()].
#'
#' @templateVar id regr.KMforCSD
#' @template section_dictionary_learner
#'
#'
#' @template seealso_learner
#' @template example
#' @export
# <Adapt the name to your learner. For regression learners inherit = LearnerRegr>
LearnerRegrKMforCSD = R6::R6Class("LearnerRegrKMforCSD",
  inherit = LearnerRegr,

  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(
        params = list(
          ParamDbl$new("cost", default = 1, tags = "train"),
          ParamFct$new("kernel", default = "rbfdot", levels = c("rbfdot", "polydot", "tanhdot", "vanilladot", "laplacedot", "besseldot", "anovadot", "splinedot"),tags = "train"),
          ParamDbl$new("gamma", default = 1, tags = "train"),
          ParamDbl$new("scale", default = 1, tags = "train"),
          ParamDbl$new("offset", default = 1, tags = "train"),
          ParamInt$new("degree", default = 1, tags = "train"),
          ParamInt$new("order", default = 1, tags = "train"),
          ParamDbl$new("alpha_cutoff", default =  1e-05, tags = "train"),
          ParamLgl$new("g_unknown", default=TRUE, tags="train"),
          ParamLgl$new("misspecification", default=FALSE, tags="train")
        )
      )

      ps$values = list(cost = 1, kernel="rbfdot",gamma=1,scale=1,offset=1,degree=1,order=1,alpha_cutoff=1e-05,g_unknown=TRUE,misspecification=FALSE)

      super$initialize(
        # see the mlr3book for a description: https://mlr3book.mlr-org.com/extending-mlr3.html
        id = "regr.KMforCSD",
        packages = "KMforCSD",
        feature_types = c("numeric", "integer"),
        predict_types = c("response","se"),
        param_set = ps
        #properties = "<properties>",
        # the help file name is the one used as @name in the roxygen2 block
        #man = "<pkgname>::<help file name>"
      )
    }


    # <Add method for importance, if learner supports that>
    # <See mlr3learners.randomForest for an example>

    # <Add method for oob_error, if learner supports that.>

  ),

  private = list(

    .train = function(task) {
      pars = self$param_set$get_values(tags = "train")
      data = task$data()
      # data = as.matrix(task$data(cols = task$feature_names))
      # p = ncol(data)
      # self$state$feature_names = colnames(data[,1:p])
#
#       # <Create objects for the train call
#       # <At least "data" and "formula" are required>
#       formula = task$formula()
#       data = task$data()

      # <here is space for some custom adjustments before proceeding to the
      # train call. Check other learners for what can be done here>

      # use the mlr3misc::invoke function (it's similar to do.call())
      mlr3misc::invoke(KMforCSD::KMforCSD, data = data,
       .args = pars)
    },

    .predict = function(task) {
      # get parameters with tag "predict"
      # pars = self$param_set$get_values(tags = "predict")
      # get newdata
      # newdata = task$data(cols = task$feature_names)
      newdata = task$data()
      # if (self$predict_type == "response") {
      #   pred = predict.KMforCSD(self$model, newdata = newdata)
      #   PredictionRegr$new(task = task, response = pred$prediction)
      # } else {
      #   pred = predict.KMforCSD(self$model, newdata = newdata)
      #   PredictionRegr$new(task = task, response = pred$prediction, se = pred$delta)
      # }
      pred = predict(self$model, newdata = newdata)
      PredictionRegr$new(task = task, response = pred$response, se = pred$delta)
      # pred = predict.KMforCSD(self$model, newdata = newdata)
      # PredictionRegr$new(task = task, response = pred)
      # # <optional: account for different predict types>
      # type = ifelse(self$predict_type == "response", "response", "prob")
      #
      # pred = mlr3misc::invoke(predict, self$model, newdata = newdata,
      #   type = type, .args = pars)
      #
      # # <Return a prediction object with PredictionClassif$new() or
      # # PredictionRegr$new()>
      # if (self$predict_type == "response") {
      #   PredictionClassif$new(task = task, response = pred)
      # } else {
      #   PredictionClassif$new(task = task, prob = pred)
      # }
    }
  )
)
