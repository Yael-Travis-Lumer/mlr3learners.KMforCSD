#' @title MeasureCSD
#' @name MeasureCSD
#'
#' @description
#' A [mlr3::measure] which is a modification of the quadratic loss for CSD
#' Calls [KMforCSD::KMforCSD()].
#'
#' @templateVar id "csd_risk"
#' @export
MeasureCSD = R6::R6Class(
  "MeasureCSD",
  inherit = mlr3::MeasureRegr,
  public = list(
    initialize = function() {
      super$initialize(
        # custom id for the measure
        id = "csd_risk",

        # additional packages required to calculate this measure
        packages = character(),

        # properties, see below
        properties = character(),

        # required predict type of the learner
        predict_type = "response",

        # feasible range of values
        range = c(0, Inf),

        # minimize during tuning?
        minimize = TRUE
      )
    }
  ),

  private = list(
    # custom scoring function operating on the prediction object
    .score = function(prediction, ...) {
      csd_risk = function(truth,response,se) {
        C <- truth
        delta <- se
        g_hat <- ks::kde(C,eval.points = C)$estimate
        csd_loss <- (1-delta)*2*(C-response)/g_hat+response^2 #all operations are element-wise
        return(mean(csd_loss))
      }

      csd_risk(prediction$truth, prediction$response,prediction$se)
    }
  )
)


