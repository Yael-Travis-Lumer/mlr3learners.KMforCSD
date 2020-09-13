# mlr3learners.KMforCSD
AN R package mlr3learners.KMforCSD, that wraps the KMforCSD algorithm as an MLR3 learner. The mlr3learners.KMforCSD package enables cross validation with respect to the loss presented in the paper Kernel Machines for Current Status Data.
## Example
1. After installing the package, initialize hyper-parameter tuner and optimization criteria:
```{r}
library(mlr3)
library(mlr3tuning)
library(mlr3learners.KMforCSD)
mlr_tuners$get("design_points")
tnr("design_points")
mlr3::mlr_measures$add("csd_risk", MeasureCSD)
```
2. Define train set and test set as MLR3 tasks:
```{r}
n_test = 10000
n_train = 100
test_list = weibull_data(n=n_test)
test_data = test_list$data
task_test  =  TaskRegr$new(id = "Test Data", backend = test_data, target = "C")
train_list = weibull_data(n=n_train)
train_data = train_list$data
task_train =  TaskRegr$new(id = "Train Data", backend = train_data, target = "C")
```
3. Initializing the KM-CSD as an MLR3 learner, with either an RBF kernel or a linear kernel:
```{r}
learner_rbf = lrn("regr.KMforCSD")
learner_rbf$param_set$values$kernel = "rbfdot"
learner_linear = lrn("regr.KMforCSD")
learner_linear$param_set$values$kernel = "vanilladot"
```
4. Use cross-validation:
```{r}
cv_cost_val=c(0.1,1,10,100) # cost CV values
cv_gamma_val=c(3,12,50,200) # RBF width CV values

#Tuning hyper-parameters
terminator_linear = trm("evals", n_evals = length(cv_cost_val))
terminator_rbf = trm("evals", n_evals = length(cv_cost_val)*length(cv_gamma_val))

#CV for rbf kernel
tune_ps = ParamSet$new(list(
  ParamDbl$new("cost", lower = min(cv_cost_val), upper = max(cv_cost_val)),
  ParamDbl$new("gamma", lower = min(cv_gamma_val), upper = max(cv_gamma_val))
))
instance_rbf = TuningInstanceSingleCrit$new(
  task = task_train,
  learner = learner_rbf,
  resampling = rsmp("cv", folds = 5),
  measure = msr("csd_risk"),
  search_space = tune_ps,
  terminator = terminator_rbf
)
design = data.table(cost=sort(rep(cv_cost_val,times=length(cv_gamma_val))),gamma=rep(cv_gamma_val,length(cv_cost_val)))
tuner = tnr("design_points", design = design)
result = tuner$optimize(instance_rbf)
#Add best lambda to learner
learner_rbf$param_set$values = instance_rbf$result_learner_param_vals
#train one last time with best lambda
trained_rbf = learner_rbf$train(task_train)
predictions_rbf = learner_rbf$predict(task_train)
Expectation_rbf=predictions_rbf$response
print("completed RBF training")
#testing
predictions_rbf_test = learner_rbf$predict(task_test)
Expectation_rbf_test=predictions_rbf_test$response
print("completed RBF testing")

#CV for linear kernel
tune_ps = ParamSet$new(list(
  ParamDbl$new("cost", lower = min(cv_cost_val), upper = max(cv_cost_val))
))
instance_linear = TuningInstanceSingleCrit$new(
  task = task_train,
  learner = learner_linear,
  resampling = rsmp("cv", folds = 5),
  measure = msr("csd_risk"),
  search_space = tune_ps,
  terminator = terminator_linear
)
design2 = data.table(cost=cv_cost_val)
tuner = tnr("design_points", design = design2)
result = tuner$optimize(instance_linear)
# #Add best lambda to learner
learner_linear$param_set$values = instance_linear$result_learner_param_vals
#train one last time with best lambda
trained_linear = learner_linear$train(task_train)
predictions_linear = learner_linear$predict(task_train)
Expectation_linear=predictions_linear$response
print("completed linear training")
#testing
predictions_linear_test = learner_linear$predict(task_test)
Expectation_linear_test=predictions_linear_test$response
print("completed linear testing")





#Tuning hyper-parameters
terminator_linear = term("evals", n_evals = length(cv_cost_val))
terminator_rbf = term("evals", n_evals = length(cv_cost_val)*length(cv_gamma_val))

#CV for rbf kernel
tune_ps = ParamSet$new(list(
  ParamDbl$new("cost", lower = min(cv_cost_val), upper = max(cv_cost_val)),
  ParamDbl$new("gamma", lower = min(cv_gamma_val), upper = max(cv_gamma_val))
))
instance_rbf = TuningInstance$new(
  task = task_train,
  learner = learner_rbf,
  resampling = rsmp("cv", folds = 5),
  measures = msr("csd_risk"),
  param_set = tune_ps,
  terminator = terminator_rbf
)
design = data.table(cost=sort(rep(cv_cost_val,times=length(cv_gamma_val))),gamma=rep(cv_gamma_val,length(cv_cost_val)))
tuner = tnr("design_points", design = design)
result = tuner$tune(instance_rbf)
#Add best lambda to learner
learner_rbf$param_set$values = instance_rbf$result$params
#train one last time with best lambda
trained_rbf = learner_rbf$train(task_train)
predictions_rbf = learner_rbf$predict(task_train)
Expectation_rbf=predictions_rbf$response
print("completed RBF training")
#testing
predictions_rbf_test = learner_rbf$predict(task_test)
Expectation_rbf_test=predictions_rbf_test$response
print("completed RBF testing")

#CV for linear kernel
tune_ps = ParamSet$new(list(
  ParamDbl$new("cost", lower = min(cv_cost_val), upper = max(cv_cost_val))
))
instance_linear = TuningInstance$new(
  task = task_train,
  learner = learner_linear,
  resampling = rsmp("cv", folds = 5),
  measures = msr("csd_risk"),
  param_set = tune_ps,
  terminator = terminator_linear
)
design2 = data.table(cost=cv_cost_val)
tuner = tnr("design_points", design = design2)
result = tuner$tune(instance_linear)
# #Add best lambda to learner
learner_linear$param_set$values = instance_linear$result$params
#train one last time with best lambda
trained_linear = learner_linear$train(task_train)
predictions_linear = learner_linear$predict(task_train)
Expectation_linear=predictions_linear$response
print("completed linear training")
#testing
predictions_linear_test = learner_linear$predict(task_test)
Expectation_linear_test=predictions_linear_test$response
print("completed linear testing")
 ```
