library(mlr3verse)
library(tidyverse)

## 支持向量分类
df = read_csv("datas/breast-cancer.csv") %>% 
  select(-id) %>% 
  mutate(diagnosis = factor(diagnosis)) %>% 
  set_names(str_replace(names(.), " ", "_"))

# 创建任务
task = as_task_classif(df, target = "diagnosis")
task

autoplot(task)


# 划分训练集测试集
set.seed(123)
split = partition(task, ratio = 0.7)

# 选择学习器
learner = lrn("classif.svm", type = "C-classification")   # 需要e1071包

# 超参数调参
learner$param_set

search_space = ps(
  cost = p_dbl(lower = 0.1, upper = 10),
  gamma = p_dbl(lower = 0, upper = 5),
  kernel = p_fct(c("polynomial", "radial")))

at = auto_tuner(
  method = "grid_search",
  learner = learner,
  resampling = rsmp("cv", folds = 3),
  measure = msr("classif.acc"),
  search_space = search_space,
  term_evals = 20,
  resolution = 10)

future::plan("multicore")    # 启动并行化
set.seed(1)
at$train(task, row_ids = split$train)
at$tuning_result

learner$param_set$values = at$tuning_result$learner_param_vals[[1]]
learner$train(task, row_ids = split$train)

predictions = learner$predict(task, row_ids = split$test)                                            
predictions$score(msr("classif.acc"))


## 支持向量回归

