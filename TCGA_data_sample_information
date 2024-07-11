####设置工作目录
setwd("E:/生信教程/TCGA下载与整理") #设置工作路径到上图的路径下

# 载入必要的R包,用于数据处理
library(jsonlite)
library(dplyr) 

# 从JSON文件中读取数据
json_data <- fromJSON("metadata.cart.2024-06-07.json")

# 提取ID和case_id
ID <- sapply(json_data$associated_entities,
  function(x){x[,1]})
case_id <- sapply(json_data$associated_entities,
  function(x){x[,3]})

# 将提取的ID和case_id合并成一个数据框
sample_case <- data.frame(ID, case_id, stringsAsFactors = FALSE)

# 读取临床信息数据
clinical_data <- read.delim("clinical.cart.2024-06-07 (1)\\clinical.tsv", header = TRUE, sep = "\t")

# 检查case_id的唯一性，并选择重复的记录
clinical_data <- clinical_data[duplicated(clinical_data$case_id), ]

# 将sample_case和clinical_data按照case_id合并
clinical <- left_join(sample_case, clinical_data, by = "case_id")

# 去除不必要的列
clinical <- clinical[,-2]

# 将处理后的临床数据保存为CSV文件
write.csv(clinical, "clinical.csv", row.names = FALSE)
