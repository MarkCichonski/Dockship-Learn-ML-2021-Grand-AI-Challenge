data <- read_csv("C:/Users/MarkCichonski/Learn ML 2021 Grand AI Challenge/new_train1.csv")

#shifting n rows up of a given variable
shift <- function(x, n) {
  c(x[-(seq(n))], rep(NA, n))
}
data$shifted <- shift(data$`Close-Stock-1`, 1)
tail(data)

#remove NA observations
data <- na.omit(data)
write.csv(data, "data.csv")

library(h2o)

#Initializing the Virtual Machine using all the threads (-1) and 16gb of memory
h2o.init(nthreads = -1, max_mem_size = "16g")

hf<-h2o.importFile("data.csv")
h2o.describe(hf)

y <- "shifted" #variable we want to forecast
x <- setdiff(names(data), y)

parts <- h2o.splitFrame(hf, .80)
train <- parts[[1]]
test <- parts[[2]]


automodel <- h2o.automl(x, y, train, test, max_runtime_secs = 120)

automodel@leader

predictions <- h2o.predict(automodel@leader, test)

fivepr<-as.data.frame(predictions)

datad <- read_csv("C:/Users/MarkCichonski/Learn ML 2021 Grand AI Challenge/new_testd.csv")
write.csv(datad, "datad.csv")

hfd<-h2o.importFile("datad.csv")
fpredictions<-h2o.predict(automodel@leader, hfd)

fiveprd<-as.data.frame(fpredictions)

write.csv(fiveprd,"C:\\Users\\MarkCichonski\\Learn ML 2021 Grand AI Challenge\\resultsrao1.csv", row.names = FALSE)
