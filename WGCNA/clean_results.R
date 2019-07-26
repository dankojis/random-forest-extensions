library(dplyr)
library(magrittr)


load("data20.Rds")


results1$data <- "Cor feature = 0.2"
# Select the best perfomring paramaters for each method
results1 %<>% filter(Type == "cor" & Beta == 6 & Threshold == 0.25|
                      Type == "L2" & Beta == 6 & Threshold == 0.25|
                      Type == "Regular" & Beta == 6 & Threshold == 0.25|
                      Type == "blockwiseModules" & Beta == 2 & Threshold == 0.25|
                      Type == "fastDTW" & Beta == 6 & Threshold == 0.25)








results2$data <- "Mixed Cor (.2,.4,.6,.9,.0)"
results2 %<>% filter(Type == "cor" & Beta == 6 & Threshold == 0.20|
                      Type == "L2" & Beta == 6 & Threshold == 0.25|
                      Type == "Regular" & Beta == 6 & Threshold == 0.30|
                      Type == "blockwiseModules" & Beta == 2 & Threshold == 0.25|
                      Type == "fastDTW" & Beta == 6 & Threshold == 0.25)






final <- rbind(results, results2)
final %<>% select(data, Type, everything())
