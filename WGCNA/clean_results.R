library(dplyr)
library(magrittr)


q <-  readRDS("WGCNA/data results/data30.Rds")
q <- readRDS("dataMix1.Rds")


results1$data <- "Cor feature = 0.2"
# Select the best perfomring paramaters for each method
results1 %<>% filter(Type == "cor" & Beta == 6 & Threshold == 0.25|
                      Type == "L2" & Beta == 6 & Threshold == 0.25|
                      Type == "Regular" & Beta == 6 & Threshold == 0.25|
                      Type == "blockwiseModules" & Beta == 2 & Threshold == 0.25|
                      Type == "fastDTW" & Beta == 6 & Threshold == 0.25)






# Cor feature = 0.8
results2 <-  readRDS("WGCNA/data results/data80.Rds")
results2$data <- "Cor featuer = 0.8"
results2 %<>% filter(Type == "cor" & Beta == 6 & Threshold == 0.25|
                      Type == "L2" & Beta == 8 & Threshold == 0.25|
                      Type == "Regular" & Beta == 8 & Threshold == 0.25|
                      Type == "blockwiseModules" & Beta == 6 & Threshold == 0.25|
                      Type == "fastDTW" & Beta == 8 & Threshold == 0.25)

# Mix 1
results3 <-  readRDS("WGCNA/data results/dataMix1.Rds")
results3$data <- "Mix Cor (0.2,0.4,0.6,0.8,0.0)"
results3 %<>% filter(Type == "cor" & Beta == 2 & Threshold == 0.25|
                       Type == "L2" & Beta == 6 & Threshold == 0.25|
                       Type == "Regular" & Beta == 2 & Threshold == 0.25|
                       Type == "blockwiseModules" & Beta == 2 & Threshold == 0.25|
                       Type == "fastDTW" & Beta == 6 & Threshold == 0.25)


# Mix 2
results4 <-  readRDS("WGCNA/data results/dataMix2.Rds")
results4$data <- "Mix Cor (0.4,0.5,0.6,0.9,0.0)"
results4 %<>% filter(Type == "cor" & Beta == 2 & Threshold == 0.25|
                       Type == "L2" & Beta == 6 & Threshold == 0.25|
                       Type == "Regular" & Beta == 2 & Threshold == 0.25|
                       Type == "blockwiseModules" & Beta == 2 & Threshold == 0.25|
                       Type == "fastDTW" & Beta == 6 & Threshold == 0.25)

# Mix 3
results5 <-  readRDS("WGCNA/data results/dataMix3.Rds")
results5$data <- "Mix Cor (0.2,0.4,0.8,0.8,0.0)"
results5 %<>% filter(Type == "cor" & Beta == 2 & Threshold == 0.25|
                       Type == "L2" & Beta == 6 & Threshold == 0.25|
                       Type == "Regular" & Beta == 8 & Threshold == 0.25|
                       Type == "blockwiseModules" & Beta == 2 & Threshold == 0.25|
                       Type == "fastDTW" & Beta == 8 & Threshold == 0.25)







final <- rbind(results, results2)
final %<>% select(data, Type, everything())
