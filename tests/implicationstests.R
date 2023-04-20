library(dplyr)
library(tidyverse)

comida <- read.csv("simple_comida.csv")

rownames(comida) <- comida[,1]
comida[1] <- NULL

comidaspfc <- PartialFormalContext$new(comida)

sink("outputfile.txt")
comidaspfc$find_implications()
comidaspfc$find_concepts()
sink()
comidaspfc$implications
comidaspfc$concepts