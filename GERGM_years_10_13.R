outloc = "/storage/home/clm453/soda502/" 
dataloc = "/storage/home/clm453/soda502/"

# Call the RSiena library
# This location may need to be revised depending on where you installed and saved the library
#options(repos = c(CRAN = "http://cran.rstudio.com"))
library('GERGM')
set.seed(456)
#install.packages('RMySQL', repos='http://cran.us.r-project.org')
#install.packages("devtools")
#devtools::install_github("matthewjdenny/GERGM")

setwd(paste(dataloc))


library(sna)


#make origin-destination matrix for 2010, 2013
net10 <- as.matrix(read.csv("Japan Migration 2010.csv", header = F))
net13 <- as.matrix(read.csv("Japan Migration 2013.csv", header = F))

net_cov <- as.matrix(read.csv("Japan_cov.csv", header = T))

netchange10_13 <- net13 - net10

colnames(netchange10_13) <- c(1:47)
rownames(net_cov)<- c(1:47)

formula <- netchange10_13 ~ edges + sender("quake") + receiver("quake") + sender("pop")  + receiver("pop")+ sender("tfr_diff")  + receiver("tfr_diff")+ ttriads(alpha = 0.8)+ mutual(alpha = 0.8)

test1 <- gergm(formula,
              covariate_data = net_cov,
              normalization_type = "division",
              network_is_directed = TRUE,
              use_MPLE_only = FALSE,
              estimation_method = "Metropolis",
              number_of_networks_to_simulate = 400000,
              thin = 1/100,
              proposal_variance = 0.05,
              downweight_statistics_together = TRUE,
              MCMC_burnin = 10000,
              seed = 456,
              convergence_tolerance = 0.5)


test1
