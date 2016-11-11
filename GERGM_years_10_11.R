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
net10 <- as.matrix(read.csv("Japan Migration 2010.csv", header = F))
net11 <- as.matrix(read.csv("Japan Migration 2011.csv", header = F))
net12 <- as.matrix(read.csv("Japan Migration 2012.csv", header = F))
net_cov <- as.matrix(read.csv("Japan_cov.csv", header = T))


#indegree (number of people who came to the city)
indeg10 <- degree(net10,cmode="indegree")
indeg11 <- degree(net11,cmode="indegree")
indeg12 <- degree(net12,cmode="indegree")
indeg10_n <- degree(net10,cmode="indegree")/net_cov[,2]
indeg11_n <- degree(net11,cmode="indegree")/net_cov[,2]
indeg12_n <- degree(net12,cmode="indegree")/net_cov[,2]
indeg <- cbind(indeg10,indeg10_n, indeg11, indeg11_n,indeg12,indeg12_n)
write.table(indeg, paste(outloc, file = "indegree.txt", sep=""),sep="\t")

#outdegree(number of people who left the city)
outdeg10 <- degree(net10,cmode="outdegree")
outdeg11 <- degree(net11,cmode="outdegree")
outdeg12 <- degree(net12,cmode="outdegree")
outdeg10_n <- degree(net10,cmode="outdegree")/net_cov[,2]
outdeg11_n <- degree(net11,cmode="outdegree")/net_cov[,2]
outdeg12_n <- degree(net12,cmode="outdegree")/net_cov[,2]
outdeg <- cbind(outdeg10,outdeg10_n, outdeg11, outdeg11_n,outdeg12,outdeg12_n)
write.table(outdeg, paste(outloc, file = "outdegree.txt", sep=""),sep="\t")


#make origin-destination matrix for 2010, 2011, 2012
net10 <- as.matrix(read.csv("Japan Migration 2010.csv", header = F))
net11 <- as.matrix(read.csv("Japan Migration 2011.csv", header = F))
net12 <- as.matrix(read.csv("Japan Migration 2012.csv", header = F))

net_cov <- as.matrix(read.csv("Japan_cov.csv", header = T))

netchange10_11 <- net11 - net10

colnames(netchange10_11) <- c(1:47)
rownames(net_cov)<- c(1:47)

formula <- netchange10_11 ~ edges + sender("quake") + receiver("quake") + sender("pop")  + receiver("pop")+ sender("tfr_diff")  + receiver("tfr_diff")+ttriads(alpha = 0.5)+mutual(alpha = 0.5)

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
              convergence_tolerance = 0.5,
              output_directory = outloc,
              output_name = "years_10_11")


test1
