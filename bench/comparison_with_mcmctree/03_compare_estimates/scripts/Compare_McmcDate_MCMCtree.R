#-------------------#
# CLEAN ENVIRONMENT #
#-------------------#
rm( list = ls( ) )

#---------------------------------------------------#
# SET WORKING DIRECTORY AND LOAD IN-HOUSE FUNCTIONS #
#---------------------------------------------------#
# This package lets you find automatically the path
# If you have not installed this package, you will need to install it. 
# You can uncomment the following line to do this:
#install.packages( "rstudioapi" )
library( rstudioapi ) 
# Get the path to current open R script and find main dir
path_to_file <- getActiveDocumentContext()$path
wd <- paste( dirname( path_to_file ), "/", sep = "" )
setwd( wd )
# This script and the `Functions.R` script are saved under
# a directory called `scripts`. We will remove this part from 
# the path to find our home directory
home_dir <- gsub( pattern = "scripts/", replacement = "", x = wd )
# Load scripts with function
source( "Functions.R" )

#-------#
# TASKS #
#-------#
## Notation for object names:
##  * `MD`: `McmcDate` --> `<M>cmc<D>ate`
##  * `MT`: `MCMCtree` --> `<M>CMC<t>ree`

# Read trees
#   --> tt1: tree with node labels used by `MCMCtree`
#   --> tt2: tree with node labels used by `McmcDate` format
tt1           <- ape::read.tree( file = "../../01_PAML/01_MCMCtree/00_prior/node_tree.tree" )
tt1$tip.label <- gsub( pattern = "^[0-9]*_", replacement = "",
                       x = tt1$tip.label )
tt2           <- ape::read.tree( file = "../../02_McmcDate/01_McmcDate/results-calibrations-ulognormal-prior-run1/mtCDNApri.meantree.index" )
# Read samples collected by MCMCtree ----
MT_CLK        <- read.table( file = "../../01_PAML/01_MCMCtree/00_prior/CLK/mtcdnapri/mtcdnapri_all_mean_est.tsv" )
MT_ILN        <- read.table( file = "../../01_PAML/01_MCMCtree/01_posterior/ILN/mtcdnapri_ILN/mtcdnapri_ILN_all_mean_est.tsv" )
node_labs_MT  <- as.numeric( gsub( x = rownames( MT_CLK ),
                                  pattern = "t_n|_..*", replacement = "" ) )
node_const    <- MT_CLK[,4]
# Read samples collected by McmcDate ----
num_chains <- 6
MD_CLK_sum <- format_inp_McmcDate( agesfull = "mtCDNApri.timetree.ages.full",
                                   path_agesfull = "../../02_McmcDate/01_McmcDate/results-calibrations-ulognormal-prior-run",
                                   outdir = "../../03_compare_estimates/",
                                   num_chains = num_chains,
                                   prior = TRUE )
MD_ILN_sum <- format_inp_McmcDate( agesfull = "mtCDNApri.timetree.ages.full",
                                   path_agesfull = "../../02_McmcDate/01_McmcDate/results-calibrations-ulognormal-sparse-run",
                                   outdir = "../../03_compare_estimates/",
                                   num_chains = num_chains,
                                   prior = FALSE )
# Read calibration file
calib_f <- read.table( file = "../../00_inp_data/calibs/raw_calibs/calibrations.txt",
                       sep = ";", header = TRUE,
                       stringsAsFactors = FALSE )

#-------#
# TASKS #
#-------#
## Find matching nodes ----
# Map `node_names_MD` with corresponding tips in inp tree
node_names_tt2 <- rep( "", lengh = length( tt2$tip.label ) )
count <- 0
##> CHECK
all.equal( MD_CLK_sum$nlab, MD_ILN_sum$nlab )
all.equal( MD_CLK_sum$nnames, MD_ILN_sum$nnames )
# Yes! We can use either when extracting node labels
##> END CHECK
for( i in tt2$tip.label ){
  count <- count + 1
  tmp_id         <- which( as.numeric( i ) == MD_CLK_sum$nlab )
  node_names_tt2[count] <- MD_CLK_sum$nnames[tmp_id]
}
# Replace tip numbers with sp. names
tt2$tip.label <- node_names_tt2
## Compare time estimates and compute sum stats 
matchnodes_MTnMD( tt_MT = tt1, tt_MD = tt2, calibf = calib_f,
                  node_labs_MD = MD_CLK_sum$nlab,
                  node_labs_MT = node_labs_MT,
                  MD_CLK = MD_CLK_sum$all_mcmc,
                  MD_ILN = MD_ILN_sum$all_mcmc,
                  outdir = paste( wd, "../", sep = "" ) )

#--------------#
# MCMC objects #
#--------------#
# Load mcmc files for all datasets
mcmc_obj          <- vector( "list", 4 )
names( mcmc_obj ) <- c( "CLK-MCMCtree", "CLK-McmcDate",
                        "ILN-MCMCtree", "ILN-McmcDate" )
# Set options
perc <- 0.975 # Percentile to calculate the quantiles. Default: 0.97 so that
              # the 95%CIs are calculated
# Parsing data
mcmc_obj[[ 1 ]] <- load_dat( mcmc = "../../01_PAML/01_MCMCtree/00_prior/mcmc_files_mtcdnapri_CLK/mcmc.txt",
                             delcol = 1, perc = perc,
                             def_samples = 120005,
                             prior = TRUE )
mcmc_obj[[ 2 ]] <- load_dat_McmcDate( mcmc = "../prior_allsamples.tsv",
                                      compdivt = "../out/compare_divtimes.tsv",
                                      perc = perc )
mcmc_obj[[ 3 ]] <- load_dat( mcmc = "../../01_PAML/01_MCMCtree/01_posterior/mcmc_files_mtcdnapri_ILN/mcmc.txt",
                             delcol = 1, perc = perc,
                             def_samples = 120005,
                             prior = TRUE )
mcmc_obj[[ 4 ]] <- load_dat_McmcDate( mcmc = "../post_allsamples.tsv",
                                      compdivt = "../out/compare_divtimes.tsv",
                                      perc = perc )

save( mcmc_obj, file = "../out/mcmc_obj.RData" )

#-------#
# PLOTS #
#-------#
# Plot calibrated nodes
nodes2plot  <- c( 1,2,4 ) #0-t8, 1-t9, 3-t11
name_labs   <- calib_f[,1]
cols <- c( "blue", "brown", "darkgreen", "grey", "purple", "black" )

## PRIOR
labs_in_csv <- colnames(mcmc_obj[[2]]$divt)[nodes2plot]
lim_x       <- c( 0, 60 )
lim_y       <- c( 0, 1 ) # arbitrary density numbers, can be modified a 
                         # a posteriori
x_plotlab   <- 0
plots_MTvsMT( outdir = paste( wd, "../", sep = "" ), clock = "CLK",
              labs_in_csv = labs_in_csv, lim_x = lim_x,
              lim_y = lim_y, x_plotlab = x_plotlab, # by eye
              mcmc_MT = mcmc_obj$`CLK-MCMCtree`,
              mcmc_MD = mcmc_obj$`CLK-McmcDate`, out_format = "pdf" )
plots_MTvsMT( outdir = paste( wd, "../", sep = "" ), clock = "CLK",
              labs_in_csv = labs_in_csv, lim_x = lim_x,
              lim_y = lim_y, x_plotlab = x_plotlab, # by eye
              mcmc_MT = mcmc_obj$`CLK-MCMCtree`,
              mcmc_MD = mcmc_obj$`CLK-McmcDate`, out_format = "jpg" )
## POSTERIOR
lim_x       <- c( 0, 40 )
plots_MTvsMT( outdir = paste( wd, "../", sep = "" ), clock = "ILN",
              labs_in_csv = labs_in_csv, lim_x = lim_x,
              lim_y = lim_y, x_plotlab = x_plotlab, # by eye
              mcmc_MT = mcmc_obj$`ILN-MCMCtree`,
              mcmc_MD = mcmc_obj$`ILN-McmcDate`, out_format = "pdf" )
plots_MTvsMT( outdir = paste( wd, "../", sep = "" ), clock = "ILN",
              labs_in_csv = labs_in_csv, lim_x = lim_x,
              lim_y = lim_y, x_plotlab = x_plotlab, # by eye
              mcmc_MT = mcmc_obj$`ILN-MCMCtree`,
              mcmc_MD = mcmc_obj$`ILN-McmcDate`, out_format = "jpg" )
