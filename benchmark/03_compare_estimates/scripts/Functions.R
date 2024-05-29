#--------------------#
# DEFINE GLOBAL FUNS #
#--------------------#

# Load packages required in functions
library( stringr )

# Function to load the `mcmc.txt` files.
# Used to get mean estimates for convergence plots.
# 
# Parameters:
# mcmc         Character, path to `mcmc.txt` file.
# delcol       Numeric, number of columns that are to be deleted 
#              as they do not contain divtimes. You can read the header
#              of the `mcmc.txt` files and count the number of `mu*` and
#             `sigma2*` elements. Do not count the `lnL` value as this is
#              automatically deleted in the calculations below. 
#              Assuming an MCMC run under a relaxed-clock model with no 
#              partitions, we would see `mu` and `sigma2` columns. Therefore,
#              `delcol = 2`. The default value is 2, but you will need to change
#              it if you run `MCMCtree` with different settings.
# perc         Numeric. Percentile to calculate the quantiles. Default: 0.975.
# def_samples  Numeric. Number of samples that the user defined through the
#              `MCMCtree` option `nsample. 
# prior        Boolean. TRUE if `MCMCtree` has been sampling from the prior, FALSE
#              otherwise
load_dat <- function( mcmc, delcol = 3, perc = 0.975, def_samples = 20000,
                      prior = FALSE )
{
  
  # 1. Load files and get parameters. Load everything with option `fill=TRUE`
  # in case a line is not complete
  cat( "Load \"mcmc.txt\" from path \n", mcmc, "\n... ...\n" )
  run_tmp  <- read.table( mcmc, header = TRUE, fill = TRUE, sep = "\t" )
  test_len <- def_samples+1
  #print( test_len )
  #print( dim(run_tmp))
  if( test_len != dim(run_tmp)[1] ){
    cat( " [[ The last line of the \"mcmc.txt\" is incomplete and will be removed ]] \n\n" )
    run <- matrix( 0, nrow = c(dim( run_tmp )[1]-1), ncol = dim( run_tmp )[2] )
    run <- run_tmp[1:c(dim( run_tmp )[1]-1),]
  }else{
    cat( " [[ MCMCtree collected the same amount of samples you specified ]] \n",
         " [[       All lines will be kept in the \"mcmc.txt\"            ]] \n\n" )
    run <- run_tmp
  }
  
  # 2. Summarise parameters
  cat( "Generate objects with summarised estimated divergence times... ...\n")
  dim.r   <- dim(run)[2]
  # divtimes <- run[,-c(1, dim(run)[2])]
  # If `MCMCtree` has sampled from the prior, then the `lnL` column will not
  # appear and we need to subtract `1` from the `delcol`
  if( prior == TRUE ){
    delcol <- delcol - 1
    divtimes <- run[,-c( 1, ( dim.r-delcol ):dim.r )]
  }else{
    divtimes <- run[,-c( 1, ( dim.r-delcol ):dim.r )]
  }
  # Calculate mean and quantiles
  mean_est_divt  <- apply( X = divtimes, MARGIN = 2, FUN = mean )
  quant_est_divt <- apply( X = divtimes, MARGIN = 2, FUN = quantile, probs = c( 1-perc,perc ) )
  quant_est_divt <- t( quant_est_divt )
  test_alleq     <- all.equal( quant_est_divt[1,], quantile( divtimes[,1], probs = c( 1-perc,perc ) ) )
  if( test_alleq != TRUE ){
    stop( "There was an issue calculating quantiles!" )
  }
  
  # 3. Return object 
  cat( "\nTasks done! Returning objects\n\n")
  return( list( divt = divtimes, mean_divt = mean_est_divt, quant_divt = quant_est_divt ) )
  
}


# Function to load the `mcmc.txt` files output by `McmcDate` after being
# reformatted
# Used to get mean estimates for convergence plots.
# 
# Parameters:
# mcmc         Character, path to `mcmc.txt` file.
# comp_divt    Character, path to the output file that compares MCMCtree nodes
#              with McmcDate nodes
# perc         Numeric. Percentile to calculate the quantiles. Default: 0.975.
load_dat_McmcDate <- function( mcmc, compdivt, perc = 0.975 )
{
  
  # 0. Load comparison table
  comp_divt <- read.table( compdivt, header = TRUE, sep = "\t" )
  # 1. Load files and get parameters. Load everything with option `fill=TRUE`
  # in case a line is not complete
  cat( "Load \"mcmc.txt\" from path \n", mcmc, "\n... ...\n" )
  run  <- read.table( mcmc, header = TRUE, fill = TRUE, sep = "\t" )
  # 2. Get equivalent nodes
  coln_raw <- as.numeric( gsub( x = colnames( run )[-1], pattern = "X",
                                replacement = "" ) )
  colnames( run )[2:dim(run)[2]] <- coln_raw
  count <- 0
  for( i in comp_divt$McmcDate ){
    count <- count + 1
    match_ind  <- which( coln_raw %in% i )
    MT_ind     <- comp_divt$MCMCtree[count]
    colnames( run )[match_ind+1] <- paste( colnames( run )[match_ind+1],
                                           "-t_n", MT_ind, sep = "" )
  }
  # 3. Summarise parameters
  cat( "Generate objects with summarised estimated divergence times... ...\n")
  dim.r   <- dim(run)[2]-1
  # Remove first column, which contains the num. iterations
  divtimes <- run[,-c( 1 )]
  # Calculate mean and quantiles
  mean_est_divt  <- apply( X = divtimes, MARGIN = 2, FUN = mean )
  quant_est_divt <- apply( X = divtimes, MARGIN = 2, FUN = quantile, probs = c( 1-perc,perc ) )
  quant_est_divt <- t( quant_est_divt )
  test_alleq     <- all.equal( quant_est_divt[1,], quantile( divtimes[,1], probs = c( 1-perc,perc ) ) )
  if( test_alleq != TRUE ){
    stop( "There was an issue calculating quantiles!" )
  }
  
  # 3. Return object 
  cat( "\nTasks done! Returning objects\n\n")
  return( list( divt = divtimes, mean_divt = mean_est_divt, quant_divt = quant_est_divt ) )
  
}


# Function to format the input file <name_dat>.timetree.ages.full to load
# a readable matrix object and generate an output file useful for 
# benchmarking
#
# Arguments  Character
#
# agesfull  Character, path to the input file <name_dat>.timetree.ages.full
# outdir    Character, path to the output directory where the
#           `[prior|post]_samples.csv` will be saved.
# prior     Boolean, TRUE if the samples were collected when sampling from the
#           prior, FALSE otherwise.
format_inp_McmcDate <- function( agesfull, path_agesfull, outdir,
                                 prior = FALSE, num_chains ){

  # Get vector to keep track of columns
  num_rows <- 0
  num_rows_per_mat <- vector( mode = "numeric", length = num_chains )
  ind_MCMC <- vector( mode = "list", length = num_chains )
  names( ind_MCMC ) <- paste( "run", 1:6, sep = "" )
  for( i in 1:num_chains ){
    # Read output file that ends with <name_dat>.timetree.ages.full
    name_resdir <- gsub( pattern = "..*\\/", replacement = "",
                         x = path_agesfull )
    cat( paste( "\n[[ Formatting McmcDate output file:\n", name_resdir, i, "/",
                agesfull, " ]]\n", sep = "" ) )
    MD_mcmc_raw <- read.table( file = paste( path_agesfull, i, "/",
                                             agesfull, sep = "" ),
                               header = FALSE, sep = "\t",
                               stringsAsFactors = FALSE, skip = 1 )
    node_labs_MD   <- MD_mcmc_raw[,1]
    node_names_MD  <- MD_mcmc_raw[,2]
    pos_tips       <- which( grepl( pattern = "[A-Za-z]",
                                    x = node_names_MD ) == TRUE )
    # Get transposed to generate output file compatible with `Tracer` and
    # scripts for MCMC diagnostics
    MD_mcmc_notips <- MD_mcmc_raw[-pos_tips,]
    MD_mcmc_T      <- t(MD_mcmc_notips)[c(1,3:dim(t(MD_mcmc_notips))[1]),]
    MD_mcmc  <- matrix( 0, nrow = dim(MD_mcmc_T)[1]-1,
                              ncol = dim(MD_mcmc_T)[2]+1)
    # First columnn: iterations
    MD_mcmc[,1] <- 1:dim(MD_mcmc)[1]
    # Second column: samples | First convert into numeric before being able to
    # generate a matrix
    prior_samples <- matrix( as.numeric( MD_mcmc_T[c(2:dim(MD_mcmc_T)[1]),] ),
                             nrow = dim(MD_mcmc_T)[1]-1,
                             ncol = dim(MD_mcmc_T)[2] )
    MD_mcmc[,2:dim(MD_mcmc)[2]] <- prior_samples
    # Header
    colnames( MD_mcmc ) <- c( "Iter", as.numeric( MD_mcmc_T[1,] ) )
    cat( "\n TASKS FINISHED!\n",
         " --> You can find the formatted output file in the following",
         "directory:\n", outdir, "\n" )
    if( prior == TRUE ){
      write.table( x = MD_mcmc, file = paste( outdir, "/prior_samples_run",
                                              i, ".tsv", sep = "" ),
                   quote = FALSE, sep = "\t", col.names = TRUE, row.names = FALSE )
    }else{
      write.table( x = MD_mcmc, file = paste( outdir, "/post_samples_run",
                                              i, ".tsv", sep = "" ),
                   quote = FALSE, sep = "\t", col.names = TRUE, row.names = FALSE )
    }
    num_rows <- num_rows + dim( MD_mcmc )[1]
    num_rows_per_mat[i] <- dim( MD_mcmc )[1]
    ind_MCMC[[ i ]] <- MD_mcmc
  }
  # Create a big matrix for all the runs
  all_MCMC <- matrix( 0, nrow = num_rows, ncol = dim( MD_mcmc )[2] )
  start <- stop <- 0
  for( i in 1:num_chains ){
    start <- stop + 1
    stop <- start + num_rows_per_mat[i] - 1
    #cat( "start: ", start , "| stop: ", stop, "\n" )
    all_MCMC[start:stop,] <- ind_MCMC[[ i ]]
  }
  all_MCMC[,1] <- c( 1:dim(all_MCMC)[1] )
  colnames( all_MCMC ) <- colnames( ind_MCMC[[ 1 ]] )
  if( prior == TRUE ){
    write.table( x = all_MCMC, file = paste( outdir, "/prior_allsamples.tsv",
                                             sep = "" ),
                 quote = FALSE, sep = "\t", col.names = TRUE, row.names = FALSE )
  }else{
    write.table( x = all_MCMC, file = paste( outdir, "/post_allsamples.tsv",
                                             sep = "" ),
                 quote = FALSE, sep = "\t", col.names = TRUE, row.names = FALSE )
  }
  # Return objects
  return( list( nlab = node_labs_MD, ind_mcmc = all_MCMC, all_mcmc = all_MCMC,
                nnames = node_names_MD ) )
  
}

# Function to generate a table that will help benchmark the results obtained
# with both programs
#
# Arguments
# 
# tt_MT  Phylo object, object `tt1` previously generated.
# tt_MD  Phylo object, object `tt2` previously generated, and which species
#        names should have already been updated.
# calibf List, object `calib_f` previously generated
matchnodes_MTnMD <- function( tt_MT, tt_MD, calibf, node_labs_MD, node_labs_MT,
                              MD_CLK, MD_ILN, outdir )
{
  # Go over calibration file and find matching nodes
  match_nodes_MDMT <- matrix( 0, ncol = 20, nrow = dim( calibf )[1] )
  colnames( match_nodes_MDMT ) <- c( "name_calib",
                                     "McmcDate", "MCMCtree",
                                     "leaf1", "leaf2",
                                     "SE_MDvsMT_CLK", "SE_MDvsMT_ILN",
                                     "MD_CLK-mean_t", "MD_CLK-q2.5%", "MD_CLK-q97.5%",
                                     "MT_CLK-mean_t", "MT_CLK-q2.5%", "MT_CLK-q97.5%",
                                     "MD_ILN-mean_t", "MD_ILN-q2.5%", "MD_ILN-q97.5%",
                                     "MT_ILN-mean_t", "MT_ILN-q2.5%", "MT_ILN-q97.5%",
                                     "prior" )
  for( i in 1:dim( calibf )[1] ){
    # Get MRCA for these two tips
    mrca_MT <- ape::getMRCA( phy = tt_MT, tip = c( calibf[i,2],
                                                 calibf[i,3]) )
    mrca_MD <- ape::getMRCA( phy = tt_MD, tip = c( calibf[i,2],
                                                 calibf[i,3]) )
    # Find node labels for MRCA
    match_nodes_MDMT[i,1] <- calibf[i,1]
    match_nodes_MDMT[i,2] <- tt_MD$node.label[mrca_MD-ape::Ntip(tt_MD)]
    match_nodes_MDMT[i,3] <- tt_MT$node.label[mrca_MT-ape::Ntip(tt_MT)]
    match_nodes_MDMT[i,4] <- calibf[i,2]
    match_nodes_MDMT[i,5] <- calibf[i,3]
    # McmcDate: find mean divt, q2.5%, q97.5% (CLK)
    ind_MD        <- which( match_nodes_MDMT[i,2] == node_labs_MD )
    tmp_MDsamples <- MD_CLK[,c(1+ind_MD)]
    tmp_MDq       <- quantile( tmp_MDsamples, probs = c( 0.025, 0.975 ) )
    match_nodes_MDMT[i,8]  <- round( mean( tmp_MDsamples ), 3 ) # mean
    match_nodes_MDMT[i,9]  <- round( tmp_MDq[1], 3 ) #2.5%q
    match_nodes_MDMT[i,10] <- round( tmp_MDq[2], 3 ) #97.5%q
    # MCMCtree: find mean divt, q2.5%, q97.5% (CLK)
    ind_MT <- which( match_nodes_MDMT[i,3] == node_labs_MT )
    match_nodes_MDMT[i,11] <- MT_CLK[ind_MT, 1] # mean
    match_nodes_MDMT[i,12] <- MT_CLK[ind_MT, 2] # 2.5%q
    match_nodes_MDMT[i,13] <- MT_CLK[ind_MT, 3] # 97.5%q
    # McmcDate: find mean divt, q2.5%, q97.5% (ILN)
    tmp_MDsamples <- MD_ILN[,c(1+ind_MD)]
    tmp_MDq       <- quantile( tmp_MDsamples, probs = c( 0.025, 0.975 ) )
    match_nodes_MDMT[i,14] <- round( mean( tmp_MDsamples ), 3 ) # mean
    match_nodes_MDMT[i,15] <- round( tmp_MDq[1], 3 ) # 2.5%q
    match_nodes_MDMT[i,16] <- round( tmp_MDq[2], 3 ) # 97.5%q
    # MCMCtree: Find mean divt, q2.5%, q97.5% (ILN)
    match_nodes_MDMT[i,17] <- MT_ILN[ind_MT, 1] # mean
    match_nodes_MDMT[i,18] <- MT_ILN[ind_MT, 2] # 2.5%q
    match_nodes_MDMT[i,19] <- MT_ILN[ind_MT, 3] # 97.5%q
    # S.E. in time estimates - McmcDate vs MCMCtree (CLK and ILN)
    match_nodes_MDMT[i,6]  <- round( sd( x = match_nodes_MDMT[i,c(8,11)] ), 3 )
    match_nodes_MDMT[i,7]  <- round( sd( x = match_nodes_MDMT[i,c(14,17)] ), 3 )
    # Include prior that was used to constrain this node age
    match_nodes_MDMT[i,20] <- MT_CLK[ind_MT,4]
  }
  
  ## Create out dir and save comparison table ----
  if( ! dir.exists( paste( outdir, "/out", sep = "" ) ) ){
    dir.create( paste( outdir, "/out", sep = "" ) ) 
  }
  write.table( x = match_nodes_MDMT, file = paste( outdir,
                                                   "/out/compare_divtimes.tsv",
                                                   sep = "" ),
               sep = "\t", quote = FALSE, row.names = FALSE,
               col.names = TRUE )
}



# Functions to plot densities when estimated with the samples collected by
# `MCMCtree` and `McmcDate`
#
# Arguments
#
# outdir
# clock        Character, "CLK", "ILN", or "GBM; type of clock model.
#              "CLK": strict clock model; "ILN": independent-rates log-normal
#              model; "GBM": Geometric Brownian motion model.
# labs_in_csv  Character, equivalent node notation for `MCMCtree` and
#              `McmcDate`. Vector generated before running this script.
# lim_x        Character, boundaries to plot the x axis.
# lim_y        Character, boundaries to plot the y axis.
# x_plotlab    Numeric, x value at which the node labels will be written.
#              Try any number, see how the plot looks like, then adjust.
# mcmc_MT      List, `mcmc_obj[[ X ]]` previously created. Replace `X` with the
#              number of the entry with the samples collected by `MCMCtree` 
#              that you want to plot.
# mcmc_MD      List, `mcmc_obj[[ X ]]` previously created. Replace `X` with the
#              number of the entry with the samples collected by `McmcDate` 
#              that you want to plot.
plots_MTvsMT <- function( outdir, clock, labs_in_csv, lim_x, lim_y,
                          x_plotlab = 30, mcmc_MT, mcmc_MD, out_format )
{
  
  # Start plotting!
  if( out_format == "pdf" ){
    pdf( file = paste( outdir, "out/compare_MCMCtreeVSMcmcDate_", clock, ".pdf",
                       sep = "" ), paper = "a4r", width = 0, height = 0 )
  }else{
    jpeg( filename = paste( outdir, "out/compare_MCMCtreeVSMcmcDate_", clock, 
                            ".jpg", sep = "" ),
          width = 1024, height = 768, quality = 100 )
  }
  for( i in 1:length(labs_in_csv) ){
    # Get max/min x and y
    ##> NOTE: Only the first number is used, because the other number/s in 
    ##> `dup_dat[[i]` have the same fossil info!
    lab_MT <- gsub( x = labs_in_csv[i], pattern = "..*-", replacement = "" )
    tn_matrix_MD <- which( colnames(mcmc_MD$divt) %in% labs_in_csv[i] )
    tn_matrix_MT <- which( colnames(mcmc_MT$divt) %in% lab_MT )
    # Get name of calibration, which corresponds to the first calibration
    # in the list
    tmp_name_calib <- calib_f[i,1]
    # Start plotting densities
    # Get temporary matching column in `mcmc_obj` to `j`
    dens2plot_MT <- density( unlist( mcmc_MT$divt[ tn_matrix_MT ]*100 ),
                             adj = 1 )
    dens2plot_MD <- density( unlist( mcmc_MD$divt[ tn_matrix_MD ] ),
                             adj = 1 )
    transp <- t( col2rgb( col = cols[i] ) )
    if( i == 1 ){
      plot( dens2plot_MT,
            main = "Comparing densities - MCMCtree vs McmcDate",
            col = cols[i], ylim = lim_y, xlim = lim_x )
      polygon( dens2plot_MT, col = "lightblue" )
      lines( dens2plot_MD, col = cols[i], lty = 2 )
      lines( dens2plot_MT, col = cols[i] )
    }else{
      lines( dens2plot_MT, col = cols[i] )
      polygon( dens2plot_MT, col = rgb( transp[1]/255, transp[2]/255,
                                        transp[3]/255, 0.2 ) )
      lines( dens2plot_MD, col = cols[i], lty = 2 )
    }
    # Add node labels on plot
    plot_coords <- c( x_plotlab, max( density( unlist( mcmc_MD$divt[ tn_matrix_MD ] ) )$y ) )
    text( x = plot_coords[1], y = plot_coords[2],
          labels = colnames( mcmc_MD$divt[ tn_matrix_MD ] ),
          col = cols[i], cex = 1.1 )
    legend( "topright", legend = c( "MCMCtree", "McmcDate" ),
            lty = c( 1, 2 ), cex = 2 )
  }
  dev.off()
  # Change densities with filling
  if( out_format == "pdf" ){
    pdf( file = paste( outdir, "out/compare_McmcDateVSMCMCtree_", clock,
                       ".pdf", sep = "" ), 
         paper = "a4r", width = 0, height = 0 )
  }else{
    jpeg( filename = paste( outdir, "out/compare_McmcDateVSMCMCtree_", clock, 
                            ".jpg", sep = "" ),
          width = 1024, height = 768, quality = 100 )
  }
  for( i in 1:length(labs_in_csv) ){
    # Get max/min x and y
    ##> NOTE: Only the first number is used, because the other number/s in 
    ##> `dup_dat[[i]` have the same fossil info!
    lab_MT <- gsub( x = labs_in_csv[i], pattern = "..*-", replacement = "" )
    tn_matrix_MD <- which( colnames(mcmc_MD$divt) %in% labs_in_csv[i] )
    tn_matrix_MT <- which( colnames(mcmc_MT$divt) %in% lab_MT )
    # Get name of calibration, which corresponds to the first calibration
    # in the list
    tmp_name_calib <- calib_f[i,1]
    # Start plotting densities
    # Get temporary matching column in `mcmc_obj` to `j`
    dens2plot_MT <- density( unlist( mcmc_MT$divt[ tn_matrix_MT ] )*100,
                             adj = 1 )
    dens2plot_MD <- density( unlist( mcmc_MD$divt[ tn_matrix_MD ] ),
                             adj = 1 )
    transp <- t( col2rgb( col = cols[i] ) )
    if( i == 1 ){
      plot( dens2plot_MD,
            main = "Comparing densities - McmcDate vs MCMCtree",
            col = cols[i], ylim = lim_y, xlim = lim_x )
      polygon( dens2plot_MD, col = "lightblue" )
      lines( dens2plot_MT, col = cols[i], lty = 2 )
      
    }else{
      lines( dens2plot_MD, col = cols[i] )
      polygon( dens2plot_MD, col = rgb( transp[1]/255, transp[2]/255,
                                        transp[3]/255, 0.2 ) )
      lines( dens2plot_MT, col = cols[i], lty = 2 )
    }
    # Add node labels on plot
    plot_coords <- c( x_plotlab, max( density( unlist( mcmc_MD$divt[ tn_matrix_MD ] ) )$y ) )
    text( x = plot_coords[1], y = plot_coords[2],
          labels = colnames( mcmc_MD$divt[ tn_matrix_MD ] ),
          col = cols[i], cex = 1.1 )
    legend( "topright", legend = c( "MCMCtree", "McmcDate" ),
            lty = c( 2, 1 ), cex = 2 )
  }
  dev.off()
  
}
