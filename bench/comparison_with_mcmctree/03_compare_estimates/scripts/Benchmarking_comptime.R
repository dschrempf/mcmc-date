#-------------------#
# CLEAN ENVIRONMENT #
#-------------------#
rm( list = ls( ) )

#-------#
# TASKS #
#-------#
# Code extracted from StackOverflow:
# https://stackoverflow.com/questions/12034424/convert-hourminutesecond-hhmmss-string-to-proper-time-class
# Prior (MCMCtree)
time_priorMT  <- c( "00:00:25","00:00:31","00:00:34",
                    "00:00:31","00:00:28","00:00:26" )
convt_priorMT <- as.difftime( time_priorMT, units = "mins" ) 
secs_priorMT  <- as.numeric( convt_priorMT, units = "secs" )
hours_priorMT <- as.numeric( convt_priorMT, units = "hours" )
mean_secs_priorMT  <- mean( secs_priorMT ) # 29.16667 seconds ~ 29s
mean_hours_priorMT <- mean( hours_priorMT ) # 0.008101852h ~ 0.008h

# Prior (McmcDate)
time_priorMD  <- c( "00:02:33","00:02:29","00:02:39",
                    "00:01:47","00:01:47","00:01:49" )
convt_priorMD <- as.difftime( time_priorMD, units = "mins" ) 
secs_priorMD  <- as.numeric( convt_priorMD, units = "secs" )
hours_priorMD <- as.numeric( convt_priorMD, units = "hours" )
mean_secs_priorMD  <- mean( secs_priorMD ) # 130.6667 seconds ~ 131s
mean_hours_priorMD <- mean( hours_priorMD ) # 0.0362963 h ~ 0.036h

# Posterior (MCMCtree)
time_postMT  <- c( "00:01:59","00:02:09","00:02:14",
                   "00:02:02","00:01:55","00:01:56" )
convt_postMT <- as.difftime( time_postMT, units = "mins" ) 
secs_postMT  <- as.numeric( convt_postMT, units = "secs" )
hours_postMT <- as.numeric( convt_postMT, units = "hours" )
mean_secs_postMT  <- mean( secs_postMT ) # 122.5 seconds ~ 123s
mean_hours_postMT <- mean( hours_postMT ) # 0.03402778h ~ 0.034h

# Posterior (McmcDate)
time_postMD  <- c( "00:03:07","00:03:04","00:02:29",
                   "00:02:02","00:02:17","00:02:23" )
convt_postMD <- as.difftime( time_postMD, units = "mins" ) 
secs_postMD  <- as.numeric( convt_postMD, units = "secs" )
hours_postMD <- as.numeric( convt_postMD, units = "hours" )
mean_secs_postMD  <- mean( secs_postMD ) # 153.6667 seconds ~ 154s
mean_hours_postMD <- mean( hours_postMD ) # 0.04268519h ~ 0.043h

# ------------------------------------------------------------------- #
pb <- 5      # hours
codeml <- 0  # hours
total_MCMCtree <- c( codeml + mean_hours_priorMT + mean_hours_postMT )
# 0.04212963 ~ 0.042h
total_McmcDate <- c( pb + mean_hours_priorMD + mean_hours_postMD )
# 5.078981 ~ 5.08h


