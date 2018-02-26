# Plotting will only be done if DEBUG == TRUE
DEBUG <- TRUE
SHOWTIME <- FALSE
PLOT_ROTATE <- FALSE
PLOT_DECOMPOSE <- TRUE
PLOT_PREDICT_HIDDEN <- TRUE
PLOT_PREDICT_ACTUAL <- TRUE
PLOT_DEVIATIONS <- TRUE
PLOT_ANOMALIES <- TRUE

# prediction sample size
# should be a fairly long interval to capture variances and means needed for calculating normalized deviation scores
# PS! this parameter does not affect batch.R that will "predict" entire last day
# PPS! number of available bins depends on BIN size parameter that will be discussed below
PREDICT_BINS <- 500
# display alerts from last N bins of PREDICT_BINS matrix
ALERT_BINS <- 5

# total no. cycles, or days, used for training
# should match --days parameter for data query script
# ! MUST BE IN DAYS
CYCLES <- 7
# aggregation size of data, e.g., 300 = 5 minute mean/median represents a data point. Change according to influx "GROUP BY time(Ns)" query parameter
# PS! MUST BE IN SECONDS
BIN <- 60

# does not need to be changed
SEASONS <- 86400 / BIN


# number of data points to smooth with one-sided moving average
# for pre-processing, not trend recognition
# use NA to disable
SMOOTH_BATCH <- 15
SMOOTH_INCRE <- 10
# number of principal components to use for actual predictions
# visually verify PCA rotated data plots when choosing an appropriate number
# alternatively, set value to NA to choose all components where eigenvalue > 1
HIDDEN <- NA
# ovarall confidence level to consider
# inverse normalized DTW distance is used to measure the quality of our predictions
CONFIDENCE <- 0.95

# statistical deviation values used for alert thresholding
# 3 is conisdered statistically sound, though higher values may need to be used in practice (depends on the amount of noise)
# my data is usually noisy, so I'm going higher
ALARM_NOTICE <- 3
ALARM_WARN <- 4
ALARM_CRIT <- 5
REPORT <- ALARM_CRIT

DATA_ROOT <- "/home/vagrant/data/mk/"
PROJECT_ROOT <- "/home/vagrant/spiritedAway"

DATA_INCREMENTAL <- "/home/vagrant/data/data.csv"
# TODO for markus - start using a proper database
# though finding a good N*M matrix blob oriented database is suprisingly difficult to find
DUMP_COVAR <- "/home/vagrant/data/cache/covariance.rda"
DUMP_TRAIN_1 <- "/home/vagrant/data/cache/train1.rda"
DUMP_TRAIN_2 <- "/home/vagrant/data/cache/train2.rda"
DUMP_DATA <- "/home/vagrant/data/cache/digested.rda"
DUMP_NAMES <- "/home/vagrant/data/cache/names.rda"
DUMP_MEANS <- "/home/vagrant/data/cache/means.rda"
DUMP_COUNTS <- "/home/vagrant/data/cache/counts.rda"
DUMP_CONFIDENCE <- "/home/vagrant/data/cache/confidence.rda"
DUMP_SUMS <- "/home/vagrant/data/cache/sums.rda"
DUMP_SUMSQUARE <- "/home/vagrant/data/cache/sumsquared.R"

# use elastic as final output of our alerts
ELASTIC <- "http://elasticsearch:9200"
ELASTIC_IDX <- "crystalball"
