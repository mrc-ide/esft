# this is my testing script, only for dev use

# original is best
data<-data.frame(und=c(2.5, 4, 3, 5), turns = c(10,32,42,63))
fit<-lm(und~turns, data)

# bare 2 - no double trycatch, no timout elongation
# bare - no double trycatch
# og - trycatch, but start with download.file
library(microbenchmark)
results_large <- microbenchmark(
  original = load_imperial_data(),
  no_trycatch = load_imperial_data_bare(),
  no_trycatch_no_timeout = load_imperial_data_bare2(),
  times=3
)

autoplot(object = results_large) +
  #scale_y_log10() +
  labs(y = "Time [milliseconds], logged")

# another issue is timing out: need to update timeout speed -
# DOWNLOAD FILE IS THE WINNER
# need to run final standardized tests

# try download.file then read.csv
t0 = Sys.time()
data_combined4<-load_imperial_data()
t1 = Sys.time()
summary(as.factor(data_combined4$country))
print(paste0("Timeout length still there. Time for download.file first, then read.csv is ", t1-t0))
# fuck download file might be the most reliable

# am not sure the place i put the timeout length worked, or wther i needed to also add the timeout
# length in the function call

# result: best with read.csv first THEN vroom
# test updating timeout speed BEFORE function call
t0 = Sys.time()
data_combined3<-load_imperial_data()
t1 = Sys.time()
summary(as.factor(data_combined3$country))
print(paste0("Update timeout length. \n Time for combined vroom first, then read.csv is ", t1-t0))

# testing if vroom first, then read.csv
t0 = Sys.time()
data_combined2<-load_imperial_data()
t1 = Sys.time()
summary(as.factor(data_combined2$country))
print(paste0("Time for combined vroom first, then read.csv is ", t1-t0))
# 2.08 minutes, but only afghanistan and argentina

# testing read.csv first, then vroom as trycatch
t0 = Sys.time()
data_combined<-load_imperial_data()
t1 = Sys.time()
summary(as.factor(data_combined$country))
print(paste0("Time for combined read.csv first, then vroom is ", t1-t0)) # 2.5
# got afg, ang, argentina, armenia

t0 = Sys.time()
data_readcsv<-load_imperial_data()
t1 = Sys.time()
data_vroom<-load_imperial_data2()
t2 = Sys.time()
data_readr<-load_imperial_data3()
t3 = Sys.time()

print(paste0("Time for read.csv is ", t1-t0))
print(paste0("Time for vroom is ", t2-t1))
print(paste0("Time for read_csv is ", t3-t2))

summary(as.factor(data_readcsv$country)) # albania, angola, armenia
summary(as.factor(data_vroom$country)) # afghanistan, albania, argentina, armenia

# this never seems to connect if the above two haven't
summary(as.factor(data_readr$country)) # afghanistan, armenia


