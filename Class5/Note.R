# Where are we getting the data from
met_url <- "https://github.com/USCbiostats/data-science-data/raw/master/02_met/met_all.gz"

# Downloading the data to a tempfile (so it is destroyed afterwards)
# you can replace this with, for example, your own data:
tmp <- tempfile(pattern = "met", fileext = ".gz")
# tmp <- "met.gz"

# We should be downloading this, ONLY IF this was not downloaded already.
# otherwise is just a waste of time.
if (!file.exists(tmp)) {
  download.file(
    url      = met_url,
    destfile = tmp,
    # method   = "libcurl", timeout = 1000 (you may need this option)
  )
}
getwd()
# Reading the data
dat <- read.csv(tmp)
head(dat)

dat[, c('USAFID', 'lat', 'lon')]
dat |> select(USAFID, lat, lon)

# select only the relevant variables, DO NOT create new dataset with subset
dat <- dat |> 
  select(USAFID, WBAN, year, month, day, 
         hour, min, lat, lon, elev, 
         wind.sp, temp, atm.press)

dat[dat$day == 1 &
      dat$lat > 40 &
      ((dat$elev < 500) | (dat$elev > 1000)), ]

length(dat[dat$day == 1 &
             dat$lat > 40 &
             ((dat$elev < 500) | (dat$elev > 1000)), 1])

dat |>
  filter(day == 1, lat > 40, ((elev < 500) | (elev > 1000))) |>
  collect() |> # Notice this line!
  nrow() 

# Variable Creation
# Listing the names
names  <- c("wind.sp", "temp", "atm.press")

for(var in names){
  dat[,paste0(var,'_scaled')] <- dat[,var] / sd(dat[,var], na.rm = TRUE)
}

# create the new variable one entry at a time
start_time <- Sys.time()
dat$wind.temp <- sapply(1:nrow(dat), function(i){
  if(is.na(dat$temp[i]) | is.na(dat$wind.sp[i])){
    return(NA)
  }
  if(dat$temp[i] <= median(dat$temp, na.rm=TRUE)){
    if(dat$wind.sp[i] <= median(dat$wind.sp, na.rm=TRUE)){
      return('cool & still')
    }else{
      return('cool & windy')
    }
  }else{
    if(dat$wind.sp[i] <= median(dat$wind.sp, na.rm=TRUE)){
      return('warm & still')
    }else{
      return('warm & windy')
    }
  }
})

end_time <- Sys.time()
time_taken <- end_time - start_time
print(time_taken)


stations <- fread("https://noaa-isd-pds.s3.amazonaws.com/isd-history.csv")
stations <- as.data.frame(stations)
stations$USAF <- as.integer(stations$USAF)

# Dealing with NAs and 999999
stations$USAF[stations$USAF == 999999] <- NA
stations$CTRY[stations$CTRY == ""] <- NA
stations$STATE[stations$STATE == ""] <- NA

# Selecting the three relevant columns, and keep unique records
stations <- unique(stations[, c('USAF', 'CTRY', 'STATE')])

# Dropping NAs
stations <- stations[!is.na(stations$USAF), ]

head(stations, n = 4)

merge(
  # Data
  x     = dat,      
  y     = stations, 
  # List of variables to match
  by.x  = "USAFID",
  by.y  = "USAF", 
  # Which obs to keep?
  all.x = TRUE,      
  all.y = FALSE
) |> nrow()

stations <- stations[!duplicated(stations$USAF), ]

#order: non-NA first -> NA

