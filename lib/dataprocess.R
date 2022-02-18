
skate_parks <- read.csv(url("https://data.cityofnewyork.us/resource/pvvr-75zk.csv"), 
                        as.is = TRUE)
dog_runs <- read.csv(url("https://data.cityofnewyork.us/resource/wswf-9pts.csv"), 
                     as.is = TRUE)
playgrounds <- read.csv(url("https://data.cityofnewyork.us/resource/a4qt-mpr5.csv"), 
                        as.is = TRUE)
adult_exe <- read.csv(url("https://data.cityofnewyork.us/resource/tkzt-zfpz.csv"), 
                      as.is = TRUE)
ath_faci <- read.csv(url("https://data.cityofnewyork.us/resource/g3xg-qtbc.csv"), 
                     as.is = TRUE)
comfort <- read.csv(url("https://data.cityofnewyork.us/resource/i5n2-q8ck.csv"), 
                    as.is = TRUE)
event <- read.csv("../data/NYC_Permitted_Event_Information.csv",as.is = TRUE)



# Extract Longitude & Latitude
latlong <- function(x){
  x$point <- gsub("POINT |[()]", "", x$point)
  x <- x %>%
    separate(point, c("longitude", "latitude"), " ") %>%
    mutate(longitude = as.numeric(longitude)) %>%
    mutate(latitude = as.numeric(latitude))
  return(x)
}
latlong2 <- function(x){
  x <- x %>%
    mutate(longitude = as.numeric(str_extract(x$polygon, "-[0-9][0-9].[0-9]+"))) %>%
    mutate(latitude = as.numeric(substring(str_extract(x$polygon, " [0-9][0-9].[0-9]+"),2)))
  return(x)
}
playgrounds <- latlong(playgrounds)
adult_exe <- latlong(adult_exe)


ath_faci <- latlong2(ath_faci)
dog_runs <- latlong2(dog_runs)
comfort <- latlong2(comfort)
skate_parks <- latlong2(skate_parks)

# Covid Peak Status Check
covid_peak <- as.Date("2020-05-15")
peakstatus <- function(x){
  x <- x %>%
    mutate(approx_date_closed = as.Date(approx_date_closed)) %>%
    mutate(approx_date_reopened = as.Date(approx_date_reopened)) %>%
    mutate(
      peak_status = 
        ifelse(covid_peak > approx_date_closed & covid_peak < approx_date_reopened, 
               "COVID-19 Closure", "Active")) %>%
    mutate(peak_status = ifelse(is.na(peak_status), status, peak_status))
    return(x)
}
dog_runs <- peakstatus(dog_runs)
skate_parks <- peakstatus(skate_parks)
playgrounds <- peakstatus(playgrounds)
comfort <- peakstatus(comfort)
ath_faci <- peakstatus(ath_faci)
adult_exe <- adult_exe %>%
  mutate(approx_date_closed = as.Date(approx_date_closed,format = '%Y-%m-%d'))%>%
  mutate(approx_date_reopened = as.Date(approx_date_reopened, format = '%Y-%m-%d')) %>%
  mutate(
    peak_status = 
      ifelse(covid_peak > approx_date_closed & covid_peak < approx_date_reopened, 
             "COVID-19 Closure", "Active")) %>%
  mutate(peak_status = ifelse(is.na(peak_status), status, peak_status))


# Add Popup Content
playgrounds <- playgrounds %>%
  mutate(popup = paste(sep = "<br/>",
                        paste0(location),
                        paste0("<b>Accessibility: </b>", playgrounds$accessibilityLevel)
                        ))

ath_faci <- ath_faci %>%
  mutate(popup = paste(sep = "<br/>",
                       paste0(propname),
                       paste0("<b>Sports: </b>", ath_faci$primarysport)
                       ))

dog_runs <- dog_runs %>%
  mutate(popup = paste(sep = "<br/>",
                       paste0(propertyname),
                       paste0("<b>Red Sign: </b>", 
                              ifelse(dog_runs$red_sign_installed == "true", 
                                     "Installed", "Not Installed"))
                       ))

adult_exe <- adult_exe %>%
  mutate(popup = paste(sep = "<br/>",
                       paste0(sitename),
                       paste0("<b>Feature Type: </b>",
                              adult_exe$featuretype)
                       ))

skate_parks <- skate_parks %>%
  mutate(popup = paste(sep = "<br/>",
                       paste0(propname),
                       paste0("<b>Red Sign: </b>",
                              ifelse(skate_parks$red_sign_installed == "true",
                                     "Installed", "Not Installed"))
                       ))



# Event Data Process
event <- event %>%
  mutate(propname = substring(event$Event.Location,1,regexpr(":",event$Event.Location) - 1))
ath_faci <- ath_faci %>%
  rename(propname = propertyname)
playgrounds <- playgrounds %>%
  rename(propname = name)
skate_parks <- skate_parks %>%
  rename(propname = name)
all <- rbind(adult_exe[,c("propname", "longitude", "latitude")],
      ath_faci[,c("propname", "longitude", "latitude")],
      skate_parks[,c("propname", "longitude", "latitude")],
      playgrounds[,c("propname", "longitude", "latitude")])
event <- merge(event, all, by = "propname", all.x = TRUE)


