# Load some great Hadley libraries
library(readxl)
library(dplyr)
library(lubridate)
library(tidyr)
library(haven)

# Import file
diab <- read_excel("Diabetes_example.xlsx", sheet = 11)

# Change variable names to lower case
names(diab) <- tolower(names(diab))

# Check each variable type
sapply(diab, class)

# Fix issue with some times losing one second (don't know why this happens)
diab$date_time <- round_date(diab$date_time, "minute")

# Sort data by ID and then time
diab <- diab %>% 
    arrange(patientid1, date_time)



# Episode Index for Hypo ---------------

# Initialise empty vector to store results
ep <- integer(nrow(diab))

for (i in unique(diab$patientid1)) {
    first_low <- min(which(diab$patientid1 == i & diab$valueresult < 4))
    for (j in which(diab$patientid1 == i)) {
        if (j == match(i,diab$patientid1)) {
            ep[j] <- 1L
            if (j == first_low) {
                time_ref <- diab$date_time[j]}
        } 
        else {
            if (j == first_low) {
                time_ref <- diab$date_time[j]
                ep[j] <- ep[j - 1] + 1L
            }
            else if (diab$valueresult[j] < 4) {
                time_int <- suppressMessages(as.period(diab$date_time[j] - time_ref))
                if (time_int@hour < 1) 
                {ep[j] <- ep[j - 1]} 
                else {
                    ep[j] <- ep[j - 1] + 1
                    time_ref <- diab$date_time[j]}
            }
            else{ep[j] <- ep[j - 1]}
        }
    }
}

# View the results
ep

# Add to the dataframe
diab$ep <- ep



# Hypo_episode ---------------
z <- diab %>% 
    group_by(patientid1, ep) %>% 
    summarise(min_res = min(valueresult)) %>% 
    ungroup() %>% 
    mutate(hypo = ifelse(min_res < 4, 1L, 0L)) %>% 
    select(-min_res)

diab <- diab %>% 
    left_join(z)

z_sel <- diab %>% select(patientid1, ep, hypo) %>% duplicated()
diab$hypo[z_sel] <- 0L

# View the results
diab$hypo



# Hypo_correctly_treated ---------------

# Select out hypo episodes for each person
z1 <- diab %>% 
    filter(hypo == 1) %>% 
    select(patientid1, ep) %>% 
    inner_join(diab)

# Find first time when result was less than 4
z2 <- z1 %>% 
    filter(valueresult < 4) %>% 
    group_by(patientid1, ep) %>% 
    summarise(first_time_bad = min(date_time))

# Find first time when result was greater than 4
z3 <- z1 %>% 
    filter(valueresult >= 4) %>% 
    group_by(patientid1, ep) %>% 
    summarise(first_time_good = min(date_time))

# Join times together
# Get time interval and flag which ones were treated within an hour
# Turn into lookup table by patient and hypo episode

z4 <- z1 %>% 
    left_join(z2) %>% 
    left_join(z3) %>%
    mutate(time_int = suppressMessages(as.period(first_time_good - first_time_bad)))

z4$hypo_c <- ifelse(z4$time_int@hour < 1, 1L, 0L)
z4 <- z4 %>% 
    select(patientid1, ep, hypo_c) %>% distinct()

# Add to main dataset
diab <- diab %>% 
    left_join(z4)

# Tidy up
diab$hypo_c[is.na(diab$hypo_c)] <- 0L
z_sel <- diab %>% select(patientid1, ep, hypo_c) %>% duplicated()
diab$hypo_c[z_sel] <- 0L

#Change date_time to string.???
#diab$date_time2<-toString(diab$date_time)



# Episode Index for Hyper ---------------

# Initialise empty vector to store results
ep_hyper <- integer(nrow(diab))

for (i in unique(diab$patientid1)) {
    first_high <- min(which(diab$patientid1 == i & diab$valueresult > 15))
    for (j in which(diab$patientid1 == i)) {
        if (j == match(i,diab$patientid1)) {
            ep_hyper[j] <- 1L
            if (j == first_high) {
                time_ref <- diab$date_time[j]}
        } 
        else {
            if (j == first_high) {
                time_ref <- diab$date_time[j]
                ep_hyper[j] <- ep_hyper[j - 1] + 1L
            }
            else if (diab$valueresult[j] > 15) {
                time_int <- suppressMessages(as.period(diab$date_time[j] - time_ref))
                if (time_int@hour < 1) 
                {ep_hyper[j] <- ep_hyper[j - 1]} 
                else {
                    ep_hyper[j] <- ep_hyper[j - 1] + 1
                    time_ref <- diab$date_time[j]}
            }
            else{ep_hyper[j] <- ep_hyper[j - 1]}
        }
    }
}

# View the results
ep_hyper

# Add to the dataframe
diab$ep_hyper <- ep_hyper



# Hyper_episode ---------------

z <- diab %>% 
    group_by(patientid1, ep_hyper) %>% 
    summarise(max_res = max(valueresult)) %>% 
    ungroup() %>% 
    mutate(hyper = ifelse(max_res > 15, 1L, 0L)) %>% 
    select(-max_res)

diab <- diab %>% 
    left_join(z)

z_sel <- diab %>% select(patientid1, ep_hyper, hyper) %>% duplicated()
diab$hyper[z_sel] <- 0L

# View the results
diab$hyper



# Low Therapeutic Inertia Episode ---------------

# Import file
diab <- read_excel("Diabetes_example_Therapeutic inertia2.xlsx", sheet = "Sheet4")

# Change variable names to lower case
names(diab) <- tolower(names(diab))

# Check each variable type
sapply(diab, class)

# Fix issue with some times losing one second (don't know why this happens)
diab$date_time <- round_date(diab$date_time, "minute")

# Sort data by ID and then time
diab <- diab %>% 
    arrange(patientid1, date_time)

# Categorise into time periods
diab <- diab %>% 
    mutate(date = as.Date(substr(date_time, 1, 10)),
           hour = hour(date_time),
           min = minute(date_time))

diab$time_period <- "other"
z_sel <- diab$hour >= 6 & diab$hour <= 9
diab$time_period[z_sel] <- "pre-breakfast"

z_sel <- (diab$hour == 11 & diab$min >= 30) |
    (diab$hour >= 12 & diab$hour <= 14)
diab$time_period[z_sel] <- "pre-lunch"

z_sel <- diab$hour >= 16 & diab$hour <= 18
diab$time_period[z_sel] <- "pre-evening"

z_sel <- diab$hour >= 20 & diab$hour <= 23
diab$time_period[z_sel] <- "pre-bed"

diab <- diab %>% 
    arrange(patientid1, time_period)

diab$consec_days <- NA_integer_
diab$consec_ep <- NA_integer_

for (i in 1:nrow(diab)) {
    if (diab$time_period[i] == "other") {
        next
    } else if (diab$time_period[i] != diab$time_period[i - 1]) {
        diab$consec_days[i] <- 1
        diab$consec_ep[i] <- 1
    } else {
        z_timediff <- diab$date[i] - diab$date[i - 1]
        if (z_timediff == 0) {
            diab$consec_days[i] <- diab$consec_days[i - 1]
            diab$consec_ep[i] <- diab$consec_ep[i - 1]
        } else if (z_timediff == 1) {
            diab$consec_days[i] <- diab$consec_days[i - 1] + 1
            diab$consec_ep[i] <- diab$consec_ep[i - 1]
        } else {
            diab$consec_days[i] <- 1
            diab$consec_ep[i] <- diab$consec_ep[i - 1] + 1
        }
    }
}

z_eps_needed <- diab %>% 
    filter(time_period != "other") %>% 
    group_by(patientid1, time_period, consec_ep) %>% 
    summarise(max_consec_days = max(consec_days)) %>% 
    filter(max_consec_days >= 3)

z_iner <- diab %>% 
    inner_join(z_eps_needed) %>% 
    group_by(patientid1, time_period, consec_ep) %>% 
    summarise(min_result = min(valueresult),
              max_result = max(valueresult)) %>% 
    ungroup() %>% 
    mutate(low_iner = ifelse(max_result < 4, 1L, 0L),
           high_iner = ifelse(min_result > 3, 1L, 0L))

diab <- diab %>% 
    left_join(z_iner)



# 12 HOUR ---------------

diab <- diab %>% 
    arrange(patientid1, date_time)

ep_iner_low <- integer(nrow(diab))

for (i in unique(diab$patientid1)) {
    first_low <- min(which(diab$patientid1 == i & diab$valueresult < 4))
    for (j in which(diab$patientid1 == i)) {
        if (j == match(i, diab$patientid1)) {
            ep_iner_low[j] <- 1L
        } 
        else {
            if (j == first_low) {
                ep_iner_low[j] <- ep_iner_low[j - 1] + 1L
            }
            else if (diab$valueresult[j - 1] > 4 & diab$valueresult[j] < 4) {
                ep_iner_low[j] <- ep_iner_low[j - 1] + 1}
            else{ep_iner_low[j] <- ep_iner_low[j - 1]}
        }
    }
}

# View the results
ep_iner_low

# Add to the dataframe
diab$ep_iner_low <- ep_iner_low

z <- diab %>% 
    group_by(patientid1, ep_iner_low) %>% 
    summarise(min_res = min(valueresult)) %>% 
    ungroup() %>% 
    mutate(hypo2 = ifelse(min_res < 4, 1L, 0L)) %>% 
    select(-min_res)

diab <- diab %>% 
    left_join(z)

z_sel <- diab %>% select(patientid1, ep_iner_low, hypo2) %>% duplicated()
diab$hypo2[z_sel] <- 0L

# View the results
diab$hypo2

# Select out hypo episodes for each person
z1 <- diab %>% 
    filter(hypo2 == 1) %>% 
    select(patientid1, ep_iner_low) %>% 
    inner_join(diab)

# Find first and last time when result was less than 4
z2 <- z1 %>% 
    filter(valueresult < 4) %>% 
    group_by(patientid1, ep_iner_low) %>% 
    summarise(first_time_bad = min(date_time),
              last_time_bad = max(date_time))

# Join times together
# Get time interval and flag which ones were treated within an hour
# Turn into lookup table by patient and hypo episode

z4 <- z1 %>% 
    left_join(z2) %>% 
    mutate(time_int = suppressMessages(as.period(last_time_bad - first_time_bad)))

z4$hypo_iner <- ifelse(z4$time_int@hour >= 12 | z4$time_int@day >= 1, 1L, 0L)
z4 <- z4 %>% 
    select(patientid1, ep_iner_low, hypo_iner) %>% distinct()

# Add to main dataset
diab <- diab %>% 
    left_join(z4)

# Tidy up
diab$hypo_iner[is.na(diab$hypo_iner)] <- 0L
z_sel <- diab %>% select(patientid1, ep_iner_low, hypo_iner) %>% duplicated()
diab$hypo_iner[z_sel] <- 0L


# Tina test. 
write_sav(diab,"Diab1.sav")