glimpse(DT)
dt <- DT[1:100,]
dt
dt[50,]
dt_tmp <- dt[100,3]
dt_tmp <- as.numeric(dt_tmp)
dt_tmp %/% 60
dt_tmp / 60
require(lubridate)
num <-815
den <- 60
min <- round(((num / 60 - floor(num / 60)) * 60), 0)
hour <- floor( num / 60)
if(hour < 10){
    hour <- paste0("0", hour)
}

if(min < 10){
    min <- paste0("0", min)
}

tim <- paste(hour, ":", min, sep = "")
tim
DT1 <- DT %>% 
    mutate(hour = floor(interval / 100)) %>% 
    mutate(min = ifelse(hour < 1, interval, interval - 100 * hour)) %>%
    mutate(hour1 = ifelse(hour < 10, paste0("0", hour, sep = ""), hour)) %>% 
    mutate(min1 = ifelse(min < 10, paste0("0", min, sep = ""), min)) %>% 
    mutate(time = paste(hour1, ":", min1, sep = "")) %>% 
    select(steps, date, time)
dt1
    
dt_summ <- 
    DT_comp %>% 
    group_by(time) %>% 
    summarise(mean = mean(steps))
ttime <- "08:55"
dt_avg <- dt_summ %>% 
    filter(time == "08:55")
dt_avg$mean
dt_avg[ttime,]
    
ttime
dt_summ <- as.list(dt_summ)
dt_summ[(time == "08:55"),]
dt_summ
y <- which(is.na(DT1)==TRUE) 
DT2 <- DT1
DT2$steps[y] <- round(dt_summ$mean[match(dt_summ$time, DT2$time)],0)
mean(DT2$steps)
mean(DT_comp$steps)
