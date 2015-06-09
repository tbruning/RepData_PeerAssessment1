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
y_len <- prettyNlength(y)
ttime
dt_summ <- as.list(dt_summ)
dt_summ[(time == "08:55"),]
dt_summ
y <- which(is.na(DT1)==TRUE) 
DT2 <- DT1
DT2$steps[y] <- round(dt_summ$mean[match(dt_summ$time, DT2$time)],0)
mean(DT2$steps)
mean(DT_comp$steps)
z <- summarise(dt_summ, max = max(mean))
filter(dt_summ, mean == z$max)
z
z
DT_comp %>% 
    group_by(time)
dt_summ_int <- 
    DT_comp %>% 
    group_by(time) %>% 
    summarise(mean = round((mean(steps)), 0))
plot(dt_summ_int$time, dt_summ_mean, type = "l", col = "red")
plot(dt_summ_int$mean, type = "l", col = "red")
dt_summ_int %>% 
    ggvis(~time, ~mean) %>% 
    layer_lines(stroke := "red")
# Use title offset to push the titles further away
data("mtcars")
ggvis(mtcars, props(x = ~wt, y = ~mpg)) +
    layer_point() +
    guide_axis("x", title = "Weight", title_offset = 50) +
    guide_axis("y", title = "Miles per gallon", title_offset = 50)

dt3 <- dt3 %>% 
    mutate(wdind = ifelse((wd == 7 | wd == 1), 0, 1))
sp <- ggplot(dt3, aes(x=date, y=)) + geom_point(shape=1)
dt3 <- dt3 %>% 
    mutate(wd = wday(date))
dt3 <- dt3 %>% 
mutate(wdind = ifelse((wd == 7 | wd == 1), "weekend", "weekday"))


dt3 <- DT2
glimpse(dt3)
glimpse(DT2)
require(lubridate)
dt3$date <- ymd(dt3$date)
glimpse(dt3)
dt3 <- dt3 %>%
    dt3 <- dt3 %>%
    mutate(wd = wday(date))
dt3_summ <- dt3 %>% 
    group_by(wdind, time) %>% 
    summarise(mean = round(mean(steps),0))
dt3_summ <- group_by(dt3_summ, wdind)
sp <- ggplot(dt3_summ, aes(x=time, y=mean, group=wdind)) + geom_line(col = "red")
sp
sp <- sp + facet_wrap( ~ wdind, ncol=1)
sp

glimpse(dt3)
summary(dt3)
glimpse(dt3_summ)
