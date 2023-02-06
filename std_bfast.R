library(tidyverse)
library(bfast)
library(lubridate)
library(purrr)
library(furrr)



path <- "C:/Users/Fellipe/Desktop/Projetos/IC/IC/data/"
setwd(path)


########################### FUNCTION ###########################################

time.series.to.dataframe <- function(time_series, source) {
  s.df           <- data.frame(as.numeric(time_series))
  colnames(s.df) <- "NDVI"
  s.df$Time      <- as.Date(date_decimal(as.numeric(time(time_series))))
  s.df$Type      <- source
  return(s.df)
}

################################################################################

files_median <- list.files(path = path,pattern = '^df_median*')

files_std <- list.files(path = path,pattern = '^df_std*')

df_median <- lapply(files_median,function(x){return(read_csv(x))})

df_std <- lapply(files_std,function(x){return(read_csv(x))})

ts_median <- lapply(df_median, function(x){bfastts(data = x$NDVI,
                                                   dates = x$Timestamp,
                                                   type = c("irregular"))})

df_median[[1]]$Timestamp %>% summary 

s.d.periodic <- lapply(ts_median, function(x){round(forecast::na.interp(x),4)})

fit <- list()
for (i in seq(1,9)) {
  fit[[i]] <- bfast(Yt = s.d.periodic[[i]], h = 50/length(s.d.periodic[[i]]), season = "harmonic", breaks = 4, max.iter = 2) 
  print(paste0('iteração:',i))
  write.csv(fit[[i]], file = paste0('fit_',i,".csv"))
  gc()
}

St<- map(fit,function(x){x[["output"]][[1]][["St"]]})
St[[1]] %>% plot()

St_df <- list() 
for (x in 1:9) {
  St_df[[x]] <- time.series.to.dataframe(St[[x]],'none')
  }  


for(i in 1:9){
  write.csv(St_df[[i]], file = paste0('St_df',i,".csv"))
}

files.st <-list.files(pattern = "^St_df")
St_df <- map(files.st,read_csv)

df_merged <- map2(St_df,
                  df_std,
                  function(x,y){return(left_join(x,y,
                                                 by=c('Time'='Timestamp'),
                                                 suffix = c(".bfast", ".std")))})

for(i in 1:9){
  write.csv(df_merged[[i]], file = paste0('merged_df',i,".csv"))
}

St_df[[9]] %>% glimpse
df_std[[9]] %>%  glimpse
df_merged[[1]] %>% glimpse()


df_merged[[1]] %>% 
  mutate(ndvi = round(forecast::na.interp(NDVI.std),4),
         inf_lim = if_else(is.na(NDVI.std),0,(NDVI.bfast - sd(NDVI.std,na.rm = T))),
         sup_lim = if_else(is.na(NDVI.std),0,(NDVI.bfast + sd(NDVI.std,na.rm = T)))) %>% 
  filter(lubridate::year(Time) %in% c(2014:2020)) %>% 
  ggplot(., aes(x=Time))+
  geom_line(aes(y=NDVI.bfast),col='black', size =0.3)+
  geom_point(aes(y=sup_lim),col='blue3', size=0.3)+
  geom_point(aes(y=inf_lim),col='blue3', size=0.3)+
  theme_linedraw()



ts <- bfastts(data = df$ndvi,dates = df$Timestamp,type = c("irregular"))

head(ts)

#s.d.linear <- round(na.approx(ts), 4) 
s.d.periodic <- round(forecast::na.interp(ts),4)

fit <- bfast(Yt = s.d.periodic, h = 50/length(s.d.periodic),
      season = "harmonic", breaks = 5, max.iter = 3)

plot(fit)

 
st <- fit[["output"]][[1]][["St"]]

st.df <- time.series.to.dataframe(st,'none')
st.df$Time

df_std <- read_csv('~/Desktop/Python/IC/scripts/IC/.ipynb_checkpoints/df_median0.csv') %>% 
  mutate(Time = as_date(Timestamp)) %>% 
  select(Time, ndvi)

df_merged <- left_join(st.df,df_std,by='Time')

names(df_merged) <- c("NDVI", "Time", "Type", "ndvi_std")

df_merged[[1]] %>% 
  mutate(ndvi = round(forecast::na.interp(n),4),
         inf_lim = if_else(is.na(ndvi),0,(NDVI - sd(NDVI.std,na.rm = T))),
         sup_lim = if_else(is.na(ndvi),0,(NDVI + sd(NDVI.std,na.rm = T)))) %>% 
  filter(lubridate::year(Time) %in% c(2015:2020)) %>% 
  ggplot(., aes(x=Time))+
  geom_line(aes(y=NDVI.bfast),col='black', size =0.3)+
  geom_point(aes(y=sup_lim),col='blue3', size=0.3)+
  geom_point(aes(y=inf_lim),col='blue3', size=0.3)+
  theme_linedraw()

########################  Rasters ##############################################


