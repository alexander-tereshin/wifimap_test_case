library(tidyverse) 
library(DT)
library(scales)
library(plyr)
library(lubridate)
library(ggplot2)
library(reshape2)


user <- read.csv("wifimap_test_case/data/users_test.csv",
                    header = TRUE,
                    sep = ",")

hotspots <- read.csv("wifimap_test_case/data/hotspots_test.csv",
                        header = TRUE,
                        sep = ",",
                        quote = "\"",
                        colClasses=rep("character" ,13),
                        fill = TRUE)

# Меняем пустые ячейки на NA:
hotspots[hotspots == ""] <- NA

conns <- read.csv("wifimap_test_case/data/conns_test.csv",
                     header = TRUE,
                     sep = ",",
                     quote ="\"",
                     fill = TRUE)

# owner_id агрегрированы по id точкам доступа:
hotspots_created_in_total <- ddply(hotspots, 
                                   .(owner_id), 
                                   summarise, 
                                   "hotspots_created_in_total" = length(id))

# format the dates into a more readable format using the Date Time conversion function:
hotspots$created_at <- ymd_hms(hotspots$created_at)
hotspots$updated_at <- ymd_hms(hotspots$updated_at)                 
hotspots$deleted_at <- ymd_hms(hotspots$deleted_at)          
conns$connected_at <- ymd_hms(conns$connected_at)

# добавляем логический фильтр за последний месяц от системной даты:
hotspots$logical_last_month <- hotspots$created_at >= Sys.Date() %m+% months(-1)
# добавляем логический фильтр за последнюю неделю от системной даты:
hotspots$logical_last_week <- hotspots$created_at >= Sys.Date() %m+% weeks(-1)

# агрегируем owner_id по hotspots_id созданным за последний месяц:
hotspots_created_last_month <- ddply(hotspots, .(owner_id), summarise,
                                     "hotsposts_created_last_month" = length(which(logical_last_month)))

# агрегируем owner_id по hotspots_id созданным за последнюю неделю:
hotsposts_created_last_week <- ddply(hotspots, .(owner_id), summarise,
                                     "hotsposts_created_last_week" = length(which(logical_last_week)))


# создаем логический вектор для фильтрации точки доступа с привязкой к месту:
hotspots$location_filter <- !is.na(hotspots$foursquare_id) | !is.na(hotspots$google_place_id)

# агрегируем owner_id по hotpots с локациями по (если foursquare_id or google_place_id не NULL - место указано):
hotsposts_with_location <- ddply (hotspots, .(owner_id), summarize, 
                                  "hotspots_with_location" = length(which(location_filter)))

# создаем логические вектора для фильтрации точки доступа по типу соединения:
hotspots$good_hotspots <- hotspots$score_v4 >= 0.6
hotspots$moderate_hotspots <- hotspots$score_v4 < 0.6 & 0.3 < hotspots$score_v4
hotspots$poor_hotspots <- hotspots$score_v4 <= 0.3

length(which(hotspots$good_hotspots))+length(which(hotspots$moderate_hotspots))+length(which(hotspots$poor_hotspots))

# агрегируем owner_id по hotpots с разным качеством соединения:
hotsposts_good <- ddply (hotspots, .(owner_id), summarize, 
                         "hotspots_with_good_connection" = length(which(good_hotspots)))

hotsposts_moderate <- ddply (hotspots, .(owner_id), summarize, 
                             "hotspots_with_moderate_connection" = length(which(moderate_hotspots)))

hotsposts_poor <- ddply (hotspots, .(owner_id), summarize, 
                         "hotspots_with_poor_connection" = length(which(poor_hotspots)))

#добавляем логический фильтр за последний год от системной даты:
conns$logical_last_year <- conns$connected_at >= Sys.Date() %m+% years(-1)
#добавляем логический фильтр за последний месяц от системной даты:
conns$logical_last_month <- conns$connected_at >= Sys.Date() %m+% months(-1)
# добавляем логический фильтр за последнюю неделю от системной даты:
conns$logical_last_week <- conns$connected_at >= Sys.Date() %m+% weeks(-1)

# считаем частоту значений переменной installation_id за все время:
conns_freq <- ddply (conns, .(hotspot_id), summarize, 
                     "unique_conns_total" = length(unique(installation_id)))

# считаем частоту уникальных значений переменной installation_id за последний год:
unique_conns_last_year <- ddply (conns, .(hotspot_id), summarize, 
                                 "unique_conns_year" = length(unique(installation_id[logical_last_year])))

# считаем частоту уникальных значений переменной installation_id за последний месяц:
unique_conns_last_month <- ddply (conns, .(hotspot_id), summarize, 
                                  "unique_conns_month" = length(unique(installation_id[logical_last_month])))

# считаем частоту уникальных значений переменной installation_id за последнюю неделю:
unique_conns_last_week <- ddply(conns, .(hotspot_id), summarise, 
                                "unique_conns_week" = length(unique(installation_id[logical_last_week])))

# создаем dataframe с количеством уникальных значений переменной installation_id за последнюю неделю, месяц и год и total:
frequency_list <- list(unique_conns_last_year,
                       unique_conns_last_month,unique_conns_last_week,conns_freq)
frequency_list <- frequency_list %>% reduce(full_join,by= "hotspot_id")

#добавляем логические фильтры на количество подключений за все время:
frequency_list$more_than_1_total <- frequency_list$unique_conns_total>1
frequency_list$more_than_5_total <- frequency_list$unique_conns_total>5
frequency_list$more_than_10_total <- frequency_list$unique_conns_total>10

#добавляем логические фильтры на количество подключений за последний год:
frequency_list$more_than_1_year <- frequency_list$unique_conns_year>1
frequency_list$more_than_5_year <- frequency_list$unique_conns_year>5
frequency_list$more_than_10_year <- frequency_list$unique_conns_year>10

#добавляем логические фильтры на количество подключений за последний месяц:
frequency_list$more_than_1_month <- frequency_list$unique_conns_month>1
frequency_list$more_than_5_month <- frequency_list$unique_conns_month>5
frequency_list$more_than_10_month <- frequency_list$unique_conns_month>10

#добавляем логические фильтры на количество подключений за последнюю неделю:
frequency_list$more_than_1_week <- frequency_list$unique_conns_week>1
frequency_list$more_than_5_week <- frequency_list$unique_conns_week>5
frequency_list$more_than_10_week <- frequency_list$unique_conns_week>10

#делаем INNER JOIN, добавляя owner_id для каждой точки доступа:
frequency_list<-merge(frequency_list,hotspots[,c(1,9)],by.x = "hotspot_id", by.y = "id")


total_conns_more_than_1 <- ddply (frequency_list, .(owner_id), summarize, 
                                  "total_conns_more_than_1" = length(hotspot_id[more_than_1_total]))
total_conns_more_than_5 <- ddply (frequency_list, .(owner_id), summarize,
                                  "total_conns_more_than_5" = length(hotspot_id[more_than_5_total]))
total_conns_more_than_10 <- ddply (frequency_list, .(owner_id), summarize, 
                                   "total_conns_more_than_10" = length(hotspot_id[more_than_10_total]))

year_conns_more_than_1 <- ddply (frequency_list, .(owner_id), summarize, 
                                 "year_conns_more_than_1" = length(hotspot_id[more_than_1_year]))
year_conns_more_than_5 <- ddply (frequency_list, .(owner_id), summarize, 
                                 "year_conns_more_than_5" = length(hotspot_id[more_than_5_year]))
year_conns_more_than_10 <- ddply (frequency_list, .(owner_id), summarize, 
                                  "year_conns_more_than_10" = length(hotspot_id[more_than_10_year]))

month_conns_more_than_1 <- ddply (frequency_list, .(owner_id), summarize, 
                                  "month_conns_more_than_1" = length(hotspot_id[more_than_1_month]))
month_conns_more_than_5 <- ddply (frequency_list, .(owner_id), summarize, 
                                  "month_conns_more_than_5" = length(hotspot_id[more_than_5_month]))
month_conns_more_than_10 <- ddply (frequency_list, .(owner_id), summarize, 
                                   "month_conns_more_than_10" = length(hotspot_id[more_than_10_month]))

week_conns_more_than_1 <- ddply (frequency_list, .(owner_id), summarize, 
                                 "week_conns_more_than_1" = length(hotspot_id[more_than_1_week]))
week_conns_more_than_5 <- ddply (frequency_list, .(owner_id), summarize,
                                 "week_conns_more_than_5" = length(hotspot_id[more_than_5_week]))
week_conns_more_than_10 <- ddply (frequency_list, .(owner_id), summarize, 
                                  "week_conns_more_than_10" = length(hotspot_id[more_than_10_week]))

final_list <- list(hotspots_created_in_total,
                   hotspots_created_last_month,
                   hotsposts_created_last_week, 
                   hotsposts_with_location,
                   hotsposts_good,
                   hotsposts_moderate, 
                   hotsposts_poor,
                   total_conns_more_than_1,
                   total_conns_more_than_5,
                   total_conns_more_than_10,
                   year_conns_more_than_1,
                   year_conns_more_than_5,
                   year_conns_more_than_10,
                   month_conns_more_than_1,
                   month_conns_more_than_5,
                   month_conns_more_than_10,
                   week_conns_more_than_1,
                   week_conns_more_than_5,
                   week_conns_more_than_10)

final_df <-join_all(final_list, by='owner_id', type='inner')
View(final_df)

dfr <- melt(arrange(final_df,desc(hotspots_created_in_total)), id.vars = "owner_id")

dfr <- filter(dfr, variable=="hotspots_created_in_total" | variable =="hotspots_with_poor_connection" | variable=="hotspots_with_good_connection" | variable == "hotspots_with_moderate_connection")

ggplot (dfr %>% filter (variable =="hotspots_created_in_total"), 
        aes(reorder(owner_id, value), as.numeric(value), 
            fill = variable)) + 
  geom_bar(position = "stack", stat = "identity") +
  geom_bar(dfr%>% filter(grepl("hotspots_with", variable)), 
           mapping = aes(x=owner_id),
           position= "stack", stat = "identity") +
  geom_text(aes(label=value, family = "Comic Sans MS" ), position=position_dodge(width=0.9), 
            hjust=-0.25, size =3) + 
  labs(title = "Гистограмма",
        subtitle = "Точки связи и качество соединения",
        x=  "Owner id",
       y= "Количество, шт")+
  scale_fill_manual(name="Качество соединения", 
                    labels = c("Информации о соединении нет", "Хорошее соединение", "Среднее соединение","Плохое соединение"), 
                    values = c("hotspots_created_in_total"="#999999","hotspots_with_good_connection"="#00ba38", "hotspots_with_moderate_connection"="#FFFF33", "hotspots_with_poor_connection"="#FF5733")) + 
  theme_light() + 
  theme(axis.text.x = element_text(angle = 0, size = 8),
        axis.text.y = element_text(angle = 0, size = 8), text=element_text(family="Comic Sans MS")) + coord_flip()