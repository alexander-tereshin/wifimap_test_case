# Test task for WifiMap
Initially, it is supposed to be performed with Python, but I used R in this project in order to practice R.

### **Problem description**

Данные:
1) users.csv - пользователи, id - уникальный идентификатор пользователя
2) hotspots.csv - точки Wifi, id - уникальный идентификатор точки
3) conns.csv - логи о каждом подключении к точке Wifi, id - уникальный идентификатор одного подключения

! В данных присутствуют поля, которые могут не использоваться в задании, это нормально!

Задание:

Написать скрипт для агрегации данных для пользователя:
- посчитать сколько wifi точек (мы wifi записи еще называем hotpots) создал пользователь. (owner_id - идентификатор(связь) пользователя id в таблице hotspots) 
- посчитать сколько hotpots у пользователя с привязкой к месту (если foursquare_id or google_place_id не NULL - место указано)
- посчитать сколько hotspots пользователь создал за все время, за последний месяц, неделю
- посчитать сколько у пользователя хороших hotspots (score_v4 > 0.6), средних hotspots (0.3 < score_v4 < 0.6), плохих (score_v4  < 0.3)
- посчитать сколько у пользователя hotspots к которым было больше 1, 5 и 10 уникальных(!) подключений за все время, за последний год, за последний месяц, за последнюю неделю. (Уникальными считать подключения по полю installation_id)

* Сделать визуализацию данных (любую информативную на свой вкус)

**Analysis has been performed and visualisition is [here](https://github.com/alexander-tereshin/wifimap_test_case/blob/master/Rplot.png)**
