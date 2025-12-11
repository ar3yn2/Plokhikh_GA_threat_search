# Исследование информации о состоянии беспроводных сетей
gleb.plokhikh@yandex.ru

## Цель работы

1.  Получить знания о методах исследования радиоэлектронной обстановки.
2.  Составить представление о механизмах работы Wi-Fi сетей на канальном
    и сетевом уровне модели OSI.
3.  Закрепить практические навыки использования языка программирования R
    для обработки данных
4.  Закрепить знания основных функций обработки данных экосистемы
    tidyverse языка R

## Исходные данные

1.  Программное обеспечение Windows 11 Pro
2.  Rstudio Desktop
3.  Интерпретатор языка R 4.5.1
4.  Программный пакет dplyr
5.  Журналы программных средств анализа беспроводных сетей – tcpdump и
    airodump-ng

## План

1.  Подготовка данных для анализа
2.  Анализ точек доступа
3.  Анализ данных клиентов
4.  Оформление отчета

## Шаги:

### Подготовка данных

1.  Импортирование данных

``` r
options(repos = c(CRAN = "https://mirror.truenetwork.ru/CRAN/"))
packages <- c("tidyverse","lubridate","stringr","curl")
missing <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(missing)) install.packages(missing)
library(tidyverse)
```

    Warning: пакет 'tidyverse' был собран под R версии 4.5.2

    Warning: пакет 'ggplot2' был собран под R версии 4.5.2

    Warning: пакет 'tidyr' был собран под R версии 4.5.2

    Warning: пакет 'purrr' был собран под R версии 4.5.2

    Warning: пакет 'forcats' был собран под R версии 4.5.2

    Warning: пакет 'lubridate' был собран под R версии 4.5.2

    ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ✔ forcats   1.0.1     ✔ stringr   1.5.2
    ✔ ggplot2   4.0.1     ✔ tibble    3.3.0
    ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ✔ purrr     1.2.0     
    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()
    ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(lubridate)
library(stringr)
library(curl)
```

    Using libcurl 8.14.1 with Schannel

    Присоединяю пакет: 'curl'

    Следующий объект скрыт от 'package:readr':

        parse_date

1.  Загрузка данных

``` r
dataset_url <- "https://storage.yandexcloud.net/dataset.ctfsec/P2_wifi_data.csv"
tmp_file <- tempfile(fileext=".csv")
curl::curl_download(dataset_url, tmp_file, quiet = TRUE)
raw_data <- read_csv(tmp_file, skip = 1, col_names = FALSE)
```

    Warning: One or more parsing issues, call `problems()` on your data frame for details,
    e.g.:
      dat <- vroom(...)
      problems(dat)

    Rows: 12250 Columns: 15
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ","
    chr (15): X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
divider <- which(raw_data$X1 == "Station MAC")
```

1.  Создание таблиц AP и клиентов

``` r
ap_raw <- raw_data[2:(divider[1]-1), ]
clients_raw <- raw_data[divider[1]:nrow(raw_data), ]  # исправлено
ap_raw <- ap_raw[rowSums(is.na(ap_raw) | ap_raw=="") < ncol(ap_raw), ]
clients_raw <- clients_raw[rowSums(is.na(clients_raw) | clients_raw=="") < ncol(clients_raw), ]
clients_raw <- clients_raw[, 1:7]
ap_cols <- c("bssid","first_seen","last_seen","chan","speed","privacy","cipher","auth","signal","beacon_count","iv","lan","id_len","essid","key_type")
client_cols <- c("mac","first_seen","last_seen","signal","pkt","assoc_bssid","probe")
names(ap_raw) <- ap_cols[1:ncol(ap_raw)]
names(clients_raw) <- client_cols[1:ncol(clients_raw)]
```

1.  Преобразование типов и очистка

``` r
wifi_ap <- ap_raw %>%
  filter(!is.na(bssid), bssid!="" , !is.na(essid), essid!="") %>%
  mutate(
    first_seen = as.POSIXct(first_seen, format="%Y-%m-%d %H:%M:%S"),
    last_seen = as.POSIXct(last_seen, format="%Y-%m-%d %H:%M:%S"),
    chan = as.numeric(chan),
    speed = as.numeric(speed),
    signal = as.numeric(signal),
    beacon_count = as.numeric(beacon_count),
    iv = as.numeric(iv),
    id_len = as.numeric(id_len)
  )

wifi_clients <- clients_raw[-1, ] %>%
  filter(!is.na(mac), mac!="" , mac!="Station MAC") %>%
  mutate(
    first_seen = as.POSIXct(first_seen, format="%Y-%m-%d %H:%M:%S"),
    last_seen = as.POSIXct(last_seen, format="%Y-%m-%d %H:%M:%S"),
    signal = as.numeric(signal),
    pkt = as.numeric(pkt)
  )
```

1.  Просмотр структуры данных

``` r
glimpse(wifi_ap)
```

    Rows: 96
    Columns: 15
    $ bssid        <chr> "BE:F1:71:D5:17:8B", "6E:C7:EC:16:DA:1A", "9A:75:A8:B9:04…
    $ first_seen   <dttm> 2023-07-28 09:13:03, 2023-07-28 09:13:03, 2023-07-28 09:…
    $ last_seen    <dttm> 2023-07-28 11:50:50, 2023-07-28 11:55:12, 2023-07-28 11:…
    $ chan         <dbl> 1, 1, 1, 7, 6, 11, 11, 11, 1, 6, 11, 11, 6, 6, 1, 1, 11, …
    $ speed        <dbl> 195, 130, 360, 360, 130, 195, 130, 130, 195, 180, 130, 13…
    $ privacy      <chr> "WPA2", "WPA2", "WPA2", "WPA2", "OPN", "WPA2", "WPA2", "W…
    $ cipher       <chr> "CCMP", "CCMP", "CCMP", "CCMP", NA, "CCMP", "CCMP", "CCMP…
    $ auth         <chr> "PSK", "PSK", "PSK", "PSK", NA, "PSK", "PSK", "PSK", "PSK…
    $ signal       <dbl> -30, -30, -68, -37, -63, -27, -38, -38, -66, -42, -73, -6…
    $ beacon_count <dbl> 846, 750, 694, 510, 251, 1647, 1251, 704, 617, 1390, 28, …
    $ iv           <dbl> 504, 116, 26, 21, 3430, 80, 11, 0, 0, 86, 0, 0, 907, 806,…
    $ lan          <chr> "0.  0.  0.  0", "0.  0.  0.  0", "0.  0.  0.  0", "0.  0…
    $ id_len       <dbl> 12, 4, 2, 14, 13, 12, 13, 24, 12, 10, 24, 24, 12, 8, 4, 3…
    $ essid        <chr> "C322U13 3965", "Cnet", "KC", "POCO X5 Pro 5G", "MIREA_HO…
    $ key_type     <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…

``` r
glimpse(wifi_clients)
```

    Rows: 12,081
    Columns: 7
    $ mac         <chr> "CA:66:3B:8F:56:DD", "96:35:2D:3D:85:E6", "5C:3A:45:9E:1A:…
    $ first_seen  <dttm> 2023-07-28 09:13:03, 2023-07-28 09:13:03, 2023-07-28 09:1…
    $ last_seen   <dttm> 2023-07-28 10:59:44, 2023-07-28 09:13:03, 2023-07-28 11:5…
    $ signal      <dbl> -33, -65, -39, -61, -53, -43, -31, -71, -74, -65, -45, -65…
    $ pkt         <dbl> 858, 4, 432, 958, 1, 344, 163, 3, 115, 437, 265, 77, 7, 71…
    $ assoc_bssid <chr> "BE:F1:71:D5:17:8B", "(not associated)", "BE:F1:71:D6:10:D…
    $ probe       <chr> "C322U13 3965", "IT2 Wireless", "C322U21 0566", "C322U13 3…

##Анализ точек доступа 1. Определение небезопасных точек OPN

``` r
open_ap <- wifi_ap %>%
filter(privacy=="OPN") %>%
arrange(desc(signal)) %>%
select(bssid, essid, privacy, signal, chan)

if(nrow(open_ap) > 0){
print(open_ap)
cat("Всего небезопасных точек:", nrow(open_ap), "\n")
cat("Процент от всех:", round(100*nrow(open_ap)/nrow(wifi_ap),1), "%\n")
} else cat("Небезопасные точки отсутствуют\n")
```

    # A tibble: 23 × 5
       bssid             essid         privacy signal  chan
       <chr>             <chr>         <chr>    <dbl> <dbl>
     1 00:3E:1A:5D:14:45 MT_FREE       OPN        -57    11
     2 E8:28:C1:DC:B2:52 MIREA_HOTSPOT OPN        -63     6
     3 E8:28:C1:DC:B2:50 MIREA_GUESTS  OPN        -63     6
     4 E8:28:C1:DD:04:52 MIREA_HOTSPOT OPN        -67    11
     5 02:BC:15:7E:D5:DC MT_FREE       OPN        -67    11
     6 00:03:7A:1A:03:56 MT_FREE       OPN        -68    11
     7 02:67:F1:B0:6C:98 MT_FREE       OPN        -68    11
     8 00:53:7A:99:98:56 MT_FREE       OPN        -68    11
     9 E8:28:C1:DE:74:32 MIREA_HOTSPOT OPN        -69     6
    10 E8:28:C1:DC:C8:32 MIREA_HOTSPOT OPN        -69     1
    # ℹ 13 more rows
    Всего небезопасных точек: 23 
    Процент от всех: 24 %

1.  Производители AP

``` r
extract_oui <- function(mac){ ifelse(!is.na(mac)&mac!="", str_replace_all(substr(mac,1,8),":","-"), NA_character_) }

ap_vendor <- wifi_ap %>%
mutate(vendor = extract_oui(bssid)) %>%
select(bssid, essid, vendor, privacy, signal)

vendor_count <- ap_vendor %>%
count(vendor, name="count") %>%
arrange(desc(count))

print(head(vendor_count,10))
```

    # A tibble: 10 × 2
       vendor   count
       <chr>    <int>
     1 E8-28-C1    17
     2 38-1A-52     6
     3 00-23-EB     4
     4 00-26-99     4
     5 A6-02-B9     3
     6 BE-F1-71     3
     7 00-26-CB     2
     8 00-03-7A     1
     9 00-03-7F     1
    10 00-3E-1A     1

``` r
cat("Уникальных производителей:", n_distinct(ap_vendor$vendor, na.rm=TRUE), "\n")
```

    Уникальных производителей: 64 

1.  Точки с WPA3

``` r
wpa3_ap <- wifi_ap %>%
filter(str_detect(toupper(coalesce(auth,"")),"WPA3") |
str_detect(toupper(coalesce(privacy,"")),"WPA3") |
str_detect(toupper(coalesce(cipher,"")),"WPA3")) %>%
arrange(desc(signal)) %>%
select(bssid, essid, privacy, auth, cipher, signal)

if(nrow(wpa3_ap)>0){
print(wpa3_ap)
cat("Всего WPA3 AP:", nrow(wpa3_ap), "\n")
}else cat("WPA3 устройства не найдены\n")
```

    # A tibble: 5 × 6
      bssid             essid                            privacy auth  cipher signal
      <chr>             <chr>                            <chr>   <chr> <chr>   <dbl>
    1 BE:FD:EF:18:92:44 "Димасик"                        WPA3 W… SAE … CCMP      -64
    2 CE:48:E7:86:4E:33 "iPhone (Анастасия)"             WPA3 W… SAE … CCMP      -65
    3 3A:DA:00:F9:0C:02 "iPhone XS Max \U0001f98a\U0001… WPA3 W… SAE … CCMP      -65
    4 8E:1F:94:96:DA:FD "iPhone (Анастасия)"             WPA3 W… SAE … CCMP      -67
    5 A2:FE:FF:B8:9B:C9 "Christie’s"                     WPA3 W… SAE … CCMP      -70
    Всего WPA3 AP: 5 

1.  Длительность сессий AP

``` r
ap_sessions <- wifi_ap %>%
group_by(bssid) %>%
arrange(first_seen) %>%
mutate(gap_m = as.numeric(difftime(first_seen, lag(last_seen, default=first(first_seen)),"mins")),
sess_id = cumsum(gap_m>45 | row_number()==1)) %>%
group_by(bssid,sess_id) %>%
summarise(essid=first(essid),
start=min(first_seen),
end=max(last_seen),
.groups="drop") %>%
mutate(duration_min=as.numeric(difftime(end,start,"mins"))) %>%
arrange(desc(duration_min)) %>%
select(bssid, essid, start, end, duration_min)

print(head(ap_sessions,10))
```

    # A tibble: 10 × 5
       bssid             essid  start               end                 duration_min
       <chr>             <chr>  <dttm>              <dttm>                     <dbl>
     1 E8:28:C1:DD:04:52 MIREA… 2023-07-28 09:13:09 2023-07-28 11:56:05         9776
     2 E8:28:C1:DC:B2:52 MIREA… 2023-07-28 09:13:03 2023-07-28 11:55:38         9755
     3 6E:C7:EC:16:DA:1A Cnet   2023-07-28 09:13:03 2023-07-28 11:55:12         9729
     4 E8:28:C1:DC:B2:50 MIREA… 2023-07-28 09:13:06 2023-07-28 11:55:12         9726
     5 8E:55:4A:85:5B:01 Vladi… 2023-07-28 09:13:06 2023-07-28 11:55:09         9723
     6 00:26:99:BA:75:80 GIVC   2023-07-28 09:13:20 2023-07-28 11:55:10         9710
     7 00:26:99:F2:7A:E2 GIVC   2023-07-28 09:13:06 2023-07-28 11:54:53         9707
     8 1E:93:E3:1B:3C:F4 Galax… 2023-07-28 09:13:04 2023-07-28 11:53:37         9633
     9 9A:75:A8:B9:04:1E KC     2023-07-28 09:13:03 2023-07-28 11:53:31         9628
    10 00:23:EB:E3:81:F2 GIVC   2023-07-28 09:13:40 2023-07-28 11:53:35         9595

``` r
cat("Максимальная длительность:", round(max(ap_sessions$duration_min),1), "мин\n")
```

    Максимальная длительность: 9776 мин

1.  Топ-10 по скорости

``` r
fast_ap <- wifi_ap %>%
filter(!is.na(speed), speed>0) %>%
arrange(desc(speed)) %>%
slice(1:10) %>%
select(bssid, essid, speed, chan, signal)

print(fast_ap)
```

    # A tibble: 10 × 5
       bssid             essid              speed  chan signal
       <chr>             <chr>              <dbl> <dbl>  <dbl>
     1 CE:48:E7:86:4E:33 iPhone (Анастасия)   866    44    -65
     2 8E:1F:94:96:DA:FD iPhone (Анастасия)   866    44    -67
     3 9A:75:A8:B9:04:1E KC                   360     1    -68
     4 4A:EC:1E:DB:BF:95 POCO X5 Pro 5G       360     7    -37
     5 56:C5:2B:9F:84:90 OnePlus 6T           360     1    -64
     6 E8:28:C1:DC:B2:41 MIREA_GUESTS         360    48    -89
     7 E8:28:C1:DC:B2:40 MIREA_HOTSPOT        360    48    -88
     8 E8:28:C1:DD:04:40 MIREA_HOTSPOT        360    52    -84
     9 E8:28:C1:DD:04:41 MIREA_GUESTS         360    52    -83
    10 14:EB:B6:6A:76:37 Gnezdo_lounge 2      360     3    -85

``` r
cat("Максимальная скорость:", max(fast_ap$speed), "Mbps\n")
```

    Максимальная скорость: 866 Mbps

1.  Beacon-трафик

``` r
beacon_stats <- wifi_ap %>%
mutate(hours=as.numeric(difftime(last_seen, first_seen, "hours")),
rate=ifelse(hours>0, beacon_count/hours, beacon_count)) %>%
filter(!is.na(rate), hours>0, rate>0) %>%
arrange(desc(rate)) %>%
select(bssid, essid, beacon_count, hours, rate, chan)

print(head(beacon_stats,10))
```

    # A tibble: 10 × 6
       bssid             essid                        beacon_count hours  rate  chan
       <chr>             <chr>                               <dbl> <dbl> <dbl> <dbl>
     1 F2:30:AB:E9:03:ED "iPhone (Uliana)"                       6     7 0.857     1
     2 B2:CF:C0:00:4A:60 "Михаил's Galaxy M32"                   4     5 0.8       6
     3 3A:DA:00:F9:0C:02 "iPhone XS Max \U0001f98a\U…            5     9 0.556     6
     4 02:BC:15:7E:D5:DC "MT_FREE"                               1     2 0.5      11
     5 00:3E:1A:5D:14:45 "MT_FREE"                               1     2 0.5      11
     6 D2:25:91:F6:6C:D8 "Саня"                                  5    13 0.385    12
     7 BE:F1:71:D6:10:D7 "C322U21 0566"                       1647  9461 0.174    11
     8 00:03:7A:1A:03:56 "MT_FREE"                               1     6 0.167    11
     9 38:1A:52:0D:84:D7 "EBFCD57F-EE81fI_DL_1AO2T"            704  4319 0.163    11
    10 0A:C5:E1:DB:17:7B "AndroidAP177B"                      1251  8608 0.145    11

``` r
cat("Максимальная частота beacon:", round(max(beacon_stats$rate),1), "в час\n")
```

    Максимальная частота beacon: 0.9 в час

##Анализ клиентов 1. Производители клиентов

``` r
client_vendor <- wifi_clients %>%
mutate(oui=substr(mac,1,8)) %>%
select(mac, oui, probe) %>%
distinct()

print(head(client_vendor,10))
```

    # A tibble: 10 × 3
       mac               oui      probe       
       <chr>             <chr>    <chr>       
     1 CA:66:3B:8F:56:DD CA:66:3B C322U13 3965
     2 96:35:2D:3D:85:E6 96:35:2D IT2 Wireless
     3 5C:3A:45:9E:1A:7B 5C:3A:45 C322U21 0566
     4 C0:E4:34:D8:E7:E5 C0:E4:34 C322U13 3965
     5 5E:8E:A6:5E:34:81 5E:8E:A6 <NA>        
     6 10:51:07:CB:33:E7 10:51:07 <NA>        
     7 68:54:5A:40:35:9E 68:54:5A C322U06 5179
     8 74:4C:A1:70:CE:F7 74:4C:A1 <NA>        
     9 8A:A3:5A:33:76:57 8A:A3:5A <NA>        
    10 CA:54:C4:8B:B5:3A CA:54:C4 GIVC        

``` r
cat("Уникальных производителей клиентов:", n_distinct(client_vendor$oui), "\n")
```

    Уникальных производителей клиентов: 11792 

1.  Нерандомизированные MAC

``` r
clients_nonrnd <- wifi_clients %>%
mutate(second=substr(mac,2,2),
is_rnd=second %in% c("2","6","a","e","A","E")) %>%
filter(!is_rnd) %>%
arrange(desc(pkt)) %>%
select(mac, first_seen, last_seen, signal, pkt, probe)

print(head(clients_nonrnd,10))
```

    # A tibble: 10 × 6
       mac               first_seen          last_seen           signal   pkt probe 
       <chr>             <dttm>              <dttm>               <dbl> <dbl> <chr> 
     1 00:95:69:E7:7D:21 2023-07-28 09:13:15 2023-07-28 11:56:17    -33  8171 nvrip…
     2 00:95:69:E7:7C:ED 2023-07-28 09:13:11 2023-07-28 11:56:13    -55  4096 nvrip…
     3 00:95:69:E7:7F:35 2023-07-28 09:13:11 2023-07-28 11:56:07    -69  2245 nvrip…
     4 98:F6:21:72:9E:D6 2023-07-28 10:41:15 2023-07-28 11:56:14    -59  2143 Beeli…
     5 C0:E4:34:D8:E7:E5 2023-07-28 09:13:03 2023-07-28 11:53:16    -61   958 C322U…
     6 74:DF:BF:7B:00:19 2023-07-28 09:45:48 2023-07-28 10:31:11    -65   911 MIREA…
     7 50:3E:AA:33:52:EC 2023-07-28 09:48:25 2023-07-28 11:56:06    -53   862 <NA>  
     8 14:13:33:59:9F:AB 2023-07-28 09:13:12 2023-07-28 10:26:21    -57   849 <NA>  
     9 04:8C:9A:0B:40:EA 2023-07-28 10:27:47 2023-07-28 11:55:51    -73   756 MIREA…
    10 BC:F1:71:D5:0E:53 2023-07-28 09:13:17 2023-07-28 11:50:10    -35   675 <NA>  

``` r
cat("Количество нерандомизированных:", nrow(clients_nonrnd), "\n")
```

    Количество нерандомизированных: 220 

``` r
cat("Процент:", round(100*nrow(clients_nonrnd)/nrow(wifi_clients),1), "%\n")
```

    Процент: 1.8 %

1.  Кластеризация probe-запросов

``` r
probe_stats <- wifi_clients %>%
filter(!is.na(probe), probe!="") %>%
group_by(mac, probe) %>%
summarise(mean_signal=mean(signal,na.rm=TRUE),
sd_signal=sd(signal,na.rm=TRUE),
n_obs=n(),
first_seen=min(first_seen),
last_seen=max(last_seen),
.groups="drop") %>%
arrange(sd_signal)

probe_stats
```

    # A tibble: 1,477 × 7
       mac               probe       mean_signal sd_signal n_obs first_seen         
       <chr>             <chr>             <dbl>     <dbl> <int> <dttm>             
     1 00:90:4C:E6:54:54 Redmi               -65        NA     1 2023-07-28 09:16:59
     2 00:95:69:E7:7C:ED nvripcsuite         -55        NA     1 2023-07-28 09:13:11
     3 00:95:69:E7:7D:21 nvripcsuite         -33        NA     1 2023-07-28 09:13:15
     4 00:95:69:E7:7F:35 nvripcsuite         -69        NA     1 2023-07-28 09:13:11
     5 00:F4:8D:F7:C5:19 Redmi 12            -73        NA     1 2023-07-28 10:45:04
     6 02:00:00:00:00:00 xt3 w64dtg…         -67        NA     1 2023-07-28 09:54:40
     7 02:06:2B:A5:0C:31 Avenue611           -65        NA     1 2023-07-28 09:55:12
     8 02:1D:0F:A4:94:74 iPhone (Ди…         -61        NA     1 2023-07-28 09:57:08
     9 02:32:DC:56:5C:82 Timo Resort         -84        NA     1 2023-07-28 10:58:23
    10 02:35:E9:C2:44:5F iPhone (Ма…         -88        NA     1 2023-07-28 10:00:55
    # ℹ 1,467 more rows
    # ℹ 1 more variable: last_seen <dttm>

1.  Стабильность сигнала

``` r
probe_stats <- probe_stats %>%
mutate(stability_score=1/(sd_signal+1e-6))

top_cluster <- probe_stats %>%
arrange(desc(stability_score)) %>%
slice(1)

top_cluster
```

    # A tibble: 1 × 8
      mac               probe mean_signal sd_signal n_obs first_seen         
      <chr>             <chr>       <dbl>     <dbl> <int> <dttm>             
    1 00:90:4C:E6:54:54 Redmi         -65        NA     1 2023-07-28 09:16:59
    # ℹ 2 more variables: last_seen <dttm>, stability_score <dbl>

## Оценка результата

В результате лабораторной работы мы получили знания о методах
исследования радиоэлектронной обстановки, составили представление о
механизмах работы Wi-Fi сетей, закрепили практические навыки
использования языка программирования R и знания основных функций
обработки данных экосистемы tidyverse.

## Вывод

Таким образом, мы научились провоить анализ журналов с использованием
программного пакета dplyr.
