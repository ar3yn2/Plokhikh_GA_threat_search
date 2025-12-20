# 5 Пр
gleb.plokhikh@yandex.ru

## Цель работы

1.  Получить знания о методах исследования радиоэлектронной обстановки.
2.  Понять работу Wi-Fi сетей на канальном и сетевом уровнях модели OSI
3.  Закрепить практические навыки использования языка программирования R
    для обработки данных
4.  Научиться применять функции tidyverse для работы с данными.

## Исходные данные

1.  Программное обеспечение Windows 11
2.  Rstudio Desktop
3.  Интерпретатор языка R 4.5.1
4.  dplyr
5.  Логи tcpdump и airodump-ng для анализа беспроводных сетей

## План

1.  Подготовка данных для анализа
2.  Анализ точек доступа
3.  Анализ данных клиентов

## Шаги:

## Подготовка данных

1.  Импортирование данных

``` r
libs <- c("tidyverse","lubridate","stringr","curl")
installed <- libs %in% installed.packages()[,"Package"]
if(any(!installed)) install.packages(libs[!installed])

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
url <- "https://storage.yandexcloud.net/dataset.ctfsec/P2_wifi_data.csv"
tmp <- tempfile(fileext = ".csv")
curl::curl_download(url, tmp)
```

1.  Считываем весь CSV в R

``` r
lines <- read_lines(tmp)
divider <- which(str_detect(lines, "Station MAC"))
ap_data <- read_csv(tmp, n_max = divider - 2, col_names = FALSE)
```

    Warning: One or more parsing issues, call `problems()` on your data frame for details,
    e.g.:
      dat <- vroom(...)
      problems(dat)

    Rows: 169 Columns: 15
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ","
    chr (15): X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
client_data <- read_csv(tmp, skip = divider - 1, col_names = FALSE)
```

    Warning: One or more parsing issues, call `problems()` on your data frame for details,
    e.g.:
      dat <- vroom(...)
      problems(dat)

    Rows: 12082 Columns: 7
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ","
    chr (7): X1, X2, X3, X4, X5, X6, X7

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

1.  Преобразование датасетов в аккуратный вид

``` r
ap_clean <- ap_data |>
rename(
bssid = X1,
first_seen = X2,
last_seen = X3,
channel = X4,
speed = X5,
privacy = X6,
cipher = X7,
auth = X8,
signal = X9,
beacons = X10,
iv = X11,
lan = X12,
id_len = X13,
essid = X14
) |>
mutate(
first_seen = as_datetime(first_seen),
last_seen = as_datetime(last_seen),
across(c(channel,speed,signal,beacons,iv,id_len), as.numeric)
)
```

    Warning: There were 8 warnings in `mutate()`.
    The first warning was:
    ℹ In argument: `first_seen = as_datetime(first_seen)`.
    Caused by warning:
    !  2 failed to parse.
    ℹ Run `dplyr::last_dplyr_warnings()` to see the 7 remaining warnings.

``` r
clients_clean <- client_data |>
rename(
mac = X1,
first_seen = X2,
last_seen = X3,
signal = X4,
packets = X5,
assoc_bssid = X6,
probe = X7
) |>
mutate(
first_seen = as_datetime(first_seen),
last_seen = as_datetime(last_seen),
signal = as.numeric(signal),
packets = as.numeric(packets)
) |>
filter(!is.na(mac), mac != "")
```

    Warning: There were 4 warnings in `mutate()`.
    The first warning was:
    ℹ In argument: `first_seen = as_datetime(first_seen)`.
    Caused by warning:
    !  1 failed to parse.
    ℹ Run `dplyr::last_dplyr_warnings()` to see the 3 remaining warnings.

1.  Просмотр структуры данных

``` r
glimpse(ap_clean)
```

    Rows: 169
    Columns: 15
    $ bssid      <chr> "BSSID", "BE:F1:71:D5:17:8B", "6E:C7:EC:16:DA:1A", "9A:75:A…
    $ first_seen <dttm> NA, 2023-07-28 09:13:03, 2023-07-28 09:13:03, 2023-07-28 0…
    $ last_seen  <dttm> NA, 2023-07-28 11:50:50, 2023-07-28 11:55:12, 2023-07-28 1…
    $ channel    <dbl> NA, 1, 1, 1, 7, 6, 6, 11, 11, 11, 1, 6, 14, 11, 11, 6, 6, 6…
    $ speed      <dbl> NA, 195, 130, 360, 360, 130, 130, 195, 130, 130, 195, 180, …
    $ privacy    <chr> "Privacy", "WPA2", "WPA2", "WPA2", "WPA2", "WPA2", "OPN", "…
    $ cipher     <chr> "Cipher", "CCMP", "CCMP", "CCMP", "CCMP", "CCMP", NA, "CCMP…
    $ auth       <chr> "Authentication", "PSK", "PSK", "PSK", "PSK", "PSK", NA, "P…
    $ signal     <dbl> NA, -30, -30, -68, -37, -57, -63, -27, -38, -38, -66, -42, …
    $ beacons    <dbl> NA, 846, 750, 694, 510, 647, 251, 1647, 1251, 704, 617, 139…
    $ iv         <dbl> NA, 504, 116, 26, 21, 6, 3430, 80, 11, 0, 0, 86, 0, 0, 0, 9…
    $ lan        <chr> "LAN IP", "0.  0.  0.  0", "0.  0.  0.  0", "0.  0.  0.  0"…
    $ id_len     <dbl> NA, 12, 4, 2, 14, 25, 13, 12, 13, 24, 12, 10, 0, 24, 24, 12…
    $ essid      <chr> "ESSID", "C322U13 3965", "Cnet", "KC", "POCO X5 Pro 5G", NA…
    $ X15        <chr> "Key", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …

``` r
glimpse(clients_clean)
```

    Rows: 12,082
    Columns: 7
    $ mac         <chr> "Station MAC", "CA:66:3B:8F:56:DD", "96:35:2D:3D:85:E6", "…
    $ first_seen  <dttm> NA, 2023-07-28 09:13:03, 2023-07-28 09:13:03, 2023-07-28 …
    $ last_seen   <dttm> NA, 2023-07-28 10:59:44, 2023-07-28 09:13:03, 2023-07-28 …
    $ signal      <dbl> NA, -33, -65, -39, -61, -53, -43, -31, -71, -74, -65, -45,…
    $ packets     <dbl> NA, 858, 4, 432, 958, 1, 344, 163, 3, 115, 437, 265, 77, 7…
    $ assoc_bssid <chr> "BSSID", "BE:F1:71:D5:17:8B", "(not associated)", "BE:F1:7…
    $ probe       <chr> "Probed ESSIDs", "C322U13 3965", "IT2 Wireless", "C322U21 …

## Анализ точек доступа

1.  Определение небезопасных точек

``` r
open_ap <- ap_clean |>
filter(privacy == "OPN") |>
arrange(desc(signal)) |>
select(bssid, essid, privacy, signal)
print(open_ap)
```

    # A tibble: 42 × 4
       bssid             essid         privacy signal
       <chr>             <chr>         <chr>    <dbl>
     1 E8:28:C1:DC:FF:F2 <NA>          OPN         -1
     2 00:25:00:FF:94:73 <NA>          OPN         -1
     3 E0:D9:E3:48:FF:D2 <NA>          OPN         -1
     4 00:26:99:F2:7A:E0 <NA>          OPN         -1
     5 00:AB:0A:00:10:10 <NA>          OPN         -1
     6 00:26:99:F2:7A:EF <NA>          OPN         -1
     7 00:3E:1A:5D:14:45 MT_FREE       OPN        -57
     8 E8:28:C1:DC:B2:52 MIREA_HOTSPOT OPN        -63
     9 E8:28:C1:DC:B2:50 MIREA_GUESTS  OPN        -63
    10 E8:28:C1:DC:B2:51 <NA>          OPN        -63
    # ℹ 32 more rows

``` r
cat("Всего открытых точек:", nrow(open_ap), "\n")
```

    Всего открытых точек: 42 

1.  Производители oui

``` r
get_oui <- function(mac) str_replace_all(substr(mac,1,8), ":", "-")
ap_vendors <- ap_clean |>
mutate(vendor = get_oui(bssid)) |>
count(vendor, name = "num") |>
arrange(desc(num))
print(head(ap_vendors, 10))
```

    # A tibble: 10 × 2
       vendor     num
       <chr>    <int>
     1 E8-28-C1    45
     2 E0-D9-E3     9
     3 00-23-EB     7
     4 00-26-99     7
     5 38-1A-52     6
     6 9E-A3-A9     3
     7 A6-02-B9     3
     8 BE-F1-71     3
     9 00-03-7A     2
    10 00-03-7F     2

1.  WPA3 точки

``` r
wpa3_ap <- ap_clean |>
filter(str_detect(toupper(coalesce(auth,"")), "WPA3") |
str_detect(toupper(coalesce(privacy,"")), "WPA3") |
str_detect(toupper(coalesce(cipher,"")), "WPA3")) |>
arrange(desc(signal)) |>
select(bssid, essid, privacy, auth, cipher, signal)
print(wpa3_ap)
```

    # A tibble: 8 × 6
      bssid             essid                            privacy auth  cipher signal
      <chr>             <chr>                            <chr>   <chr> <chr>   <dbl>
    1 76:C5:A0:70:08:96  <NA>                            WPA3 W… SAE … CCMP      -52
    2 BE:FD:EF:18:92:44 "Димасик"                        WPA3 W… SAE … CCMP      -64
    3 CE:48:E7:86:4E:33 "iPhone (Анастасия)"             WPA3 W… SAE … CCMP      -65
    4 3A:DA:00:F9:0C:02 "iPhone XS Max \U0001f98a\U0001… WPA3 W… SAE … CCMP      -65
    5 8E:1F:94:96:DA:FD "iPhone (Анастасия)"             WPA3 W… SAE … CCMP      -67
    6 A2:FE:FF:B8:9B:C9 "Christie’s"                     WPA3 W… SAE … CCMP      -70
    7 26:20:53:0C:98:E8  <NA>                            WPA3 W… SAE … CCMP      -85
    8 96:FF:FC:91:EF:64  <NA>                            WPA3 W… SAE … CCMP      -85

1.  Сессии точек доступа

``` r
ap_sessions <- ap_clean |>
arrange(bssid, first_seen) |>
group_by(bssid) |>
mutate(gap_min = as.numeric(difftime(first_seen, lag(last_seen, default = first(first_seen)), units="mins")),
session_id = cumsum(gap_min > 45 | row_number()==1)) |>
group_by(bssid, session_id) |>
summarise(start = min(first_seen), end = max(last_seen), .groups="drop") |>
mutate(duration_min = as.numeric(difftime(end, start, units="mins"))) |>
arrange(desc(duration_min))
head(ap_sessions,10)
```

    # A tibble: 10 × 5
       bssid         session_id start               end                 duration_min
       <chr>              <int> <dttm>              <dttm>                     <dbl>
     1 00:25:00:FF:…          1 2023-07-28 09:13:06 2023-07-28 11:56:21         163.
     2 E8:28:C1:DD:…          1 2023-07-28 09:13:09 2023-07-28 11:56:05         163.
     3 E8:28:C1:DC:…          1 2023-07-28 09:13:03 2023-07-28 11:55:38         163.
     4 08:3A:2F:56:…          1 2023-07-28 09:13:27 2023-07-28 11:55:53         162.
     5 6E:C7:EC:16:…          1 2023-07-28 09:13:03 2023-07-28 11:55:12         162.
     6 E8:28:C1:DC:…          1 2023-07-28 09:13:06 2023-07-28 11:55:12         162.
     7 48:5B:39:F9:…          1 2023-07-28 09:13:06 2023-07-28 11:55:11         162.
     8 E8:28:C1:DC:…          1 2023-07-28 09:13:06 2023-07-28 11:55:11         162.
     9 E8:28:C1:DC:…          1 2023-07-28 09:13:06 2023-07-28 11:55:10         162.
    10 8E:55:4A:85:…          1 2023-07-28 09:13:06 2023-07-28 11:55:09         162.

## Анализ клиентов

1.  Производители клиентов

``` r
clients_clean <- clients_clean |>
mutate(oui = substr(mac,1,8))
clients_clean |>
count(oui, name = "num") |> arrange(desc(num)) |> head(10)
```

    # A tibble: 10 × 2
       oui        num
       <chr>    <int>
     1 BC:F1:71    52
     2 0E:EF:92    36
     3 DA:A1:19    33
     4 FE:41:FA    32
     5 10:51:07    25
     6 5A:AB:CF    18
     7 A4:02:B9    17
     8 E2:CC:F8    10
     9 8C:55:4A     7
    10 12:BB:F3     5

1.  Нерандомизированные MAC

``` r
clients_nonrnd <- clients_clean |>
filter(!substr(mac,2,2) %in% c("2","6","a","A","e","E"))
nrow(clients_nonrnd)
```

    [1] 221

1.  Кластеры probe-запросов

``` r
probe_stats <- clients_clean |>
filter(!is.na(probe), probe != "") |>
separate_rows(probe, sep=",") |>
group_by(mac, probe) |>
summarise(first_seen = min(first_seen),
last_seen = max(last_seen),
mean_signal = mean(signal, na.rm=TRUE),
sd_signal = sd(signal, na.rm=TRUE),
.groups="drop")
probe_stats
```

    # A tibble: 1,833 × 6
       mac       probe first_seen          last_seen           mean_signal sd_signal
       <chr>     <chr> <dttm>              <dttm>                    <dbl>     <dbl>
     1 00:90:4C… "Red… 2023-07-28 09:16:59 2023-07-28 10:21:15         -65        NA
     2 00:95:69… "nvr… 2023-07-28 09:13:11 2023-07-28 11:56:13         -55        NA
     3 00:95:69… "nvr… 2023-07-28 09:13:15 2023-07-28 11:56:17         -33        NA
     4 00:95:69… "nvr… 2023-07-28 09:13:11 2023-07-28 11:56:07         -69        NA
     5 00:F4:8D… "Hor… 2023-07-28 10:45:04 2023-07-28 11:43:26         -73        NA
     6 00:F4:8D… "Red… 2023-07-28 10:45:04 2023-07-28 11:43:26         -73        NA
     7 02:00:00… "CPP… 2023-07-28 09:54:40 2023-07-28 11:55:36         -67        NA
     8 02:00:00… "MIR… 2023-07-28 09:54:40 2023-07-28 11:55:36         -67        NA
     9 02:00:00… "MIR… 2023-07-28 09:54:40 2023-07-28 11:55:36         -67        NA
    10 02:00:00… "\\x… 2023-07-28 09:54:40 2023-07-28 11:55:36         -67        NA
    # ℹ 1,823 more rows

1.  Стабильность сигнала

``` r
probe_stats <- probe_stats |>
mutate(stability = 1 / (sd_signal + 1e-6)) |>
arrange(desc(stability))
probe_stats |> slice(1)
```

    # A tibble: 1 × 7
      mac        probe first_seen          last_seen           mean_signal sd_signal
      <chr>      <chr> <dttm>              <dttm>                    <dbl>     <dbl>
    1 00:90:4C:… Redmi 2023-07-28 09:16:59 2023-07-28 10:21:15         -65        NA
    # ℹ 1 more variable: stability <dbl>

## Оценка результата

1.Проанализированы точки доступа и клиенты сети. 2.Выявлены открытые и
WPA3 AP. 3.Определены производители и стабильность сигналов клиентов.

## Вывод

Таким образом, мы научились провоить анализ журналов с использованием
программного пакета dplyr.
