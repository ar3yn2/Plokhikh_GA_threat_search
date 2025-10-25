# Исследование метаданных DNS трафика
gleb.plokhikh@yandex.ru

## Цель работы

1.  Зекрепить практические навыки использования языка программирования R
    для обработки данных
2.  Закрепить знания основных функций обработки данных экосистемы
    tidyverse языка R
3.  Закрепить навыки исследования метаданных DNS трафика

## Исходные данные

1.  Программное обеспечение ОС Windows 11
2.  RStudio
3.  Интерпретатор языка R 4.1

## План

1.  Импортируйте данные DNS –
    https://storage.yandexcloud.net/dataset.ctfsec/dns.zip Данные были
    собраны с помощью сетевого анализатора zeek
2.  Добавьте пропущенные данные о структуре данных (назначении столбцов)
3.  Преобразуйте данные в столбцах в нужный формат,просмотрите общую
    структуру данных с помощью функции glimpse()
4.  Сколько участников информационного обмена всети Доброй Организации?
5.  Какое соотношение участников обмена внутрисети и участников
    обращений к внешним ресурсам?
6.  Найдите топ-10 участников сети, проявляющих наибольшую сетевую
    активность.
7.  Найдите топ-10 доменов, к которым обращаются пользователи сети и
    соответственное количество обращений
8.  Опеределите базовые статистические характеристики (функция summary()
    ) интервала времени между последовательными обращениями к топ-10
    доменам.
9.  Часто вредоносное программное обеспечение использует DNS канал в
    качестве канала управления, периодически отправляя запросы на
    подконтрольный злоумышленникам DNS сервер. По периодическим запросам
    на один и тот же домен можно выявить скрытый DNS канал. Есть ли
    такие IP адреса в исследуемом датасете?
10. Определите местоположение (страну, город) и организацию-провайдера
    для топ-10 доменов. Для этого можно использовать сторонние
    сервисы,например http://ip-api.com (API-эндпоинт –
    http://ip-api.com/json).

## Шаги:

1\. Импортируем данные DNS

``` r
options(repos = c(CRAN = "https://mirror.truenetwork.ru/CRAN/"))
install.packages("readr")
```

    Устанавливаю пакет в 'C:/Users/timav/AppData/Local/R/win-library/4.5'
    (потому что 'lib' не определено)

    пакет 'readr' успешно распакован, MD5-суммы проверены

    Warning: не могу удалить прежнюю установку пакета 'readr'

    Warning in file.copy(savedcopy, lib, recursive = TRUE): проблема с копированием
    C:\Users\timav\AppData\Local\R\win-library\4.5\00LOCK\readr\libs\x64\readr.dll
    в C:\Users\timav\AppData\Local\R\win-library\4.5\readr\libs\x64\readr.dll:
    Permission denied

    Warning: восстановлен 'readr'


    Скачанные бинарные пакеты находятся в
        C:\Users\timav\AppData\Local\Temp\RtmpIBjOud\downloaded_packages

``` r
install.packages("dplyr")
```

    Устанавливаю пакет в 'C:/Users/timav/AppData/Local/R/win-library/4.5'
    (потому что 'lib' не определено)

    пакет 'dplyr' успешно распакован, MD5-суммы проверены

    Warning: не могу удалить прежнюю установку пакета 'dplyr'

    Warning in file.copy(savedcopy, lib, recursive = TRUE): проблема с копированием
    C:\Users\timav\AppData\Local\R\win-library\4.5\00LOCK\dplyr\libs\x64\dplyr.dll
    в C:\Users\timav\AppData\Local\R\win-library\4.5\dplyr\libs\x64\dplyr.dll:
    Permission denied

    Warning: восстановлен 'dplyr'


    Скачанные бинарные пакеты находятся в
        C:\Users\timav\AppData\Local\Temp\RtmpIBjOud\downloaded_packages

``` r
install.packages("stringr")
```

    Устанавливаю пакет в 'C:/Users/timav/AppData/Local/R/win-library/4.5'
    (потому что 'lib' не определено)

    пакет 'stringr' успешно распакован, MD5-суммы проверены

    Скачанные бинарные пакеты находятся в
        C:\Users\timav\AppData\Local\Temp\RtmpIBjOud\downloaded_packages

``` r
install.packages("httr")
```

    Устанавливаю пакет в 'C:/Users/timav/AppData/Local/R/win-library/4.5'
    (потому что 'lib' не определено)

    пакет 'httr' успешно распакован, MD5-суммы проверены

    Скачанные бинарные пакеты находятся в
        C:\Users\timav\AppData\Local\Temp\RtmpIBjOud\downloaded_packages

``` r
install.packages("jsonlite")
```

    Устанавливаю пакет в 'C:/Users/timav/AppData/Local/R/win-library/4.5'
    (потому что 'lib' не определено)

    пакет 'jsonlite' успешно распакован, MD5-суммы проверены

    Warning: не могу удалить прежнюю установку пакета 'jsonlite'

    Warning in file.copy(savedcopy, lib, recursive = TRUE): проблема с копированием
    C:\Users\timav\AppData\Local\R\win-library\4.5\00LOCK\jsonlite\libs\x64\jsonlite.dll
    в
    C:\Users\timav\AppData\Local\R\win-library\4.5\jsonlite\libs\x64\jsonlite.dll:
    Permission denied

    Warning: восстановлен 'jsonlite'


    Скачанные бинарные пакеты находятся в
        C:\Users\timav\AppData\Local\Temp\RtmpIBjOud\downloaded_packages

``` r
library(httr)
library(jsonlite)
library(readr)
library(dplyr)
```


    Присоединяю пакет: 'dplyr'

    Следующие объекты скрыты от 'package:stats':

        filter, lag

    Следующие объекты скрыты от 'package:base':

        intersect, setdiff, setequal, union

``` r
library(stringr)
library(knitr)
temp_dir <- tempdir()
download.file(
  url = "https://storage.yandexcloud.net/dataset.ctfsec/dns.zip",
  destfile = file.path(temp_dir, "dns.zip"),
  mode = "wb"
)
unzip(
  zipfile = file.path(temp_dir, "dns.zip"),
  exdir = temp_dir
)
log_files <- list.files(temp_dir, pattern = "\\.log$", full.names = TRUE)
```

2\. Добавьте пропущенные данные о структуре данных (назначении столбцов)

``` r
column_names <- c(
  "timestamp", "uid", "source_ip", "source_port", "destination_ip", 
  "destination_port", "protocol", "transaction_id", "query", "qclass", 
  "qclass_name", "qtype", "qtype_name", "rcode", "rcode_name", 
  "AA", "TC", "RD", "RA", "Z", "answers", "TTLS", "rejected"
)
dns_data <- invisible(read_delim(
  log_files[1],
  delim = "\t",
  col_names = column_names,
  comment = "#",
  na = c("", "NA", "-"),
  trim_ws = TRUE,
  show_col_types = FALSE
)) %>% as_tibble()
head(dns_data,10)
```

    # A tibble: 10 × 23
         timestamp uid         source_ip source_port destination_ip destination_port
             <dbl> <chr>       <chr>           <dbl> <chr>                     <dbl>
     1 1331901006. CWGtK431H9… 192.168.…       45658 192.168.27.203              137
     2 1331901015. C36a282Jlj… 192.168.…         137 192.168.202.2…              137
     3 1331901016. C36a282Jlj… 192.168.…         137 192.168.202.2…              137
     4 1331901017. C36a282Jlj… 192.168.…         137 192.168.202.2…              137
     5 1331901006. C36a282Jlj… 192.168.…         137 192.168.202.2…              137
     6 1331901007. C36a282Jlj… 192.168.…         137 192.168.202.2…              137
     7 1331901007. C36a282Jlj… 192.168.…         137 192.168.202.2…              137
     8 1331901006. ClEZCt3GLk… 192.168.…         137 192.168.202.2…              137
     9 1331901007. ClEZCt3GLk… 192.168.…         137 192.168.202.2…              137
    10 1331901008. ClEZCt3GLk… 192.168.…         137 192.168.202.2…              137
    # ℹ 17 more variables: protocol <chr>, transaction_id <dbl>, query <chr>,
    #   qclass <dbl>, qclass_name <chr>, qtype <dbl>, qtype_name <chr>,
    #   rcode <dbl>, rcode_name <chr>, AA <lgl>, TC <lgl>, RD <lgl>, RA <lgl>,
    #   Z <dbl>, answers <chr>, TTLS <chr>, rejected <lgl>

3\. Преобразуйте данные в столбцах в нужный формат

``` r
dns_data_clean <- dns_data %>%
  mutate(
    timestamp = as.POSIXct(timestamp, origin = "1970-01-01"),
    source_port = as.numeric(source_port),
    destination_port = as.numeric(destination_port),
    transaction_id = as.numeric(transaction_id),
    qclass = as.numeric(qclass),
    qtype = as.numeric(qtype),
    rcode = as.numeric(rcode),
  ) %>% as_tibble()
head(dns_data_clean,10)
```

    # A tibble: 10 × 23
       timestamp           uid                source_ip   source_port destination_ip
       <dttm>              <chr>              <chr>             <dbl> <chr>         
     1 2012-03-16 16:30:05 CWGtK431H9XuaTN4fi 192.168.20…       45658 192.168.27.203
     2 2012-03-16 16:30:15 C36a282Jljz7BsbGH  192.168.20…         137 192.168.202.2…
     3 2012-03-16 16:30:15 C36a282Jljz7BsbGH  192.168.20…         137 192.168.202.2…
     4 2012-03-16 16:30:16 C36a282Jljz7BsbGH  192.168.20…         137 192.168.202.2…
     5 2012-03-16 16:30:05 C36a282Jljz7BsbGH  192.168.20…         137 192.168.202.2…
     6 2012-03-16 16:30:06 C36a282Jljz7BsbGH  192.168.20…         137 192.168.202.2…
     7 2012-03-16 16:30:07 C36a282Jljz7BsbGH  192.168.20…         137 192.168.202.2…
     8 2012-03-16 16:30:06 ClEZCt3GLkJdtGGmAa 192.168.20…         137 192.168.202.2…
     9 2012-03-16 16:30:07 ClEZCt3GLkJdtGGmAa 192.168.20…         137 192.168.202.2…
    10 2012-03-16 16:30:07 ClEZCt3GLkJdtGGmAa 192.168.20…         137 192.168.202.2…
    # ℹ 18 more variables: destination_port <dbl>, protocol <chr>,
    #   transaction_id <dbl>, query <chr>, qclass <dbl>, qclass_name <chr>,
    #   qtype <dbl>, qtype_name <chr>, rcode <dbl>, rcode_name <chr>, AA <lgl>,
    #   TC <lgl>, RD <lgl>, RA <lgl>, Z <dbl>, answers <chr>, TTLS <chr>,
    #   rejected <lgl>

4\. Сколько участников информационного обмена в сети Доброй Организации?

``` r
unique_source_ips <- unique(dns_data_clean$source_ip)
unique_destination_ips <- unique(dns_data_clean$destination_ip)
all_ips<-unique(c(unique_source_ips, unique_destination_ips))
length(all_ips)
```

    [1] 1359

5\. Какое соотношение участников обмена внутрисети и участников
обращений к внешним ресурсам?

``` r
internal_ips <- all_ips[grepl("^(10\\.|192\\.168\\.|172\\.(1[6-9]|2[0-9]|3[0-1])\\.)", all_ips)]
external_ips <- all_ips[!grepl("^(10\\.|192\\.168\\.|172\\.(1[6-9]|2[0-9]|3[0-1])\\.)", all_ips)]
length(internal_ips) / length(external_ips)
```

    [1] 13.77174

6\. Найдите топ-10 участников сети, проявляющих наибольшую сетевую
активность.

``` r
dns_data_clean%>%
  group_by(source_ip)%>%
  count(sort = TRUE) %>%
  as_tibble() %>%
  head(10)
```

    # A tibble: 10 × 2
       source_ip           n
       <chr>           <int>
     1 10.10.117.210   75943
     2 192.168.202.93  26522
     3 192.168.202.103 18121
     4 192.168.202.76  16978
     5 192.168.202.97  16176
     6 192.168.202.141 14967
     7 10.10.117.209   14222
     8 192.168.202.110 13372
     9 192.168.203.63  12148
    10 192.168.202.106 10784

7\. Найдите топ-10 доменов, к которым обращаются пользователи сети и
соответственное количество обращений

``` r
top_10_domains <- dns_data_clean%>%count(query, sort = TRUE) %>%
  as_tibble() %>% head(10)
top_10_domains
```

    # A tibble: 10 × 2
       query                                                                       n
       <chr>                                                                   <int>
     1 "teredo.ipv6.microsoft.com"                                             39273
     2 "tools.google.com"                                                      14057
     3 "www.apple.com"                                                         13390
     4 "time.apple.com"                                                        13109
     5 "safebrowsing.clients.google.com"                                       11658
     6 "*\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x… 10401
     7 "WPAD"                                                                   9134
     8 "44.206.168.192.in-addr.arpa"                                            7248
     9 "HPE8AA67"                                                               6929
    10 "ISATAP"                                                                 6569

8\. Опеределите базовые статистические характеристики (функция summary()
) интервала времени между последовательными обращениями к топ-10
доменам.

``` r
top_domains_data <- dns_data_clean[dns_data_clean$query %in% top_10_domains$query, ]
top_domains_data <- top_domains_data[order(top_domains_data$query, top_domains_data$timestamp), ]
results <- data.frame(
  Domain = character(),
  Min = numeric(),
  Q1 = numeric(),
  Median = numeric(),
  Mean = numeric(),
  Q3 = numeric(),
  Max = numeric()
)
for(domain in top_10_domains$query) {
  domain_data <- top_domains_data[top_domains_data$query == domain, ]
  domain_data <- domain_data[!is.na(domain_data$timestamp), ]
  
  if(nrow(domain_data) > 1) {
    time_diffs <- diff(as.numeric(domain_data$timestamp))
    domain_stats <- summary(time_diffs)
    results <- rbind(results, data.frame(
      Domain = domain,
      Min = as.numeric(domain_stats["Min."]),
      Q1 = as.numeric(domain_stats["1st Qu."]),
      Median = as.numeric(domain_stats["Median"]),
      Mean = as.numeric(domain_stats["Mean"]),
      Q3 = as.numeric(domain_stats["3rd Qu."]),
      Max = as.numeric(domain_stats["Max."])
    ))
  }
}
results
```

                                                                        Domain Min
    1                                                teredo.ipv6.microsoft.com   0
    2                                                         tools.google.com   0
    3                                                            www.apple.com   0
    4                                                           time.apple.com   0
    5                                          safebrowsing.clients.google.com   0
    6  *\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00   0
    7                                                                     WPAD   0
    8                                              44.206.168.192.in-addr.arpa   0
    9                                                                 HPE8AA67   0
    10                                                                  ISATAP   0
              Q1   Median      Mean      Q3      Max
    1  0.0000000 0.000000  2.941409  0.5100 50387.76
    2  0.0000000 0.000000  8.187012  1.0000 50364.83
    3  0.0000000 1.000000  8.607446  3.0100 50963.63
    4  0.3699999 1.760000  8.665050  4.7225 50924.28
    5  0.0000000 1.000000 10.003054  2.0100 49952.32
    6  0.1499999 0.500000 11.244317  1.5000 52723.50
    7  0.7500000 0.750000 12.608225  1.1100 50049.11
    8  2.0899999 4.000000 16.006259 20.0900 49679.81
    9  0.7500000 0.750000 16.608906 25.4900 50044.43
    10 0.7500000 0.759999 17.463671  1.0500 51997.79

9\. Поиск IP-адресов с периодическими запросами на один домен (из топ-10
доменов)

``` r
top_domains <- top_10_domains$query
periodic_analysis <- data.frame(
  source_ip = character(),
  domain = character(),
  request_count = integer(),
  avg_interval = numeric(),
  std_dev = numeric(),
  is_periodic = logical()
)
for(domain in top_domains) {
  domain_data <- dns_data_clean[dns_data_clean$query == domain, ]
  domain_data <- domain_data[!is.na(domain_data$timestamp), ]
  unique_ips <- unique(domain_data$source_ip)
  for(ip in unique_ips) {
    ip_data <- domain_data[domain_data$source_ip == ip, ]
    if(nrow(ip_data) >= 5) {
      ip_data <- ip_data[order(ip_data$timestamp), ]
      timestamps <- as.numeric(ip_data$timestamp)
      intervals <- diff(timestamps)
      avg_interval <- mean(intervals)
      std_dev <- sd(intervals)
      is_periodic <- std_dev < avg_interval * 0.5
      periodic_analysis <- rbind(periodic_analysis, data.frame(
        source_ip = ip,
        domain = domain,
        request_count = nrow(ip_data),
        avg_interval = avg_interval,
        std_dev = std_dev,
        is_periodic = is_periodic
      ))
    }
  }
}
suspicious_ips <- periodic_analysis[periodic_analysis$is_periodic == TRUE, ] %>%
  as_tibble()
suspicious_ips
```

    # A tibble: 9 × 6
      source_ip       domain          request_count avg_interval std_dev is_periodic
      <chr>           <chr>                   <int>        <dbl>   <dbl> <lgl>      
    1 192.168.25.25   "safebrowsing.…             8       16.2   0.520   TRUE       
    2 192.168.24.25   "safebrowsing.…             8       16.2   0.165   TRUE       
    3 192.168.21.25   "safebrowsing.…             7       14.3   4.60    TRUE       
    4 192.168.25.25   "*\\x00\\x00\\…             9        1.51  0.00641 TRUE       
    5 192.168.202.120 "WPAD"                     14        0.656 0.296   TRUE       
    6 192.168.202.49  "ISATAP"                   90        0.767 0.121   TRUE       
    7 192.168.0.3     "ISATAP"                  108        0.874 0.313   TRUE       
    8 192.168.202.146 "ISATAP"                    6        0.754 0.0270  TRUE       
    9 192.168.202.147 "ISATAP"                   33        0.862 0.147   TRUE       

10. Определение местоположения и провайдера для топ-10 доменов через их
IP-адреса

``` r
top_domains <- top_10_domains$query
domain_ip_mapping <- data.frame(
  domain = character(),
  ip_address = character()
)
for (domain in top_domains) {
  domain_data <- dns_data_clean[dns_data_clean$query == domain, ]
  if (nrow(domain_data) > 0) {
    ip <- domain_data$destination_ip[1]
    domain_ip_mapping <- rbind(domain_ip_mapping, data.frame(
      domain = domain,
      ip_address = ip
    ))
  }
}
domain_geo_info <- data.frame(
  domain = character(),
  ip_address = character(),
  country = character(),
  city = character(),
  isp = character(),
  stringsAsFactors = FALSE
)
for (i in 1:nrow(domain_ip_mapping)) {
  domain <- domain_ip_mapping$domain[i]
  ip <- domain_ip_mapping$ip_address[i]
  if (grepl("^(10\\.|192\\.168\\.|172\\.(1[6-9]|2[0-9]|3[0-1])\\.)", ip)) {
    domain_geo_info <- rbind(domain_geo_info, data.frame(
      domain = domain,
      ip_address = ip,
      country = "Частный IP",
      city = "Частный IP",
      isp = "Частный IP"
    ))
    next
  }
  url <- paste0("http://ip-api.com/json/", ip)
  response <- GET(url)
  if (status_code(response) == 200) {
    data <- fromJSON(content(response, "text"))
    if (data$status == "success") {
      domain_geo_info <- rbind(domain_geo_info, data.frame(
        domain = domain,
        ip_address = ip,
        country = data$country,
        city = data$city,
        isp = data$isp
      ))%>% as_tibble()
    } else {
      domain_geo_info <- rbind(domain_geo_info, data.frame(
        domain = domain,
        ip_address = ip,
        country = "Не определено",
        city = "Не определено",
        isp = "Не определено"
      ))%>% as_tibble()
    }
  } else {
    domain_geo_info <- rbind(domain_geo_info, data.frame(
      domain = domain,
      ip_address = ip,
      country = "Ошибка API",
      city = "Ошибка API",
      isp = "Ошибка API"
    )) %>% as_tibble()
  }
  Sys.sleep(1)
}
domain_geo_info
```

    # A tibble: 10 × 5
       domain                                         ip_address country city  isp  
       <chr>                                          <chr>      <chr>   <chr> <chr>
     1 "teredo.ipv6.microsoft.com"                    <NA>       Не опр… Не о… Не о…
     2 "tools.google.com"                             <NA>       Не опр… Не о… Не о…
     3 "www.apple.com"                                172.19.1.… Частны… Част… Част…
     4 "time.apple.com"                               <NA>       Не опр… Не о… Не о…
     5 "safebrowsing.clients.google.com"              <NA>       Не опр… Не о… Не о…
     6 "*\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x… 192.168.2… Частны… Част… Част…
     7 "WPAD"                                         192.168.2… Частны… Част… Част…
     8 "44.206.168.192.in-addr.arpa"                  <NA>       Не опр… Не о… Не о…
     9 "HPE8AA67"                                     192.168.2… Частны… Част… Част…
    10 "ISATAP"                                       <NA>       Не опр… Не о… Не о…

## Оценка результата

В рамках практческой работы была исследована подозрительная сетевая
активность во внутренней сети Доброй Организации. Были восстановлены
недостающие метаданные и подготовлены ответы на вопросы.

## Вывод

Таким мобразом в ходе работы мы зекрепили практические навыки
использования языка программирования R для обработки данных, знания
основных функций обработки данных экосистемы tidyverse языка R и навыки
исследования метаданных DNS трафика
