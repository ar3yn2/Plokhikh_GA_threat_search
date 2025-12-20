# 3ПР Основы обработки данных с помощью R и Dplyr
gleb.plokhikh@yandex.ru

## Цель работы

1.  Развить практические навыки использования языка программирования R
    для обработки данных
2.  Закрепить знания базовых типов данных языка R
3.  Развить практические навыки использования функций обработки данных
    пакета dplyr – функции select(), filter(), mutate(), arrange(),
    group_by()

## Исходные данные

1.  Rstudio Desktop;
2.  Интерпретатор языка R 4.1;
3.  Программный пакет `dplyr` и `knitr`.

## План

Проанализировать встроенный в пакет dplyr набор данных `nycflights13` с
помощью языка R и ответить на вопросы.

## Решение:

1\. Загрузка библиотек

``` r
library(dplyr)
```


    Присоединяю пакет: 'dplyr'

    Следующие объекты скрыты от 'package:stats':

        filter, lag

    Следующие объекты скрыты от 'package:base':

        intersect, setdiff, setequal, union

``` r
library(knitr)
library(nycflights13)
```

2\. Сколько встроенных в пакет `nycflights13` датафреймов?

``` r
data(package = "nycflights13")$results[, "Item"]
```

    [1] "airlines" "airports" "flights"  "planes"   "weather" 

3\. Сколько строк и столбцов в каждом датафрейме?

``` r
tibble(
dataset = c("airlines", "airports", "flights", "planes", "weather"),
rows = c(nrow(airlines), nrow(airports), nrow(flights), nrow(planes), nrow(weather)),
cols = c(ncol(airlines), ncol(airports), ncol(flights), ncol(planes), ncol(weather))
) |> knitr::kable()
```

<table>
<thead>
<tr>
<th style="text-align: left;">dataset</th>
<th style="text-align: right;">rows</th>
<th style="text-align: right;">cols</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: left;">airlines</td>
<td style="text-align: right;">16</td>
<td style="text-align: right;">2</td>
</tr>
<tr>
<td style="text-align: left;">airports</td>
<td style="text-align: right;">1458</td>
<td style="text-align: right;">8</td>
</tr>
<tr>
<td style="text-align: left;">flights</td>
<td style="text-align: right;">336776</td>
<td style="text-align: right;">19</td>
</tr>
<tr>
<td style="text-align: left;">planes</td>
<td style="text-align: right;">3322</td>
<td style="text-align: right;">9</td>
</tr>
<tr>
<td style="text-align: left;">weather</td>
<td style="text-align: right;">26115</td>
<td style="text-align: right;">15</td>
</tr>
</tbody>
</table>

4\. Просмотр структуры данных

``` r
glimpse(flights)
```

    Rows: 336,776
    Columns: 19
    $ year           <int> 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2…
    $ month          <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
    $ day            <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
    $ dep_time       <int> 517, 533, 542, 544, 554, 554, 555, 557, 557, 558, 558, …
    $ sched_dep_time <int> 515, 529, 540, 545, 600, 558, 600, 600, 600, 600, 600, …
    $ dep_delay      <dbl> 2, 4, 2, -1, -6, -4, -5, -3, -3, -2, -2, -2, -2, -2, -1…
    $ arr_time       <int> 830, 850, 923, 1004, 812, 740, 913, 709, 838, 753, 849,…
    $ sched_arr_time <int> 819, 830, 850, 1022, 837, 728, 854, 723, 846, 745, 851,…
    $ arr_delay      <dbl> 11, 20, 33, -18, -25, 12, 19, -14, -8, 8, -2, -3, 7, -1…
    $ carrier        <chr> "UA", "UA", "AA", "B6", "DL", "UA", "B6", "EV", "B6", "…
    $ flight         <int> 1545, 1714, 1141, 725, 461, 1696, 507, 5708, 79, 301, 4…
    $ tailnum        <chr> "N14228", "N24211", "N619AA", "N804JB", "N668DN", "N394…
    $ origin         <chr> "EWR", "LGA", "JFK", "JFK", "LGA", "EWR", "EWR", "LGA",…
    $ dest           <chr> "IAH", "IAH", "MIA", "BQN", "ATL", "ORD", "FLL", "IAD",…
    $ air_time       <dbl> 227, 227, 160, 183, 116, 150, 158, 53, 140, 138, 149, 1…
    $ distance       <dbl> 1400, 1416, 1089, 1576, 762, 719, 1065, 229, 944, 733, …
    $ hour           <dbl> 5, 5, 5, 5, 6, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5, 6, 6, 6…
    $ minute         <dbl> 15, 29, 40, 45, 0, 58, 0, 0, 0, 0, 0, 0, 0, 0, 0, 59, 0…
    $ time_hour      <dttm> 2013-01-01 05:00:00, 2013-01-01 05:00:00, 2013-01-01 0…

``` r
glimpse(weather)
```

    Rows: 26,115
    Columns: 15
    $ origin     <chr> "EWR", "EWR", "EWR", "EWR", "EWR", "EWR", "EWR", "EWR", "EW…
    $ year       <int> 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013,…
    $ month      <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
    $ day        <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
    $ hour       <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 14, 15, 16, 17, 18, …
    $ temp       <dbl> 39.02, 39.02, 39.02, 39.92, 39.02, 37.94, 39.02, 39.92, 39.…
    $ dewp       <dbl> 26.06, 26.96, 28.04, 28.04, 28.04, 28.04, 28.04, 28.04, 28.…
    $ humid      <dbl> 59.37, 61.63, 64.43, 62.21, 64.43, 67.21, 64.43, 62.21, 62.…
    $ wind_dir   <dbl> 270, 250, 240, 250, 260, 240, 240, 250, 260, 260, 260, 330,…
    $ wind_speed <dbl> 10.35702, 8.05546, 11.50780, 12.65858, 12.65858, 11.50780, …
    $ wind_gust  <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 20.…
    $ precip     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ pressure   <dbl> 1012.0, 1012.3, 1012.5, 1012.2, 1011.9, 1012.4, 1012.2, 101…
    $ visib      <dbl> 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10,…
    $ time_hour  <dttm> 2013-01-01 01:00:00, 2013-01-01 02:00:00, 2013-01-01 03:00…

5\. Сколько компаний-перевозчиков (carrier) учитывают эти наборы данных
(представлено в наборах данных)?

``` r
airlines |> distinct(carrier) |> count() |> knitr::kable()
```

<table>
<thead>
<tr>
<th style="text-align: right;">n</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: right;">16</td>
</tr>
</tbody>
</table>

6\. Сколько рейсов принял аэропорт John F Kennedy Intl в мае?

``` r
flights |>
filter(origin == "JFK", month == 5) |>
summarise(total_flights = n()) |>
knitr::kable()
```

<table>
<thead>
<tr>
<th style="text-align: right;">total_flights</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: right;">9397</td>
</tr>
</tbody>
</table>

7\. Какой самый северный аэропорт?

``` r
airports |>
slice_max(lat, n = 1) |>
select(name, lat) |>
knitr::kable()
```

<table>
<thead>
<tr>
<th style="text-align: left;">name</th>
<th style="text-align: right;">lat</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: left;">Dillant Hopkins Airport</td>
<td style="text-align: right;">72.27083</td>
</tr>
</tbody>
</table>

8\. Аэропорт с наибольшей высотой над уровнем моря

``` r
airports |>
slice_max(alt, n = 1) |>
select(name, alt) |>
knitr::kable()
```

<table>
<thead>
<tr>
<th style="text-align: left;">name</th>
<th style="text-align: right;">alt</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: left;">Telluride</td>
<td style="text-align: right;">9078</td>
</tr>
</tbody>
</table>

9\. Какие бортовые номера у самых старых самолетов?

``` r
planes |>
filter(!is.na(year)) |>
arrange(year) |>
select(tailnum, year) |>
slice_head(n = 10) |>
knitr::kable()
```

<table>
<thead>
<tr>
<th style="text-align: left;">tailnum</th>
<th style="text-align: right;">year</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: left;">N381AA</td>
<td style="text-align: right;">1956</td>
</tr>
<tr>
<td style="text-align: left;">N201AA</td>
<td style="text-align: right;">1959</td>
</tr>
<tr>
<td style="text-align: left;">N567AA</td>
<td style="text-align: right;">1959</td>
</tr>
<tr>
<td style="text-align: left;">N378AA</td>
<td style="text-align: right;">1963</td>
</tr>
<tr>
<td style="text-align: left;">N575AA</td>
<td style="text-align: right;">1963</td>
</tr>
<tr>
<td style="text-align: left;">N14629</td>
<td style="text-align: right;">1965</td>
</tr>
<tr>
<td style="text-align: left;">N615AA</td>
<td style="text-align: right;">1967</td>
</tr>
<tr>
<td style="text-align: left;">N425AA</td>
<td style="text-align: right;">1968</td>
</tr>
<tr>
<td style="text-align: left;">N383AA</td>
<td style="text-align: right;">1972</td>
</tr>
<tr>
<td style="text-align: left;">N364AA</td>
<td style="text-align: right;">1973</td>
</tr>
</tbody>
</table>

10\. Какая средняя температура воздуха была в сентябре в аэропорту John
F Kennedy Intl (в градусах Цельсия).

``` r
weather %>% filter(origin=='JFK', month==9) %>% summarize(mean_temp = (mean(temp, na.rm = TRUE)-32)*5/9) |> knitr::kable(format='markdown')
```

<table>
<thead>
<tr>
<th style="text-align: right;">mean_temp</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: right;">19.38764</td>
</tr>
</tbody>
</table>

11\. Авиакомпания с наибольшим числом вылетов в июне

``` r
flights |>
filter(month == 6) |>
count(carrier, sort = TRUE) |>
left_join(airlines, by = "carrier") |>
slice(1) |>
select(name, n) |>
knitr::kable()
```

<table>
<thead>
<tr>
<th style="text-align: left;">name</th>
<th style="text-align: right;">n</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: left;">United Air Lines Inc.</td>
<td style="text-align: right;">4975</td>
</tr>
</tbody>
</table>

12\. Авиакомпания с наибольшим числом задержек в 2013 году

``` r
flights |>
filter(year == 2013, arr_delay > 0) |>
count(carrier, sort = TRUE) |>
left_join(airlines, by = "carrier") |>
slice(1) |>
select(name, n) |>
knitr::kable()
```

<table>
<thead>
<tr>
<th style="text-align: left;">name</th>
<th style="text-align: right;">n</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: left;">ExpressJet Airlines Inc.</td>
<td style="text-align: right;">24484</td>
</tr>
</tbody>
</table>

## Оценка результата

В результате лабораторной работы мы проанализировали встроенный в пакет
dplyr набор данных `nycflights13` с помощью языка R.

## Вывод

Таким образом, мы развили практические навыки использования языка
программирования R для обработки данных, развили практические навыки
использования функций обработки данных пакета dplyr – функции select(),
filter(), mutate(), arrange(), group_by().
