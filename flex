#------------------ Packages ------------------
library(flexdashboard)
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(highcharter)
#------------------ Data ------------------
coronavirus <- read.csv("https://raw.githubusercontent.com/RamiKrispin/coronavirus/master/csv/coronavirus.csv", stringsAsFactors = FALSE)
corona <- coronavirus %>% 
 pivot_wider(names_from = type, values_from = cases)
corona$confirmed[is.na(corona$confirmed)] = 0
corona$death[is.na(corona$death)] = 0
corona$recovered[is.na(corona$recovered)] = 0
coronavirus_Turkey <- corona %>%
  filter(country == "Turkey")
coronavirus_Turkey <- coronavirus_Turkey %>%
  mutate(confirmed_cum = cumsum(confirmed)) %>%
  mutate(confirmed_death = cumsum(death)) %>%
  mutate(confirmed_recovered = cumsum(recovered)) %>%
  mutate(confirmed_active = cumsum(confirmed)-cumsum(death)-cumsum(recovered))
coronavirus_Turkey <- coronavirus_Turkey %>%
  mutate(death_ratio = (death / confirmed)*100)
coronavirus_Turkey <- coronavirus_Turkey %>%
  mutate(confirmed_death_ratio = (confirmed_death / confirmed_cum)*100)
coronavirus_Turkey <- coronavirus_Turkey %>%
  mutate(recovered_ratio = (recovered / confirmed)*100)
coronavirus_Turkey <- coronavirus_Turkey %>%
  mutate(confirmed_recovered_ratio = (confirmed_recovered / confirmed_cum)*100)
corona <- corona %>%
  mutate(confirmed_cum = cumsum(confirmed)) %>%
  mutate(confirmed_death = cumsum(death)) %>%
  mutate(confirmed_recovered = cumsum(recovered)) %>%
  mutate(confirmed_active = cumsum(confirmed)-cumsum(death)-cumsum(recovered))
coronavirus_US <- corona %>%
  filter(country == "US")
coronavirus_US <- coronavirus_US %>%
  mutate(confirmed_cum = cumsum(confirmed))
coronavirus_Germany <- corona %>%
  filter(country == "Germany") %>%
  mutate(confirmed_cum = cumsum(confirmed))
coronavirus_Japan <- corona %>%
  filter(country == "Japan") %>%
  mutate(confirmed_cum = cumsum(confirmed))
coronavirus_Brazil <- corona %>%
  filter(country == "Brazil") %>%
  mutate(confirmed_cum = cumsum(confirmed))
tophastgr <- coronavirus_Turkey %>% 
  left_join(coronavirus_US, by = "date") %>%
  left_join(coronavirus_Germany, by = "date") %>%
  left_join(coronavirus_Japan, by = "date") %>%
  left_join(coronavirus_Brazil, by = "date")
```


Dünya
=======================================================================

Column {data-width=430}
-----------------------------------------------------------------------

### Dünyada En Yüksek Toplam Vaka Sayısına Sahip 10 Ülke
```{r}
ry<-corona %>% group_by(country) %>%
     summarise(confirmed = sum(confirmed)) %>%
     arrange(confirmed) %>%
     arrange(desc(confirmed))
ry%>%
  arrange(desc(confirmed))%>%
  head(10) %>% 
  hchart(type = "bar", hcaes(x = country, y = confirmed)) %>%
  hc_yAxis(text = "Ülke") %>% 
  hc_xAxis(text = "Toplam Hasta Sayısı")
```

### Covid 19 Vakalarının Dünya'daki Dağılımı
```{r}
coronavirus %>% 
  group_by(type, date) %>%
  summarise(total_cases = sum(cases)) %>%
  pivot_wider(names_from = type, values_from = total_cases) %>%
  arrange(date) %>%
  mutate(confirmed_total = cumsum(confirmed),
                recovered_total = cumsum(recovered),
                death_total = cumsum(death)) %>%
  plot_ly(x = ~ date,
                  y = ~ confirmed_total,
                  name = 'Toplam Vaka Sayısı', 
                  fillcolor = '#7ec0ee',
                  type = 'scatter',
                  mode = 'none', 
                  stackgroup = 'one') %>%
  add_trace(y = ~ death_total, 
             name = "Toplam Vefat Sayısı",
             fillcolor = '#E41317') %>%
  add_trace(y = ~recovered_total, 
            name = 'Toplam İyileşen Sayısı', 
            fillcolor = 'forestgreen') %>%
  layout(legend = list(x = 0.1, y = 0.9),
         yaxis = list(title = "Toplam Vaka Sayısı"),
         xaxis = list(title = "Tarih"))
```



Column {data-width=430}
-----------------------------------------------------------------------

### 5 Ülke'nin Karşılaştırılması
```{r}
plot_ly(data = tophastgr) %>%
  add_lines(x = ~ date,
            y = ~ confirmed_cum,
            name = "Brazil",  line = list(width = 2)) %>%
  add_lines(x = ~ date,
            y = ~ confirmed_cum.y.y,
                    name = "Japan",  line = list(width = 2)) %>%
  add_lines(x = ~ date,
            y = ~ confirmed_cum.x.x,
            name = "Germany",  line = list(width = 2)) %>%
  add_lines(x = ~ date,
            y = ~ confirmed_cum.y,
            name = "US",  line = list(width = 2)) %>%
  add_lines(x = ~ date,
            y = ~ confirmed_cum.x,
            name = "Turkey",  line = list(width = 2)) %>%
  layout(yaxis = list(title = "Toplam Vaka Sayısı",type = "log"),
         xaxis = list(title = "Tarih"),
         legend = list(x = 0.7, y = 0.3))
```

### Dünyada En Yüksek Toplam Vaka Sayısına Sahip 10 Ülke
```{r}
ry %>%
  arrange(desc(confirmed))%>%
  head(10) %>%
  plot_ly(labels = ~country, values = ~confirmed,
          textinfo="label+percent",
          type = 'pie')
```



Column {data-width=195}
-----------------------------------------------------------------------

### Toplam Vaka Sayısı {.value-box}
```{r}
kutu <- (corona$confirmed_cum[length(corona$confirmed_cum[!is.na(corona$confirmed_cum)])])
valueBox(kutu, icon = "fa-plus-square",color="#cd3333")
```

### Toplam İyileşen Sayısı {.value-box}
```{r}
kutu <- (corona$confirmed_recovered[length(corona$confirmed_recovered[!is.na(corona$confirmed_recovered)])])
valueBox(kutu, icon = "fa-heartbeat",color="forestgreen")
```

### Toplam Vefat Sayısı {.value-box}
```{r}
kutu <- (corona$confirmed_death[length(corona$confirmed_death[!is.na(corona$confirmed_death)])])
valueBox(kutu, icon = "fa-frown-o",color="black")
```

### Aktif Vaka Sayısı {.value-box}
```{r}
kutu <- (corona$confirmed_active[length(corona$confirmed_active[!is.na(corona$confirmed_active)])])
valueBox(kutu, icon = "fa-hospital-o",color="#d8bfd8")
```

Dünya+
=======================================================================

Column {.tabset}
-----------------------------------------------------------------------
### Karşılaştırma
```{r}
plot_ly(data = tophastgr) %>%
  add_lines(x = ~ date,
            y = ~ confirmed_cum,
            name = "Brazil",  line = list(width = 2)) %>%
  add_lines(x = ~ date,
            y = ~ confirmed_cum.y.y,
                    name = "Japan",  line = list(width = 2)) %>%
  add_lines(x = ~ date,
            y = ~ confirmed_cum.x.x,
            name = "Germany",  line = list(width = 2)) %>%
  add_lines(x = ~ date,
            y = ~ confirmed_cum.y,
            name = "US",  line = list(width = 2)) %>%
  add_lines(x = ~ date,
            y = ~ confirmed_cum.x,
            name = "Turkey",  line = list(width = 2)) %>%
  layout(yaxis = list(title = "Toplam Vaka Sayısı",type = "log"),
         xaxis = list(title = "Tarih"),
         legend = list(x = 0.7, y = 0.3))
```


Türkiye
=======================================================================

Column {.tabset}
-----------------------------------------------------------------------
### Günlük Vaka Sayısı Grafiği
```{r}
plot_ly(data = coronavirus_Turkey) %>%
  add_lines(x = ~ date,
            y = ~ confirmed,
            name = "Türkiye",  line = list(width = 3)) %>%
  layout(yaxis = list(title = "Günlük Vaka Sayısı",type = "scatter"),
         xaxis = list(title = "Tarih"),
         legend = list(x = 0.7, y = 0.3))
```

### Toplam Vaka Sayısı Grafiği
```{r}
plot_ly(data = coronavirus_Turkey) %>%
  add_lines(x = ~ date,
            y = ~ confirmed_cum,
            name = "Türkiye",  line = list(width = 3)) %>%
  layout(yaxis = list(title = "Toplam Vaka Sayısı",type = "log"),
         xaxis = list(title = "Tarih"),
         legend = list(x = 0.7, y = 0.3))
```

### Günlük Vefat Sayısı Grafiği
```{r}
plot_ly(data = coronavirus_Turkey) %>%
  add_lines(x = ~ date,
            y = ~ death,
            name = "Türkiye",  line = list(width = 3)) %>%
  layout(title = "Günlük Vefat Sayısı Grafiği",
         yaxis = list(title = "Günlük Vefat Sayısı",type = "scatter"),
         xaxis = list(title = "Tarih"),
         legend = list(x = 0.7, y = 0.3))
```

### Toplam Vefat Sayısı Grafiği
```{r}
plot_ly(data = coronavirus_Turkey) %>%
  add_lines(x = ~ date,
            y = ~ confirmed_death,
            name = "Türkiye",  line = list(width = 3)) %>%
  layout(title = "Toplam Vefat Sayısı Grafiği",
         yaxis = list(title = "Toplam Vefat Sayısı",type = "scatter"),
         xaxis = list(title = "Tarih"),
         legend = list(x = 0.7, y = 0.3))
```

### Günlük İyileşen Sayısı Grafiği
```{r}
plot_ly(data = coronavirus_Turkey) %>%
  add_lines(x = ~ date,
            y = ~ recovered,
            name = "Türkiye",  line = list(width = 3)) %>%
  layout(title = "Günlük İyileşen Sayısı Grafiği",
         yaxis = list(title = "Günlük İyileşen Sayısı",type = "scatter"),
         xaxis = list(title = "Tarih"),
         legend = list(x = 0.7, y = 0.3))
```

### Toplam İyileşen Sayısı Grafiği
```{r}
plot_ly(data = coronavirus_Turkey) %>%
  add_lines(x = ~ date,
            y = ~ confirmed_recovered,
            name = "Türkiye",  line = list(width = 3)) %>%
  layout(title = "Toplam İyileşen Sayısı Grafiği",
         yaxis = list(title = "Toplam İyileşen Sayısı Grafiği",type = "scatter"),
         xaxis = list(title = "Tarih"),
         legend = list(x = 0.7, y = 0.3))
```

### Aktif Vaka Sayısı Grafiği
```{r}
plot_ly(data = coronavirus_Turkey) %>%
  add_lines(x = ~ date,
            y = ~ confirmed_active,
            name = "Türkiye",  line = list(width = 3)) %>%
  layout(title = "Aktif Vaka Sayısı Grafiği",
         yaxis = list(title = "Aktif Vaka Sayısı",type = "scatter"),
         xaxis = list(title = "Tarih"),
         legend = list(x = 0.7, y = 0.3))
```


### Covid 19 Vakalarının Türkiye'deki Dağılımı
```{r}
coronavirus_Turkey %>%
  plot_ly(x = ~ date,
          y = ~ confirmed_cum,
          name = 'Toplam Vaka Sayısı', 
          fillcolor = '#7ec0ee',
          type = 'scatter',
          mode = 'none', 
          stackgroup = 'one') %>%
  add_trace(y = ~ confirmed_death, 
            name = "Toplam Vefat Sayısı",
            fillcolor = '#E41317') %>%
  add_trace(y = ~confirmed_recovered, 
            name = 'Toplam İyileşen Sayısı', 
            fillcolor = 'forestgreen') %>%
  layout(legend = list(x = 0.1, y = 0.9),
         yaxis = list(title = "Toplam Vaka Sayısı"),
         xaxis = list(title = "Tarih"))
```


Column {data-width=100}
-----------------------------------------------------------------------
### Toplam Vaka Sayısı {.value-box}
```{r}
kutu <- coronavirus_Turkey$confirmed_cum[length(coronavirus_Turkey$confirmed_cum)]
valueBox(kutu, icon = "fa-plus-square",color="#cd3333")
```

### Bugünkü Vaka Sayısı {.value-box}
```{r}
kutu <- coronavirus_Turkey$confirmed[length(coronavirus_Turkey$confirmed)]
valueBox(kutu, icon = "fa-plus-square",color="#cd3333")
```

### Dünkü Vaka Sayısı {.value-box}
```{r}
kutu <-coronavirus_Turkey$confirmed[length(coronavirus_Turkey$confirmed)-1]
valueBox(kutu, icon = "fa-plus-square",color="#cd3333")
```


### Toplam Vefat Sayısı {.value-box}
```{r}
kutu <- coronavirus_Turkey$confirmed_death[length(coronavirus_Turkey$confirmed_death)]
valueBox(kutu, icon = "fa-frown-o",color="black")
```

### Aktif Vaka Sayısı {.value-box}
```{r}
kutu <- coronavirus_Turkey$confirmed_active[length(coronavirus_Turkey$confirmed_active)]
valueBox(kutu, icon = "fa-hospital-o",color="#d8bfd8")
```



Column {data-width=100}
-----------------------------------------------------------------------

### Toplam İyileşen Sayısı {.value-box}
```{r}
kutu <- coronavirus_Turkey$confirmed_recovered[length(coronavirus_Turkey$confirmed_recovered)]
valueBox(kutu, icon = "fa-heartbeat",color="forestgreen")
```

### Bugünkü İyileşen Sayısı {.value-box}
```{r}
kutu <- coronavirus_Turkey$confirmed[length(coronavirus_Turkey$confirmed)]
valueBox(kutu, icon = "fa-heartbeat",color="forestgreen")
```

### Dünkü İyileşen Sayısı {.value-box}
```{r}
kutu <- coronavirus_Turkey$confirmed[length(coronavirus_Turkey$confirmed)-1]
valueBox(kutu, icon = "fa-heartbeat",color="forestgreen")
```

### Bugünkü Vefat Sayısı {.value-box}
```{r}
kutu <- coronavirus_Turkey$death[length(coronavirus_Turkey$death)]
valueBox(kutu, icon = "fa-frown-o",color="black")
```

### Dünkü Vefat Sayısı {.value-box}
```{r}
kutu <- coronavirus_Turkey$death[length(coronavirus_Turkey$death)-1]
valueBox(kutu, icon = "fa-frown-o",color="black")
```


Türkiye (Oransal)
=======================================================================

Column {data-width=100}
-----------------------------------------------------------------------
### Bugünkü Vaka Sayısı {.value-box}
```{r}
kutu <- coronavirus_Turkey$confirmed[length(coronavirus_Turkey$confirmed)]
valueBox(kutu, icon = "fa-plus-square",color="#cd3333")
```

### Bugünkü İyileşen Sayısı {.value-box}
```{r}
kutu <- coronavirus_Turkey$confirmed[length(coronavirus_Turkey$confirmed)]
valueBox(kutu, icon = "fa-heartbeat",color="forestgreen")
```

### Bugünkü Vefat Sayısı {.value-box}
```{r}
kutu <- coronavirus_Turkey$death[length(coronavirus_Turkey$death)]
valueBox(kutu, icon = "fa-frown-o",color="black")
```


Column {.tabset}
-----------------------------------------------------------------------

### Günlük İyileşen Oranı Grafiği
```{r}
plot_ly(data = coronavirus_Turkey) %>%
  add_lines(x = ~ date,
            y = ~ recovered_ratio,
            name = "Türkiye",  line = list(width = 3)) %>%
  layout(title = "Günlük İyileşen Oranı Grafiği",
         yaxis = list(title = "Günlük İyileşen Oranı",type = "scatter"),
         xaxis = list(title = "Tarih"),
         legend = list(x = 0.7, y = 0.3))
```

### Toplam İyileşen Oranı Grafiği
```{r}
plot_ly(data = coronavirus_Turkey) %>%
  add_lines(x = ~ date,
            y = ~ confirmed_recovered_ratio,
            name = "Türkiye",  line = list(width = 3)) %>%
  layout(title = "Toplam İyileşen Oranı Grafiği",
         yaxis = list(title = "Toplam İyileşen Oranı",type = "scatter"),
         xaxis = list(title = "Tarih"),
         legend = list(x = 0.7, y = 0.3))
```

### Günlük Vefat Oranı Grafiği
```{r}
plot_ly(data = coronavirus_Turkey) %>%
  add_lines(x = ~ date,
            y = ~ death_ratio,
            name = "Türkiye",  line = list(width = 3)) %>%
  layout(title = "Günlük Vefat Oranı Grafiği",
         yaxis = list(title = "Günlük Vefat Oranı",type = "scatter"),
         xaxis = list(title = "Tarih"),
         legend = list(x = 0.7, y = 0.3))
```

### Toplam Vefat Oranı Grafiği
```{r}
plot_ly(data = coronavirus_Turkey) %>%
  add_lines(x = ~ date,
            y = ~ confirmed_death_ratio,
            name = "Türkiye",  line = list(width = 3)) %>%
  layout(title = "Toplam Vefat Oranı Grafiği",
         yaxis = list(title = "Toplam Vefat Oranı",type = "scatter"),
         xaxis = list(title = "Tarih"),
         legend = list(x = 0.7, y = 0.3))
```


Column {data-width=100}
-----------------------------------------------------------------------
### Toplam Vaka Sayısı {.value-box}
```{r}
kutu <- coronavirus_Turkey$confirmed_cum[length(coronavirus_Turkey$confirmed_cum)]
valueBox(kutu, icon = "fa-plus-square",color="#cd3333")
```

### Toplam İyileşen Sayısı {.value-box}
```{r}
kutu <- coronavirus_Turkey$confirmed_recovered[length(coronavirus_Turkey$confirmed_recovered)]
valueBox(kutu, icon = "fa-heartbeat",color="forestgreen")
```

### Toplam Vefat Sayısı {.value-box}
```{r}
kutu <- coronavirus_Turkey$confirmed_death[length(coronavirus_Turkey$confirmed_death)]
valueBox(kutu, icon = "fa-frown-o",color="black")
```


