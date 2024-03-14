---
title: "Final_Project"
author: "Netra Patel"
output: 
  html_document: 
    keep_md: true
date: "2024-03-01"
keep_md: yes
---




```r
library("ggplot2")
library("naniar")
library("tidyverse")
```

```
## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
## ✔ dplyr     1.1.4     ✔ readr     2.1.5
## ✔ forcats   1.0.0     ✔ stringr   1.5.1
## ✔ lubridate 1.9.3     ✔ tibble    3.2.1
## ✔ purrr     1.0.2     ✔ tidyr     1.3.1
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

```r
library("janitor")
```

```
## 
## Attaching package: 'janitor'
## 
## The following objects are masked from 'package:stats':
## 
##     chisq.test, fisher.test
```

# Loading & cleaning the data

```r
crime <- read_csv("data/Crime_Data_from_2020_to_Present.csv", na = c("NA","", "-"))%>%
  clean_names()
```

```
## Warning: One or more parsing issues, call `problems()` on your data frame for details,
## e.g.:
##   dat <- vroom(...)
##   problems(dat)
```

```
## Rows: 901357 Columns: 28
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (16): Date Rptd, DATE OCC, TIME OCC, AREA, AREA NAME, Rpt Dist No, Crm C...
## dbl (11): DR_NO, Part 1-2, Crm Cd, Vict Age, Premis Cd, Weapon Used Cd, Crm ...
## lgl  (1): Crm Cd 4
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```


```r
crime <- crime%>%
  separate(date_rptd, into = c("month", "day", "year")) #It wouldn't let me knit because the data frame was already changed 
```

```
## Warning: Expected 3 pieces. Additional pieces discarded in 901357 rows [1, 2, 3, 4, 5,
## 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, ...].
```


```r
naniar::miss_var_summary(crime)
```

```
## # A tibble: 30 × 3
##    variable       n_miss pct_miss
##    <chr>           <int>    <dbl>
##  1 crm_cd_4       901357 100     
##  2 crm_cd_3       899144  99.8   
##  3 crm_cd_2       835674  92.7   
##  4 cross_street   759302  84.2   
##  5 weapon_used_cd 589089  65.4   
##  6 weapon_desc    589089  65.4   
##  7 mocodes        125590  13.9   
##  8 vict_descent   119401  13.2   
##  9 vict_sex       119392  13.2   
## 10 premis_desc       550   0.0610
## # ℹ 20 more rows
```

# Crimes most common in LA

```r
crime%>%
  group_by(crm_cd_desc)%>%
  summarise(common = n())%>%
  top_n(10, common)%>%
  ggplot(aes(x = crm_cd_desc, y = common, fill = common))+
  geom_col(color = "black", alpha = 0.7)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme_minimal()+
  coord_flip()+
  labs(title = "Top 10 crimes committed", x = "Number of crimes", y = "Types of crime")
```

![](slides_2_to_8_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


# Vehicles stolen

```r
crime%>%
  filter(crm_cd_desc == "VEHICLE - STOLEN")%>%
  group_by(year, crm_cd_desc)%>%
  summarise(tot = n(), .groups='keep')%>%
  ggplot(aes(x = as_factor(year), y = tot, group=crm_cd_desc, fill = crm_cd_desc))+
  geom_col(color = "black", alpha = 0.7, fill = "red4")+
  labs(title = "Vehicles stolen from 2020-2024",
       x = "years",
       y = "Crime total")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        plot.title = element_text(size = 16, face="bold"))+
  geom_text(aes(label = tot), vjust = -0.1, size = 3, color = "black")+
  theme_minimal()
```

![](slides_2_to_8_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

# Battery

```r
crime%>%
  filter(crm_cd_desc == "BATTERY - SIMPLE ASSAULT")%>%
  group_by(year, crm_cd_desc)%>%
  summarise(tot = n(), .groups='keep')%>%
  ggplot(aes(x = as_factor(year), y = tot, group=crm_cd_desc, color = crm_cd_desc))+
  geom_line(color = "darkgreen")+
  geom_point(shape = 1, size = 3, color = "black")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme_minimal()+
  labs(title = "Battery assault from 2020-2024",
       x = "years",
       y = "Crime total")
```

![](slides_2_to_8_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

# Theft 

```r
crime%>%
  filter(crm_cd_desc == "THEFT PLAIN - PETTY ($950 & UNDER)")%>%
  mutate(year=as.factor(year)) %>% 
  ggplot(aes(x=year, group=crm_cd_desc, color = crm_cd_desc))+
  geom_density(fill = "orange", alpha = 0.7)+
  labs(title = "Theft($950 and under)")+
  theme_minimal()
```

![](slides_2_to_8_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

# Id theft


```r
crime%>%
  filter(crm_cd_desc == "THEFT OF IDENTITY")%>%
  group_by(year, crm_cd_desc)%>%
  summarise(tot = n(), .groups='keep')%>%
  ggplot(aes(x = as_factor(year), y = tot, group=crm_cd_desc, color = crm_cd_desc))+
  geom_col(color = "black", fill = "yellow3")+
  labs(title = "Identity Theft",
       x = "Total crime")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        plot.title = element_text(size = 16, face="bold"))+
  geom_text(aes(label = tot), vjust = -0.1, size = 3, color = "black")+
  theme_minimal()
```

![](slides_2_to_8_files/figure-html/unnamed-chunk-9-1.png)<!-- -->
# Motorcycle

```r
crime%>%
  filter(crm_cd_desc == "THEFT FROM MOTOR VEHICLE - PETTY ($950 & UNDER)")%>%
  group_by(year, crm_cd_desc)%>%
  summarise(tot = n(), .groups='keep')%>%
  ggplot(aes(x = as_factor(year), y = tot, group=crm_cd_desc, fill = crm_cd_desc))+
  geom_point(color = "orange")+
  labs(title = "Motor Vehicle Theft",
       x = "years",
       y = "Crime total")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        plot.title = element_text(size = 15, face="bold"))+
  geom_text(aes(label = tot), vjust = -0.1, size = 3, color = "black")+
  theme_minimal()
```

![](slides_2_to_8_files/figure-html/unnamed-chunk-10-1.png)<!-- -->


# What sex committed most crimes

```r
crime %>%
  filter(vict_sex!="-" & vict_sex!="")%>%
  filter(vict_sex!="H")%>%
  group_by(vict_sex, year) %>%
  summarise(tot = n(), .groups = 'keep') %>%
  ggplot(aes(x = year, y = tot, fill = vict_sex)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7, na.rm = T) +
  facet_grid(.~vict_sex) +
  labs(title = "Most crimes committed by sex",
       x = "years",
       y = "Crime total")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        plot.title = element_text(size = 15, face="bold"))+
  geom_text(aes(label = tot), vjust = -0.1, size = 3, color = "black")+
  theme_minimal()
```

![](slides_2_to_8_files/figure-html/unnamed-chunk-11-1.png)<!-- -->



# shiny app 

```r
library(shiny)
library(shinydashboard)
```

```
## 
## Attaching package: 'shinydashboard'
```

```
## The following object is masked from 'package:graphics':
## 
##     box
```

```r
ui <- dashboardPage(
  dashboardHeader(title = "Crime Rates"),
  dashboardSidebar(disable=T),
  dashboardBody(
    fluidRow(
      box(width = 4,
          selectInput("y", "Select Crime", choices = unique(crime$crm_cd_desc)),
          selectInput("x", "Select Premises", choices = unique(crime$premis_desc))
          ), 
      box(width = 6,
          plotOutput("plot", width = "600px", height = "400px")
          )
      ) 
    ) 
  )
server <- function(input, output, session) { 
  session$onSessionEnded(stopApp)
  
  output$plot <- renderPlot({
    crime %>% 
      
      filter(crm_cd_desc !="NA") %>%
      filter(premis_desc !="NA")%>%
      filter(crm_cd_desc == input$y & premis_desc == input$x)%>%
      
      
      group_by(year, crm_cd_desc, premis_desc)%>%
      
      
      summarise(tot = n(), .groups='keep')%>%
      ggplot(aes(x = year, y = tot, fill = tot)) +
      geom_col(color = "black", alpha = 0.6, fill = "darkgreen")+
      labs(title = "Number of crime",
       x = "years",
       y = "Crime total")+
      geom_text(aes(label = tot), vjust = -0.1, size = 3, color = "black")+
      theme_classic()
    })
}
  
shinyApp(ui, server)
```

```{=html}
<div style="width: 100% ; height: 400px ; text-align: center; box-sizing: border-box; -moz-box-sizing: border-box; -webkit-box-sizing: border-box;" class="muted well">Shiny applications not supported in static R Markdown documents</div>
```





