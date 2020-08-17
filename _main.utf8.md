---
title: "Templates"
author: "christoffer.nordenlow@riksbank.se"
date: '2020-05-26'
site: bookdown::bookdown_site
output: bookdown::gitbook
---


# Intro {-}

*This repository includes useful templates, formulas and code for R, VBA, SQL, also in combination with Bloomberg formulas and Fixed Income analytics.*

<!--chapter:end:index.Rmd-->

# R

## Create template tables

### Template tables

**Create table with random dates between two dates. Use max date and create a table with dates until this date.**


```r
#https://www.cyclismo.org/tutorial/R/basicOps.html#basic-operations
library(dplyr)
df <- tibble(
  date = sample(seq(as.Date('2020/06/01'), as.Date('2025/01/01'), by="day"), 20)
)
df2 <-tibble(
  date = seq.Date(Sys.Date(), max(as.Date(df$date)), by = "day")
)
```

**Create new column with random number**


```r
library(tidyverse)
df <- data.frame(Amount = 1:10)
df %>%
  rowwise %>%
  mutate(newColumn = sample(1:5, 1))
  
  #It´s often neccessary to ungroup rowwise.
  df <- as.data.frame(df)
```


**Create table with a combination of fixed and random numbers**
  
 
 ```r
 library(tidyverse)
 
 df <- tibble(
  value = seq(10,90,1),
  rand = seq(10,90,1) +runif(81, min=-10, max=15)
 ) 
 ```


**Create a list and bind together, only 1 column*
  
 
 ```r
 libary(dplyr)
 
 lst <- list(cars, cars)
 lst <- bind_rows(lst) 
 
 lst %>% select(2)
 
 #Bind together list, include the index number / name. Use data.table
 library(data.table)
 
 lst <- list(cars, cars)
 lst <- rbindlist(lst, idcol = TRUE)
 ```


## Import & Export 

### Import


**Import fast using httpcashe**
  
*Improving efficiency in importing*



```r
get_data <- function(url) {
  httpcache::GET(url) %>%
    httr::content()
}

url_jobless_claims="https://oui.doleta.gov/unemploy/csv/ar539.csv"

data_jobless_claims <- get_data(url_jobless_claims)
```


**Import all files in a folder**
  
*Import all files in a folder. In the example below files are named "2020-05-05 Saldo". Import and create a table where the date of the filename is used in a column. Change name for column 1 and 2.*

*Map has similiar functionality to lapply. When you add \_dfr it will generate <span style="color:red">data.frames</span> and that these is merged.*



```r
parse_date <- function(x) as.Date(gsub( ".*(\\d{4}-\\d{2}-\\d{2}).*", "\\1", x))
dir_loc <- '...RX-filer\\Saldo'
rix_saldo <- dir(dir_loc, full.names = T) %>%
  map_dfr(~{
    read.csv2(.x, skip = 1, header = F) %>%
      mutate(date = as.Date(parse_date(basename(.x))))
  })
colnames(rix_saldo)[colnames(rix_saldo) == 'V1'] <- 'Participant'
colnames(rix_saldo)[colnames(rix_saldo) == 'V2'] <- 'Saldo'
```

**Import all files in a folder with conditions of name**

*Use pattern to set the conditions which files that should be imported. For example, those who ends with "Saldo.csv" as below. Regexp (see http://jkorpela.fi/perl/regexp.html)  is used in pattern. For example, you need to use \\. for the dot. Also, ^ can be used to determine the beginning of a string while $ is used to set the end. pattern = "^.*Saldo\\.csv$"*


```r
parse_date <- function(x) as.Date(gsub( ".*(\\d{4}-\\d{2}-\\d{2}).*", "\\1", x))
dir_loc <- '...RX-filer\\Saldo'
rix_saldo <- dir(dir_loc, full.names = T, pattern = "^.*Saldo\\.csv$") %>%
  map_dfr(~{
    read.csv2(.x, skip = 1, header = F) %>%
      mutate(date = as.Date(parse_date(basename(.x))))
  })
colnames(rix_saldo)[colnames(rix_saldo) == 'V1'] <- 'Participant'
colnames(rix_saldo)[colnames(rix_saldo) == 'V2'] <- 'Saldo'
```



**Import excel from web**
  
*Import excel from web by downloading it temp*



```r
library(readxl)
url_data_gdp <- ("https://www.bea.gov/system/files/2020-04/qgdpstate0420.xlsx")
download.file(url=url_data_gdp, destfile="localcopy.xlsx", mode="wb")

#Table 1: Percent Change in Real Gross Domestic Product (GDP) by State and state
table1 <- read_excel('localcopy.xlsx', sheet = 1, skip =4, col_names = FALSE)
```



### Export


**Export to txt file**
  


```r
write.table(table_for_report, "...\\Operations\\LikvProg\\likvprog_history.txt", sep="\t")
```

**Export to csv file**
  

```r
#Using both write.csv2 or write.table

library(data.table)

#Write csv2. No row.names, na = "" and quote ("") mark as false
write.csv2(total_purchases_commercial_papers, ".....R_tables\\Output_data\\webpage_purchases\\Total_purchases_commercial_papers.csv",row.names=FALSE,na = "", quote = FALSE)

#Write.table. No row.names, na = "" and quote ("") mark as false
write.table(total_purchases_commercial_papers,file="...\\Total_purchases_commercial_papers3.csv",row.names=FALSE,sep=";",dec = " ",quote = FALSE)
```



**Get table to paste into excel**


```r
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(df,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

write.excel(my.df)
```

## Tidy & Transform

### Cleaning


**Cleaning some data**
  
*Gather, Spread, Separate, Unite*

 
 
 ```r
 library(tidyr)
 
 #Create a messy dataset
 messy <- data.frame(
  country = c("A", "B", "C"),
  q1_2017 = c(0.03, 0.05, 0.01),
  q2_2017 = c(0.05, 0.07, 0.02),
  q3_2017 = c(0.04, 0.05, 0.01),
  q4_2017 = c(0.03, 0.02, 0.04))
 messy
 
 #Reshape the data. in this function we create two new variables instead of the one in the original dataset.
 tidier <- messy%>%
  gather(quarter, growth, q1_2017:q4_2017)
 tidier
 
 #Spread
 #the spread function does the opposite of gather.
 #Reshape the tidier dataset back to messy.
 
 messy_1 <- tidier %>%
  spread(quarter, growth)
 messy_1
 
 #Separate
 #Separate splits a column into two according to a separator. This function is helpful in some situations where the variable is a date, i.e. separate year and month.
 
 separate_tidier <- tidier %>%
  separate(quarter, c("Qrt", "year"), sep ="_")
 head(separate_tidier)
 
 #Unite
 #Unite concatenates two columns into one.
 
 unit_tidier <- separate_tidier%>%
  unite(Quarter, Qrt, year, sep = "_")
 head(unit_tidier)
 ```
 
 
 

### Expand

**Expand table**
  
*One example with expanding to all alternatives. Another to fill in gaps.*


```r
library(tidyverse)
library(dplyr)
##Expand all alternatives
a <- c(1:10)
b <- c(1:10)
c <- c(1:10)
df <- tibble(a,b,c)
all_combinations <- expand(df, a,b,c) 
#Expand by missing Date
df <- tibble(
  year   = c(2010, 2010, 2010, 2010, 2012, 2012, 2012),
  qtr    = c(   1,    2,    3,    4,    1,    2,    3),
  return = rnorm(7)
)
df %>% expand(year, qtr)
df %>% expand(year = 2010:2012, qtr)
df %>% complete(year = full_seq(year, 1), qtr)
```


### Join and Merge

**Join tables**
  
*Different ways to join tables.*

 
 
 ```r
 library(dplyr)
 
 df_primary <- tribble(
 ~ID,~y,
 "A", 5,
 "B", 5,
 "C", 8,
 "D", 0,
 "E", 9)
 
 df_secondary <- tribble(
  ~ID,~y,
  "A", 30,
  "B", 21,
  "C", 22,
  "D", 25,
  "F", 29)
  
 #Most common way to merge two datasets is to uset the left_join() function.
 left_join_ <- left_join(df_primary, df_secondary, by ='ID')
 
 #The right_join works like the left one.
 right_join_ <- right_join(df_primary, df_secondary, by = 'ID')
 
 
 #When we are sure that two datasets won´t match, we can consider to return only rows existing in both datasets. 
 #This is legit when we need a clean dataset or when we dont want to impute missing values with the mean or median.
 inner_join_ <- inner_join(df_primary, df_secondary, by ='ID')
 
 # Full_join keeps all observations and replace missing values with NA.
 full_join_ <- full_join(df_primary, df_secondary, by = 'ID')
 ```
 
 **Join tables on multiple conditions**
 
 *Join Tables on multiple conditions*

 
 
 ```r
 library(dplyr)
 
 df_primary <- tribble(
  ~ID, ~year, ~items,
  "A", 2015,3,
  "A", 2016,7,
  "A", 2017,6,
  "B", 2015,4,
  "B", 2016,8,
  "B", 2017,7,
  "C", 2015,4,
  "C", 2016,6,
  "C", 2017,6)
 df_secondary <- tribble(
  ~ID, ~year, ~prices,
  "A", 2015,9,
  "A", 2016,8,
  "A", 2017,12,
  "B", 2015,13,
  "B", 2016,14,
  "B", 2017,6,
  "C", 2015,15,
  "C", 2016,15,
  "C", 2017,13)
 left_join(df_primary, df_secondary, by = c('ID', 'year'))
 ```
 
 **Merge Data Frames**
 
 *Merge Data Frames in R: Full and partial match*
  
 
 ```r
 producers <- data.frame(   
  surname =  c("Spielberg","Scorsese","Hitchcock","Tarantino","Polanski"),    
  nationality = c("US","US","UK","US","Poland"),    
  stringsAsFactors=FALSE)
 
 # Create destination dataframe
 movies <- data.frame(    
  surname = c("Spielberg",
              "Scorsese",
              "Hitchcock",
              "Hitchcock",
              "Spielberg",
              "Tarantino",
              "Polanski"),    
  title = c("Super 8",
            "Taxi Driver",
            "Psycho",
            "North by Northwest",
            "Catch Me If You Can",
            "Reservoir Dogs","Chinatown"),                
  stringsAsFactors=FALSE)
 
 m1 <- merge(producers, movies, by.x = "surname")
 m1
 
 # Change name of ` movies ` dataframe
 colnames(movies)[colnames(movies) == 'surname'] <- 'name'
 
 # Merge with different key value
 m2 <- merge(producers, movies, by.x = "surname", by.y = "name")
 
 ##Partial match
 # Create a new producer
 add_producer <-  c('Lucas', 'US')
 #  Append it to the ` producer` dataframe
 producers <- rbind(producers, add_producer)
 # Use a partial merge 
 m3 <-merge(producers, movies, by.x = "surname", by.y = "name", all.x = TRUE)
 m3
 ```
 
 
 
### Transforming data with Apply etc

**apply(), lapply(), sapply(), tapply()**
  
**apply()**



```r
library(dplyr)
m1 <- matrix(c<-(1:10), nrow=5,ncol=6)
m1

#Sums columns
a_m1 <- apply(m1,2,sum)
a_m1

#Sums rows
a_m1 <- apply(m1,1,sum)
a_m1
```

**lapply()**


```r
library(dplyr)
movies <- c("spyderman", "batman", "vertigo", "chinatown")
movies_lower <- lapply(movies, tolower)
str(movies_lower)

#if we like to convert the list into a vector we can use unlist()
movies_lower <- unlist(lapply(movies, tolower))
str(movies_lower)
```
 
**sapply()**



```r
#sapply() function does the same jobs as lapply() function but returns a vectorÄ

library(dplyr)
dt <- cars
lmn_cars <- lapply(dt, min)
smn_cars <- sapply(dt,min)
lmn_cars

smn_cars


lmxcars <- lapply(dt,max)
smxcars <- sapply(dt,max)

lmxcars
smxcars

#lets create a function names avg to compute the average of the minimun and maximun of the vector.

avg <- function(x){
  (min(x) + max(x))/2
}
fcars <- sapply(dt, avg)
fcars

#sapply() function is more efficient than lapply() in the output returned because sapply() store values directly into a vector.


#it is possible to use lapply or sapply interchangeable to slice a data frame.
#lets compute a function that takes a vector of numerical values and returns a vector that only contains the values that are strictly above the average.

below_ave <- function(x){
  ave <- mean(x)
  return(x[x>ave])
}

dt_s <- sapply(dt, below_ave)
dt_l <- lapply(dt, below_ave)
identical(dt_s, dt_l)
```
 


**tapply()**


 

```r
#The function tapply() computes a measure (mean, median, min, max) or a function for each factor variable in a vector

library(dplyr)
data(iris)
tapply(iris$Sepal.Width, iris$Species, median)
```



### Tally-function

**Tally()**

*Tally is a useful wrapper for summarise with grouping conditions. In the example below we have a data set with countries. For US, there are no aggregate number, so we need to summarize each state.*


```r
library(tidyr)
library(dplyr)
df <- tibble::tribble(
  ~country, ~state, ~t1, ~t2,
  "SE", NA, 1,2,
  "US", "A", 10,20,
  "US", "B", 11,21,
)

df%>%
  tidyr::gather(date, value, -country, -state)%>%
  group_by(country, date) %>%
  tally(value)
```

 
 


## Working with strings and characters


**Remove last n characters**
  

```r
#Remove last n characters in a string
df <- tibble(
  program = c(rep("okv 20200528",10), rep("ftg 20200525",10))
)
df$program <- substr(df$program,1,nchar(df$program)-9)
```


## Visualize


### Ggplots

**geom_line with geom_ribbon**
  
*geom_line with geom_ribbon for pos / neg numbers*
 

```r
library(ggplot2)

df <- tibble(
  value = seq(1,50,1),
  rand = seq(1,50,1) +runif(50, min=-10, max=15)
)%>%
  mutate(diff = rand - value)


exposure_graph <- ggplot(df, aes(x=value,y=rand)) +
  geom_ribbon(aes(ymin=pmin(df$diff,0), ymax=0), fill="red", col="black", alpha=0.5) +
  geom_ribbon(aes(ymin=0, ymax=pmax(df$diff,0)), fill="blue", col="black", alpha=0.5) +
  geom_line(aes(y=0))
```

### Different tables

**Create table with kableExtra**
  
*Create table with different colors for pos / neg numbers*
 

```r
library(tidyverse)
library(kableExtra)
df <- tibble(
  type = c("gov_bond", "ssa", "ssa", "gov_bond","ssa", "ssa", "gov_bond", "gov_bond", "gov_bond", "ssa"),
  maturity_bucket = as.integer(runif(10, min =1, max=6)),
  diff_bm = runif(10, min = -10, max = 10)
)
sum_type <- df %>%
  group_by(type, maturity_bucket)%>%
  summarise(
    diff_exposure = round(sum(diff_bm),1)
  )
## Create table with green for positive, red for negative
sum_table <- sum_type%>%
  
  mutate(
    diff_exposure = ifelse(diff_exposure < 0,
                      cell_spec(diff_exposure, "html", color = "red", bold = T),
                      cell_spec(diff_exposure, "html", color = "green", italic = T)))%>%
  
  kable("html", escape = F, format.args=list(big.mark=" ", scientific=F)) %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position= "right", fixed_thead = T)
sum_table
```

## Misc



### Moving average

**Create a moving average**
  
*Example of creating a moving average for dates.*


```r
library(tidyverse)
library(dplyr)
library(lubridate)
df <- tibble(
Date = seq.Date(Sys.Date()-19, Sys.Date(), by="day"),
indicator = c(rep(1,10),rep(2,10)),
value = rnorm(20)
)
df <- arrange(df, Date)
df  %>%
  group_by(indicator) %>% 
  mutate(MA_3m = slide_index_dbl(value, Date, mean, .before=lubridate::days(2), .after=0,.complete=T))
  
  #Use before or after = Inf if you like to get the calculation based on all values before or after.
```


### Date Formating

**Different ways to format dates**
  
*Dates*




```r
as.Date("2/15/1986", format = "%m/%d/%Y")
```

*Formating date with use of gsub to adjust the the string.*



```r
library(tidyverse)

date_to_format = "2016-10-17 UTC"
as.Date(gsub("\\D", "", date_to_format), format = "%Y%m%d")

#or changing who table
df <- tibble(
    date_to_format = "2016-10-17 UTC"  
      )

df <- df %>%
mutate(date_to_format = as.Date(gsub("\\D", "", date_to_format), format = "%Y%m%d"))
```


### Loops

#### For loop example
  
*Creates a non-linear function by using the polynomial of x between 1 and 4 and we store it in a list*


```r
#
# Create an empty list
list <- c()
# Create a for statement to populate the list
for (i in seq(1, 4, by=1)) {
  list[[i]] <- i*i
}
print(list)
```

**For loop over a matrix**
  
*A matrix has 2-dimension, rows and columns. To iterate over a matrix, we have to define two for loop, namely one for the rows and another for the column.*


```r
# Create a matrix
mat <- matrix(data = seq(10, 20, by=1), nrow = 6, ncol =2)
# Create the loop with r and c to iterate over the matrix
for (r in 1:nrow(mat))   
  for (c in 1:ncol(mat))  
    print(paste("Row", r, "and column",c, "have values of", mat[r,c])) 
```




#### For loop example
  
*Creates a non-linear function by using the polynomial of x between 1 and 4 and we store it in a list*


```r
#
# Create an empty list
list <- c()
# Create a for statement to populate the list
for (i in seq(1, 4, by=1)) {
  list[[i]] <- i*i
}
print(list)
```

**Function for Right and Left**
  
*Functions for Right and Left.*


```r
library(dplyr)

right = function(text, num_char) {
  substr(text, nchar(text) - (num_char-1), nchar(text))
}
left = function(text, num_char) {
  substr(text, 1, num_char)
}

df <- tibble(
Date = seq.Date(Sys.Date()-19, Sys.Date(), by="day"),
indicator = c(rep(1,10),rep(2,10)),
value = rnorm(20)
)

left(df$value, 3)
right(df$Date, 3)
```


**Bloomberg API in R**
  
*Blmrg API with package Rblpapi*


```r
library(Rblpapi)

con <- blpConnect() 

#generic
us10 <- bdh(securities = "USGG10YR Index", 
           fields = "PX_LAST", 
           start.date = as.Date("2020-03-01"))
           
#Isin
us10 <- bdh(securities = "US912828ZQ64 Govt", 
            fields = "PX_LAST", 
            start.date = as.Date("2020-03-01"))

##Multiple fields

bonds <- c("CA135087K601 Govt","CA563469UP83 Govt")

fields <- c("PX_LAST", "YLD_YTM_MID", "PX_DIRTY_MID", "Issuer", "SHORT_NAME", "YRS_TO_MTY_ISSUE","YAS_ASW_SPREAD", "CPN", "AMT_OUTSTANDING", "%_OF_TSY_HLD_IN_THE_FED_RES_SOMA", "YLD_CHG_NET_1D", "YLD_CHG_NET_1M", "INTERVAL_Z_SCORE", "MTY_YEARS_TDY", "YLD_CHG_NET_5D")

df <- bdp(securities = bonds,
          fields = fields)

df <- tibble::rownames_to_column(df, "isin_govt")
```


## R Markdown 

  
### Render multiple reports

**Render multiple reports in different folders.**

*In the example below one report is created for each stated currency. Params = list(currency) is the key.*
  

```r
#Write in one R Script
#Remove old
file.remove("...xxx/report/Benchmark_R/Portfolio_report_GBP.html")
file.remove("...xxx/report/Benchmark_R/Portfolio_report_AUD.html")
file.remove("...xxx/report/Benchmark_R/Portfolio_report_EUR.html")
purrr::map(
  c("AUD", "EUR", "GBP"),
  ~ {
    res <- rmarkdown::render("...xxx\\report\\Benchmark_R\\R code\\Markdown BM.Rmd", output_file = sprintf("...xxx\\report\\Benchmark_R\\Portfolio_report_%s.html", .x), params = list(currency = .x))
    file.copy(res,  sprintf("...xxx\\report\\Benchmark_R\\Old_reports\\Portfolio_report_%1$s_%2$s.html", .x, Sys.Date()))
    file.copy(res,  sprintf("...xxx/report/Benchmark_R//Portfolio_report_%s.html", .x))
  } 
)
#Markdown Report header
---
#title: "Portfolio and benchmark report"
output: html_document
date: "`r Sys.Date()`"
author: christoffer.nordenlow@outlook.com
params:
  currency: "EUR"
  
title: "`r sprintf('Portfolio and benchmark report, %s', params$currency)`"  
---
```


## Web Scraping


### Scrape all sub page

**Scrape web page info and save in a table**
  
*Scrape all different sub web pages under a base page. In the below example there a number of sub pages under the base bage. R is scraping all different URL under the main page. Info in the tables under the sub pages are saved in a table. You will need to have HTTP_PROXY/HTTPS_PROXY as environment variables.*

   



```r
#https://cran.r-project.org/web/packages/rvest/rvest.pdf
require(rvest)
require(xml2)
require(tidyverse)
.base_url <- "https://www.riksbank.se"
doc <- read_html(file.path(.base_url, "sv/penningpolitik/penningpolitiska-instrument/kop-av-foretagscertifikat/special-terms-and-conditions/"))
urls <- doc %>%
  html_nodes("a") %>%
  html_attr("href")
urls <- urls[str_detect(urls, regex(".*/special-terms-and-conditions/.*bid-date.*$"))]
urls <- file.path(.base_url, urls)
names(urls) <- basename(urls)
doc_subpage <- read_html(urls[[1]])
df <- urls %>%
  map_dfr(~{
    doc_subpage %>%
      html_node("table") %>%
      html_table() %>%
      rename(key=X1, value=X2) %>%
      as_tibble()
  }, .id = "url")
  
  #It is possible to filter which files should be imported.
  #map(...) %>% filter(lubridate::year(date) == 2019)
```



### Scrape PL table

**Scrape one table**



```r
library(rvest)

web_pl <- read_html("https://www.foxsports.com/soccer/stats?competition=1&season=20190&category=standard&sort=3")
tbls <- html_nodes(web_pl, "table")
head(tbls)


pl_stats <- web_pl %>%
  html_nodes("table") %>%
  # .[3:4] %>%
  html_table(fill = TRUE)%>%
  .[[1]]
```


### Scrape all tables

**Scrape all tables, use one**



```r
##Web scrape US Data. Payroll
#http://bradleyboehmke.github.io/2015/12/scraping-html-tables.html

library(rvest)

web_bls <- read_html("http://www.bls.gov/web/empsit/cesbmart.htm")

tbls <- html_nodes(web_bls, "table")  #extract all table nodes that exist on the page.

head(tbls)


#To parse the HTML, we use html_table. In this example it creates
table_bls <- web_bls %>%
  html_nodes("table") %>%
  .[3:4] %>% ##determines which tables. In this case, table 3 and 4.
  html_table(fill = TRUE)

str(table_bls)

#Extract table 2, non-farm
head(table_bls[[2]], 4)

# remove row 1 that includes part of the headings. Not neccessary here
#table_bls[[2]] <- table_bls[[2]][-1,]


table_bls2 <-table_bls[[2]]
```


### Scrape title

**Scrape title**



```r
library(rvest)

lego_movie <- read_html("http://www.imdb.com/title/tt1490017/")
lego_movie %>%
  html_node(xpath='//div[@class="originalTitle"]') %>%
  html_text() 
```



## Useful functions / expressions





### Sub / Gsub
  
*Replace the first occurence of a pattern with a sub or replace all occurrences with gsub.*
*Gsub() replaces all matches of a string.*


```r
x <- "Old City"
gsub("Old", "New", x)

#case insensitive
gsub("old", "New", x, ignore.case=T)

#Vector replacement
y <- c("Stockholm City", "Uppsala City", "Malmö")
gsub(" City","",y)
```
 
 ### rnorm
  
*Generate number from a normal distribution.*

```r
 rnorm(4)
#> [1] -2.3308287 -0.9073857 -0.7638332 -0.2193786

# Use a different mean and standard deviation
rnorm(4, mean=50, sd=10)
#> [1] 59.20927 40.12440 44.58840 41.97056

# To check that the distribution looks right, make a histogram of the numbers
x <- rnorm(400, mean=50, sd=10)
hist(x)
```
  
### slice
*Example: way to take out a single row.*

```r
library(dplyr)
mtcars

select_row = 1
df <- arrange(mtcars, mpg)

df2 <- df %>%
  slice(which(row_number() == select_row))
```

### unique
*Example: get all unique values in a column*

```r
library(dplyr)
library(data.table)

df <- mtcars

unique(df$cyl, incomparables = FALSE)
```

### Map (purrr)
*Apply a function to each element of as list or vector.
https://purrr.tidyverse.org/reference/map.html*


```r
# map_dfr
# apply a function to each element

library(tidyverse)

leading_indicators <- c(
  "INJCJC Index",
  "INJCJC4 Index",
  "INJCSP Index",
  "RSTAMOM Index",
  "SAARTOTL Index",
  "USHBTRAF Index",
  "DGNOCHNG Index",
  "DGNOYOY Index",
  "LEI CHNG Index"
)

get_data <- function(indicator) {
  tibble(ind = indicator, data = 1)
}

leading_indicators %>% 
  map_dfr(get_data) 
```



<!--chapter:end:01-R.Rmd-->

# Excel & VBA


## Import

### Standard way to import file



  
  
```  
Sub import_file()

'Code to delete old data and to import new file

Application.ScreenUpdating = False
Application.DisplayAlerts = False

Sheets("location_file").ClearContents

  TheHomeFile = ActiveWorkbook.Name

    Path = "\\riksbank.se\profile\home\chnord\My Documents\test\"
    Name = "likvprog_history.txt"

        Workbooks.OpenText Filename:= _
          Path & Name, Local:=True

        Range("a1:z10000").Copy
      Workbooks(TheHomeFile).Activate
      Sheets("likvprog_history").Select
      Range("a1").PasteSpecial xlValues

  Workbooks(Name).Close savechanges:=False


Sheets("main").Select

Application.CutCopyMode = False

End Sub
```  

### Import file with conditions

*Import files after a certain date. Check if file exists before import.*

   

```  
Sub import_file()

Application.ScreenUpdating = False
Application.DisplayAlerts = False

Sheets("location_file").ClearContents

#Input files after this date
from_date = "2020-05-01"

current_date = from_date


  TheHomeFile = ActiveWorkbook.Name

    Path = "xxx\home\chnord\My Documents\test\"


Do Until Format(current_date, "YYYY-MM-DD") >= Format(to_date, "YYYY-MM-DD")

Name = current_date & " Saldo.csv"

''check if file exists
file_exists = False
If Dir(Path & Name) <> "" Then file_exists = True

If file_exists = True Then

        Workbooks.OpenText Filename:= _
          Path & Name, Local:=True

        Range("a1:z10000").Copy
      Workbooks(TheHomeFile).Activate
      Sheets("likvprog_history").Select
      Range("a1").PasteSpecial xlValues

  Workbooks(Name).Close savechanges:=False

end if

current_date = DateAdd("d", 1, current_date)
Loop




Application.CutCopyMode = False

End Sub
```  


## Loops

### For loop
**Loop thru all possible scenarios.**
  
*Loop All alternatives. In Rows 2:4 there are three alternatives in each column. Loop all possible scenarios.*


```  
sub for_loop_all_alternatives

Sheets("sheet1").Select
Sheets("sheet1").Range("c2:k2") = 1
Sheets("sheet1").Range("c3:k4") = 2
Sheets("sheet1").Range("c4:k4") = 3

Count = 1
Row = 7

For c = 2 To 4
    For d = 2 To 4
        For e = 2 To 4
            For f = 2 To 4
                For g = 2 To 4
                    For h = 2 To 4
                        For i = 2 To 4
                            For j = 2 To 4
                                For k = 2 To 4

                                Cells(Row, 2) = Count
                                Cells(Row, 3) = Range("c" & c)
                                Cells(Row, 4) = Range("d" & d)
                                Cells(Row, 5) = Range("e" & e)
                                Cells(Row, 6) = Range("f" & f)
                                Cells(Row, 7) = Range("g" & g)
                                Cells(Row, 8) = Range("h" & h)
                                Cells(Row, 9) = Range("i" & i)
                                Cells(Row, 10) = Range("j" & j)
                                Cells(Row, 11) = Range("k" & k)

                                Count = Count + 1
                                Row = Row + 1

                                Next
                            Next
                        Next
                    Next
                Next
            Next
        Next
    Next
Next
End Sub
```  
  


## Misc


### Misc

 **Format from text to number**
  
*Format from text to number when excel "requires" a press of enter button.*

``` 
or Each r In Sheets("Sheet1").UsedRange.SpecialCells(xlCellTypeConstants)
    If IsNumeric(r) Then
       r.Value = CSng(r.Value)
       r.NumberFormat = "0.00"
    End If
Next
```

## Useful excel formulas 

## Useful Bloomberg formulas

**Import Rating with override function**

``` 
'Approah to import rating for specific date

=BDP("SAND SS equity";"RTG_SP_LT_LC_ISSUER_CREDIT";"RATING_AS_OF_DATE_OVERRIDE="&"2020-01-01")
```

**Formula for importing data to excel**

``` 
'Import data from Bloomberg, 2 cols.

=BDH("INJCJC Index";"px_last";"2015-07-02";"";"Dir=V";"Dts=S";"Sort=d";"Quote=C";"QtTyp=Y";"Days=a";"Per=cd";"DtFmt=D";"Fill=P";"UseDPDF=Y";"cols=2;rows=1826")

```




<!--chapter:end:02-VBA.Rmd-->

# FI & FX


## Fixed Income
  
### Bond Calculator in R

#### Calculate Forward Rates

**Calculate Forward Rates**
  
*Create a table with bond (or import real ones) and calculate forward rates.*


```r
library(dplyr) 
#Calculate Forward rate 
#Create a table with plain vanilla bonds
df <- tribble( 
  ~bond, ~maturity, ~yield, 
  1, 1.5, 1.65, 
  2, 3, 1.55, 
  3, 5, 1.8, 
  4, 10, 1.9 
) 
#Create table with all bonds in columns for short vs long bond
df <- df %>% 
  mutate(dummy = 1L) %>% 
  inner_join(., ., by = "dummy", suffix=c("_short", "_long")) %>% 
  select(-dummy) %>% 
  filter(bond_short < bond_long) 
#Create column with maturity for length between bonds (not neccesary for below calculation)
df <- mutate(df, maturity_between_bonds = (maturity_long  - maturity_short))
day_count <- 360
#Create function for calculating frw rate
calculate_forward_rate <- function(maturity_short, yield_short, maturity_long, yield_long, day_count){
  
  short_bond <- (1+yield_short/100)^(maturity_short/day_count)
  long_bond <- (1+yield_long/100)^(maturity_long/day_count)
  days_between <- (maturity_long - maturity_short)
  forward_rate <- ((long_bond/short_bond)^(360/days_between)-1)*100
  return(round(forward_rate, digits=2))  
}
#run function
df <- df %>%
  mutate(forward_rate = calculate_forward_rate(
    maturity_short,
    yield_short,
    maturity_long,
    yield_long,
    day_count))
#Create a yield_diff. How much more/less the yield must be when its time to buy the subsequent bond       
df <- df %>%
  mutate(yield_diff = if_else(bond_short == bond_long, NA_real_, forward_rate - yield_short))
```


#### Bond Converter

**Calculate Bond Price**


```r
Maturity <- "2023-04-30"
  Handle <- 100
  x32 <- 25
  x64 <- 24
  cpn <- 2.25
  
  ttm <- as.numeric(as.Date(Maturity) - as.Date(Sys.Date())) / 365
  FV <- 100
  
  calculate_price <- function(Handle, x32, x64){
    bond_price <- Handle + ((x32+(x64/64))/32)
    return(format(round(bond_price,10), nsmall=10))
  }
  
 bond_price <- as.numeric(calculate_price(Handle, x32, x64))
```


**Convert from Discount to Yield**


```r
  discount  <- 1.69
  maturity_date <- as.Date("2021-05-06")
  settlement_date <- Sys.Date() +1
  day_count <- 360
  calculate_ytm_from_discount <- function(discount, maturity_date, settlement_date, day_count){
 
    days <- as.numeric(maturity_date-settlement_date)
    discount <- discount / 100
      yield_ <- (discount / (1-(discount * (days/day_count))))
    return(format(round(yield_*100,10),nsmall=10))
  }
  
  calculate_ytm_from_discount(discount, maturity_date, settlement_date, day_count)
```

**Convert from Yield to Discount**


```r
  yield  <- 1.719967
  maturity_date <- as.Date("2021-05-06")
  settlement_date <- Sys.Date()+1
  day_count <- 360
  yield <- yield / 100
  calculate_disc_from_yield <- function(yield, maturity_date, settlement_date, day_count){
    
    days <- as.numeric(maturity_date-settlement_date)
 
    discount <- (yield / (1+(yield /(day_count/days))))
    return(format(round(discount*100,10),nsmall=10))
  }
  
  calculate_disc_from_yield(yield, maturity_date, settlement_date, day_count)
```
  


## Foreign Exchange

<!--chapter:end:03-FI_FX.Rmd-->

# SQL




## General
  *https://www.w3schools.com/sql/default.asp*
  *'Getting started with SQL' using SQLite. Download database found in link by vlicking the ZIP button and copy the contents to a folder of choice. The navigate in the top menu to Database -> add a database.*
  *https://github.com/thomasnield/oreilly_getting_started_with_sql*


### SELECT


```r
SELECT * FROM CUSTOMER;

SELECT CUSTOMER_ID,  NAME FROM CUSTOMER;

# Generate a calculated column
SELECT
PRODUCT_ID,
DESCRIPTION,
PRICE AS UNTAXED_PRICE,
round(PRICE * 1.07,2) AS TAXED_PRICE
FROM PRODUCT;

# Text concatenation. Concatenation works with any data type.
SELECT NAME,
CITY || ', ' || STATE AS LOCATION
FROM CUSTOMER;
```

### WHERE


```r
SELECT * FROM STATION_DATA
WHERE year = 2010;

#Use != or <> to get everything but 2010
SELECT * FROM STATION_DATA
WHERE year != 2010;

#AND, OR, IN statements
SELECT * FROM STATION_DATA
WHERE year >= 2005 AND year <= 2010;

#in
SELECT * FROM STATION_DATA
WHERE MONTH IN (3,6,9,12)

#not in
SELECT * FROM STATION_DATA
WHERE MONTH NOT IN (3,6,9,12)

#modulus operator
#modulus returns the remainder instead of the quotient. A remainder of 0 means there is no remainder at all
SELECT * FROM STATION_DATA
WHERE MONTH %  3 = 0

#using where on text
SELECT * FROM STATION_DATA
WHERE report_code in ('513A63', '1F8A7B')

SELECT * FROM STATION_DATA
WHERE length(report_code) = 6

#wildcards
SELECT * FROM STATION_DATA
WHERE report_code LIKE 'A%'

#B as the first, C as the third letter

SELECT * FROM STATION_DATA
WHERE report_code LIKE 'B_C%'

#Other text functions as INSTR, SUBSTR, REPLACE ETC

SELECT * FROM  station_data
WHERE snow_depth IS NULL;

#Use coalesce to conert NULL to 0, "N/A" etc

SELECT * FROM  station_data
WHERE coalesce(precipitation,0) <= 0.5;

SELECT * FROM station_data
WHERE (rain = 1 AND temperature <= 32)
OR snow_depth > 0;
```

### GROUP BY

*WHERE filters individual records while HAVING filters aggregations.*

```r
SELECT year, month, COUNT(*) AS record_count FROM station_data
WHERE tornado = 1
GROUP BY year, month
ORDER BY year, month

#aggregate
SELECT month, AVG(temperature) as avg_temp
FROM station_data
WHERE year >= 2000
GROUP BY month

#HAVING statement
#To be able to filter on aggregate data you need to use HAVING instead of WHERE

SELECT year,
SUM(precipitation) as total_precipitation
FROM station_data
GROUP BY year
HAVING sum(precipitation) > 30
```


### CASE statements

*CASE statements are read from top to bottom, so the first true condition is the one it uses.*
*A great benefit of CASE statements compared to WHERE is that you can apply different filters for different aggregate values.*

```r
SELECT report_code, year, month, day, wind_speed,

CASE
 WHEN wind_speed >= 40 THEN 'HIGH'
 WHEN wind_speed >= 30 THEN 'MODERATE'
 ELSE 'LOW'
 END as wind_severity
 
FROM station_data

# Use of CASE to apply different filters
SELECT year, month,

SUM(CASE WHEN tornado = 1 THEN precipitation ELSE 0 END) as tornado_precipitation,

SUM(CASE WHEN tornado = 0 THEN precipitation ELSE 0 END) as non_tornado_precipitation

FROM station_data
GROUP BY year, month

# Use of boolean expression

SELECT month, 

AVG(CASE WHEN rain OR hail THEN temperature ELSE null END) AS avg_precipitation_temp,

AVG(CASE WHEN NOT (rain OR hail) THEN temperature ELSE null END) AS avg_non_precipitation_temp

FROM station_data
WHERE year > 2000
GROUP BY month
```


### JOIN

*In multiple joins, it may be erroundous to mix LEFT JOIN with INNER JOIN. This is becasue null values cannot be inner joined on and will get filtered out. LEFT JOIN tolerates null values.*


```r
#INNER JOIN
SELECT ORDER_ID,
CUSTOMER.CUSTOMER_ID,
ORDER_DATE,
ORDER_QTY

FROM CUSTOMER INNER JOIN CUSTOMER_ORDER
ON CUSTOMER.CUSTOMER_ID = CUSTOMER_ORDER.CUSTOMER_ID


#LEFT JOIN

SELECT ORDER_ID,
CUSTOMER.CUSTOMER_ID,
ORDER_DATE,
ORDER_QTY

FROM CUSTOMER LEFT JOIN CUSTOMER_ORDER
ON CUSTOMER.CUSTOMER_ID = CUSTOMER_ORDER.CUSTOMER_ID

#Checking for NULL values
SELECT CUSTOMER.CUSTOMER_ID,
NAME AS CUSTOMER_NAME

FROM CUSTOMER LEFT JOIN CUSTOMER_ORDER
ON CUSTOMER.CUSTOMER_ID = CUSTOMER_ORDER.CUSTOMER_ID

WHERE ORDER_ID IS NULL

#Multiple joins
SELECT
ORDER_ID,
CUSTOMER.CUSTOMER_ID,
'NAME AS CUSTOMER.NAME,'
STREET_ADDRESS,
CITY,
STATE,
ZIP,
ORDER_DATE,
PRODUCT_ID,
DESCRIPTION,
ORDER_QTY

FROM CUSTOMER

INNER JOIN 
CUSTOMER_ORDER ON CUSTOMER_ORDER.CUSTOMER_ID = CUSTOMER.CUSTOMER_ID

INNER JOIN 
PRODUCT ON CUSTOMER_ORDER.PRODUCT_ID = PRODUCT.PRODUCT_ID

#Use coalesce() to turn nulls into zeros.

SELECT
CUSTOMER.CUSTOMER_ID,
NAME AS CUSTOMER_NAME,
coalesce(sum(ORDER_QTY * PRICE), 0) as TOTAL_REVENUE

FROM CUSTOMER

LEFT JOIN CUSTOMER_ORDER
ON CUSTOMER.CUSTOMER_ID = CUSTOMER_ORDER.CUSTOMER_ID

LEFT JOIN PRODUCT
ON CUSTOMER_ORDER.PRODUCT_ID = PRODUCT.PRODUCT_ID

GROUP BY 1,2
```


## Setting up a database

*Always strive to gave a primary key on every table, which provides a unique identify to each record. Foreign key in a child table points to the primary key in its parent table.*
*A database schema is a diagram showing tables, their columns, and their relationships.*

### Creating tables


```r
#Autoincrement allows a unique number to be generated automatically when a new record is inserted into a table. Not needed for SQLite, but for MySQL and some other platforms.
#Allowing fields to be NULL can be done for fields that are not mandatory, while using NOT NULL should be done for fields that needs to be populated.

CREATE TABLE COMPANY (
COMPANY_ID             INTEGER PRIMARY KEY AUTOINCREMENT,
NAME                   VARCHAR(30) NOT NULL,
DESCRIPTION            VARCHAR(60),
PRIMARY_CONTACT_ID     INTEGER NOT NULL
);

CREATE TABLE ROOM (
ROOM_ID        INTEGER PRIMARY KEY AUTOINCREMENT,
FLOOR_NUMBE    INTEGER NOT NULL,
SEAT_CAPASITY  INTEGER NOT NULL
);

CREATE TABLE PRESENTATION (
PRESENTATION_ID   INTEGER PRIMARY KEY AUTOINCREMENT,
BOOKED_COMPANY_ID INTEGER NOT NULL,
BOOK_ROOM_ID      INTEGER NOT NULL,
START_TIME        TIME,
END_TIME          TIME
);

CREATE TABLE ATTENDEE (
ATTENDEE_ID       INTEGER PRIMARY KEY AUTOINCREMENT,
FIRST_NAME        VARCHAR (30) NOT NULL,
LAST_NAME         VARCHAR (30) NOT NULL,
PHONE             INTEGER,
EMAIL             VARCHAR (30),
VIP               BOOLEAN DEFAULT(0) 'BOOLEAN MAKE DEFAULT FALSE (0)'
);

CREATE TABLE PRESENTAION_ATTENDANCE (
TICKET_id         INTEGER PRIMARY KEY AUTOINCREMENT,
PRESENTATION_ID   INTEGER,
ATTENDEE_ID       INTEGER
);

#Foreign keys in a child table should be tied to primary key of a parent table. Setting up foreign keys by double clicking on the varible to modify and link this to a primary key of a parent table.
```

### Creating views

*Creating views stores querys in the database so it can easily be called upon.*



```r
CREATE VIEW PRESENTATION_VW AS
    SELECT COMPANY.NAME AS BOOKED_COMPANY,
           ROOM.ROOM_ID AS ROOM_NUMBER,
           ROOM.FLOOR_NUMBE AS FLOOR,
           ROOM.SEAT_CAPASITY AS SEATS,
           START_TIME,
           END_TIME
      FROM PRESENTATION
           INNER JOIN
           COMPANY ON PRESENTATION.BOOKED_COMPANY_ID = COMPANY.COMPANY_ID
           INNER JOIN
           ROOM ON PRESENTATION.BOOK_ROOM_ID = ROOM.ROOM_ID;


# Queryn from a view

SELECT * FROM PRESENTATION_VW
WHERE SEAT_CAPACITY >= 30
```

### Populating database

#### INSERT



```r
INSERT INTO ATTENDEE (FIRST_NAME, LAST_NAME)
VALUES ('THOMAS', 'NIELD')

SELECT * FROM ATTENDEE

#Multiple inserts. Can be used with Python to populate large amounts of data.

INSERT INTO ATTENDEE (FIRST_NAME, LAST_NAME, PHONE, EMAIL, VIP)
VALUES
('Jon', 'Skeeter', 3525253523, 'john.skeeter@rex.net', 1),
('Sam', 'Jones', 25235235235, 'sam.jones@hej.se', 0),
('Karl', 'Andersson', 5235235, 'karl.andersson@gmail.com',1)

#Inserting with a SELECT from another table

INSERT INTO ATTENDEE (FIRST_NAME, LAST_NAME, PHONE, EMAIL)
SELECT FIRST_NAME, LAST_NAME, PHONE, EMAIL
FROM ANOTHER_TABLE
```

#### DELETE

*Combining DELETE with WHERE. It could be good to replace DELETE with SELECT to get a preview what will be deleted.*


```r
DELETE FROM ATTENDEE
WHERE PHONE IS NULL
AND EMAIL IS NULL

# On some platforms the preferred way to delete all records from a table is to use TRUNCATE TABLE.
```

#### UPDATE

*UPDATE modifies existing records.*


```r
UPDATE ATTENDEE SET FIRST_NAME = UPPER(FIRST_NAME),
LAST_NAME = UPPER(LAST_NAME)

UPDATE ATTENDEE SET VIP = 1
WHERE ATTENDEE_ID IN (3,4)
```

### Connecting to database from R

*Connecting to the database from R.*



```r
#https://db.rstudio.com/dplyr/

library(dplyr)
library(DBI)
library(RSQLite)
con <- dbConnect(SQLite(), "C:\\Users\\chris\\Documents\\oreilly_getting_started_with_sql-master\\surgetech_conferencev2.db")

#Show list of table
as.data.frame(dbListTables(con))

#get data in data frame with sql syntax
df <- data.frame(tbl(con, sql("SELECT * FROM ATTENDEE")))

#with dplyr syntax
df2 <- data.frame(tbl(con, "ATTENDEE"))

# When data is fetched, disconnect to db.
dbDisconnect(con)
```

## Useful expressions


| Operator    | Description               | Example | 
| :----------- | :----------------------: | -----------: |
| abs()      | Calculates the absolute value | abs(x) |
| coalesce() | Converts a possible null value into a default | coalesce(z,y)  |
| instr()    | Checks if a text string contains another text string | instr('HTX', 'TX') |
| length()   | Provides the number of characters in a string | length('Test') |
| trim()     | Removes extraneous on both sides of a string | trim(' Test ') |
| ltrim()     | Removes extraneous on the left side of a string | ltrim(' Test') |
| rtrim()     | Removes extraneous on the right side of a string | rtrim('Test ') |
| random()     | Returns a random number | random() |
| round()     | Rounds a decimal to specified number | round(100.101, 2) |
| replace()     | Replaces a substring of text in a string | replace('Test df', ' df', '') |
| substr()     | Extracts a range of characters from a string with their num position | substr('Test', 2,3) |
| lower()     | Turns all letters in a string to lowercase | lower('Test') |
| upper()     | Turns all letters in a string to uppercase | upper('Test') |



## SQL databases and R


### Connecting to a database from R

*Access a database from R.*
*The problem with dplyr is that all operations are conducted in-memory and thus the amount of data you can work with is limited by available memory. The database connection essentially removes that limitation.* 
*Accessing a temp database by downloading it. Dplyr and dbplyr are used in R to point to the database.*




```r
#https://datacarpentry.org/R-ecology-lesson/05-r-and-databases.html
library("RSQLite")
library("plyr")
library("dbplyr")

# Downloading database for test
dir.create("data_raw", showWarnings = FALSE)
download.file(url = "https://ndownloader.figshare.com/files/2292171",
              destfile = "data_raw/portal_mammals.sqlite", mode = "wb")

# Connect to databse
mammals <- DBI::dbConnect(RSQLite::SQLite(), "data_raw/portal_mammals.sqlite")

# Querying with SQL-syntax vs dplyr syntax

#sql
tbl(mammals, sql("SELECT year, species_id, plot_id FROM surveys"))

#dplyr
surveys <- tbl(mammals, "surveys")
surveys %>%
  select(year, species_id, plot_id)

#Checking head(surveys, n = 10) and nrow(surveys) we see that the  surveys at first glance looks like a data frame but there are some differences.*
head(surveys, n = 10)
nrow(surveys)

#The reason for this is that dplyr dosen´t see the full dataset, only what was asked for when the question in dplyr was translated into SQL.
```

### Running SQL syntax in R



```r
library(sqldf)
#https://dept.stat.lsa.umich.edu/~jerrick/courses/stat701/notes/sql.html#introduction

sqldf('SELECT age, circumference FROM Orange WHERE Tree = 1 ORDER BY circumference ASC')

sqldf("SELECT * FROM iris")

#example
data(BOD)
BOD

#Wildcard: used to extract everything
bod2 <- sqldf('SELECT * FROM BOD')
bod2

#LIMIT controls the number of results
sqldf('SELECT * FROM iris LIMIT 5')

#ORDER BY syntax: ORDER BY var1 {ASC/DESC}, var2 {ASC/DESC}
sqldf("SELECT * FROM Orange ORDER BY age ASC, circumference DESC LIMIT 5")

#Where can be used to add conditional statements
sqldf('SELECT demand FROM BOD WHERE Time < 3')

#WHERE with AND and OR
sqldf('SELECT * FROM rock WHERE (peri > 5000 AND shape < .05) OR perm > 1000')

#IN is used to similiar to %in%
sqldf('SELECT * FROM BOD WHERE Time IN (1,7)')
sqldf('SELECT * FROM BOD WHERE Time NOT IN (1,7)')

#LIKE weak expression command
sqldf('SELECT * FROM chickwts WHERE feed LIKE "%bean" LIMIT 5')
sqldf('SELECT * FROM chickwts WHERE feed NOT LIKE "%bean" LIMIT 5')

#Aggregated data: AVG, MEDIAN, MAX, MIN, SUM
sqldf("SELECT AVG(circumference) FROM Orange")

#SELECT COUNT
d <- data.frame(a = c(1,1,1), b = c(1,NA,NA))
d
sqldf("SELECT COUNT() as numrows FROM d")
sqldf("SELECT COUNT(b) FROM d")
```

<!--chapter:end:04-SQL.Rmd-->

# HTML, CSS and JavaScript


## HTML


*https://www.w3schools.com/tags/ref_byfunc.asp*

<style>
th {
  text-align: left;
}
</style>

<table class="a">

  <tr>
    <th>Tag</th>
    <th>Description</th>
  </tr>

    <tr>
    <td>!DOCTYPE</td>
    <td>Defines the document type</td>
  </tr>
  
  <tr>
    <td>html</td>
    <td>Defines an HTML document</td>
  </tr>
  
  <tr>
    <td>head</td>
    <td>Contains information for the document</td>
  </tr>
  
  <tr>
    <td>title</td>
    <td>Defines a title </td>
  </tr>

  <tr>
    <td>body</td>
    <td>Defines the documents body</td>
  </tr>
  
  <tr>
    <td>h1 to h6</td>
    <td>Defines headings</td>
  </tr>

  <tr>
    <td>p</td>
    <td>Defines a paragraph</td>
  </tr>

  <tr>
    <td>br</td>
    <td>Inserts a single break</td>
  </tr>
  
  <tr>
    <td>hr</td>
    <td>Defines a thematic change in the content</td>
  </tr>

  <tr>
    <td>span and div</td>
    <td>A div is a block-level element and a span is an inline element. Div should be used to wrap sections of a document, while span to wrap small portions of text, imgages etc.</td>
  </tr>

</table>




**HTML is made up of a structure of elements which acts like a set of instructions for a web browser.**

```html
<!-- Doctype declares which dialects of HTML that is used. In this case HTML5 -->
<!DOCTYPE HTML>

<!-- <html> is the parent element for the page. All other elements are nested within this. <html> always has two child elements: <head> and <body>.  -->
<html>

<!-- <head> element contains information about the web page such as its title, links to other resources etc. Nothing in the this element is visible on the page. -->

	<head>
		<title> Web Page Test</title>
		<link href="https://fonts.googleapis.com/css2?family=Lato:ital,wght@0,300;1,300&display=swap" rel="stylesheet">
		
	</head>

<!-- <body> element takes care of the actual visible content on the page. -->

	<body>
		<header>
			<nav>Home | About | Contact </nav>
			<hgroup>
				<h1>Web page test</h1>
				<h2>Not very complexed</h2>
			</hgroup>
		</header>

	</body>

</html>	

``` 


## CSS

*There are two ways to include CSS in the HTML document. Embedded style sheet is one where all of the selectors and style rules appear in the "head" element of the HTML. With an external style sheet, CSS is written in a separate file and then linked.*


```r
<head>
		<title> Web Page Test</title>
		<link href="https://fonts.googleapis.com/css2?family=Lato:ital,wght@0,300;1,300&display=swap" rel="stylesheet">
		<style>
			body {
			font-family:"Lato";
			}

			#wrapper {
				width: 80%;
				height: auto;
				margin-left: auto;
				margin-right: auto;
			}

			section {
				width: 70%;
				height: auto;
				float: left;
				}

			aside {
				width: 30%;
				float: right;
			}

			footer {
				width: 100%;
				height: auto;
				float:left;
			}

			h1 {
				font-weight: bold;
				font-size: 26px;
				color:#990000;
			}



			hgroup > h1 {
				margin-bottom: 3px;
			}


			#homepageImage {
				float:left;
				border-style:none;
			}
		</style>
	</head>
```

## JavaScript
*Sublime can be used as a starting IDE.*<br><br>
*Link scripts to page. Write the script in a separate file and save with a .js filename. The script is then lnked to a page using the src attribute of the script element.*

*https://www.fincher.org/tips/Languages/javascript.shtml*

### Operators

<style>
th {
  text-align: left;
}
</style>

<table class="d">
  <tr>
    <th>Operator</th>
    <th>Description</th>
    <th>Example</th>
  </tr>
  <tr>
    <td>??</td>
    <td>Returns true if both operands evaluate to true</td>
    <td>true && false (false)</td>
  </tr>
  <tr>
    <td>||</td>
    <td>Returns true if either operand evaluates to true, otherwise false</td>
    <td>if  (x == y || y > z)</td>
  </tr>
  <tr>
    <td>!</td>
    <td>Inverts the boolean value of the operand</td>
    <td>!true (false)</td>
  </tr>
</table>





### Keywords

**JavaScript defines a set of keywords known as reserved words, these include: break, case, catch, continue, default, delete, do, else, false, finally, for, function, if, in, instanceof, new, null, return, switch, this, throw, true, try, typeof, var, void, while, with.**


<style>
th {
  text-align: left;
}
</style>

<table class="d">
  <tr>
    <th>Keyword</th>
    <th>Description</th>
    <th>Example</th>
  </tr>
  
  <tr>
    <td>var</td>
    <td>The "var" keyword tells the browser to create variables.</td>
    <td>var x,y;</td>
  </tr>
  
  <tr>
    <td>this</td>
    <td>The "this" keyword refers to the object it belongs to. </td>
    <td>fullName : function() {
  return this.firstName + " " + this.lastName;
}</td>
  </tr>
  
   <tr>
    <td>const</td>
    <td>For varibles that don´t change, we use const.</td>
    <td>const taxRate = 0.3050</td>
  </tr>

</table>



### Anatomy

**All JavaScript instructions are contained within statements. Related statements can be grouped together into a block by wrapping the statement in braces. **

#### Conditional statements
  
```js

//Multiple statements
if(condition) {
  statement_1;
  statement_2;
  statement_3;
 }
 
//Else clause
 if(condition) {
  statement(s);
  } else {
  statement(s);
 }
 
 //Else if clause. Branching logical structure can get very complex.
 if(condition) {
  statement(s);
 } else if(condition) {
  statement(s);
 }
 
 //example
 
var balance = 400;
if(balance < 0.0){
	status= "bankrupt";
} else if(balance < 100.0) {
	status = "ok";
} else {
	status = "rich";
}
document.write("customer is " +status)


 
 //Switch statement
 switch(condition) {
  case label_1:
    statement(s);
    break;
   
   case label_2:
    statement(s);
    break;
   
   default:
    statement(s);
    break;
}

//switch examplevar flavor = "vanilla";
switch(flavor) {
   case "chocolate":
      document.write("I like chocolate too.");
      break;
   case "strawberry":
      document.write("Strawberry is for sissies.");
      break;
   case "vanilla":
      document.write("Vanilla is boring.");
      //no break statement so control will continue to the statement below
   default:
      document.write("Ice cream is cool.");
}

``` 



#### Loops
  
```js
//While loops
while(condition) {
  statement(s);
}

//example
var total = 1;
while(total < 100) {
   document.write(total+",");
   total = total * 2;
}


//Do while
do {
  statement(s);
} while(condition);

//For loops
for(initialize ; condition ; iteration) {
  statement(s);
}

//Go to (should be used carefully)
for(var i=0; i < 2; i++) {
   outerloop:
   for(var j=0; j < 10; j++) {    
      if(j > 3) {
         break outerloop;
      }
      document.write(" "+i+j+", ");
   }
}
``` 

*Loop: existing a loop*
```js
for(var i=0; i<100; i++) {
   document.write(i);
   if(i > 5) { break; }
}
```

### Functions and classes

#### Named and anonymous functions

*Using anonymous functions we use a variable name instead of a function name.*
```js
//Named function
function calculateRectArea(rectWidth, rectHeight) {
  return(rectWidth * rectheight)
}

//Anonymous function
var calculateRectHypotenuse = function(rectWidth, rectHeight) {
  var widthsquared = rectWidth * rectWidth;
  var heigthSquared = rectHeight * rectHeight;
  return(Math.sqrt(widthSquared + heightSquared));
 }
 
//calling
var myRectArea = calculateRectArea(49, 28);
var myRectHypotenuse = calculateRectHypotenuse(49,28);

``` 

*Anonymous function*

´´´html
<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8">
    		<title>A Simple Quiz</title>		
			<form action="#">
			<input type="button" value="Click Me" id="anonbutton" />
			</form>
				<script type="text/javascript">
						var anonbutton = document.getElementById("anonbutton");
					anonbutton.onclick = function() {
		    		alert("anonymous function called.");
					}
				</script>

  </head>

  <body>
  </body>

</html>

´´´

#### Namespace

*Namespaces are notional spaces or contexts in which a set of names can be defined, to avoid name clashed. Two names can be identical as long as they exist in different namespace.*

```js
//Creating a namespace in js script. It is a good practise to include your namespace declaration in top of all your scripts. Functions needs to be anonymous.
//First we create an object called com inside the global namespace, and then we create an object called flametreepublishing inside the com project.
// After declaring, but not initializing, we test that each namespace object does not exist before using it.


var com;
if(!com) {
	com = {};
}

if (!com.flametreepublishing) {
	com.flametreepublishing = {}
}


```


#### Object-Oriented

*While JavaScript is not a true OOP language, it is still used in a OOP style. We write scripts called classes that define properties.*

```js
//Example: Define a custom class, then create an instance.

var com;
if(!com) {
	com = {};
}
if(!com.flametreepublishing) {
	com.flametreepublishing = {};
}

com.flametreepublishing.QuizQuestion = functio(aQustionNum, AQuestionText, aAnswer, aCorrectAnswerIndex){
	this.questionNum = aQustionNum;
	this.questionText = AQuestionText;
	this.answers = aAnswer;
	this.correctAnswerIndex = aCorrectAnswerIndex;


//Creating an instance
new com.flametreepublishing.QuizQuestion(
	1,
	"Approx how far away from the Earth is the Sun?",
	["200 miles", "93 000 000 miles", "49 000 000 miles", "150 000 miles"],
	1
)
}

```



### Misc

*When JavaScript is referenced from a separate file, the use of async makes the page load ahead of the script.*

```js
<script async src="js/main.js"></script>
```

*Variables defines outsisde of a function are global variables, which can be accessed from any function. Local variables only live inside a function. If you forgets to preface with var, the variable becomes global.*

```js
var imAGlobalVar = 10;

function foo() {
   var imALocalVar = 11;  //local variable
   imAGlobalVar2 = 12;    //global variable, not good practice
}
```

**Associating functions with objects**
```html
<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8">
    		<title>A Simple Quiz</title>		

			<script type="text/javascript">
				function movie(title, director) {
					this.title = title;
					this.director = director;
					this.toString = function movieToString() {
						return("title: "+this.title+" director: "+this.director);

					}
				}
				var narnia = new movie("Narnia", "Andew Adamson");
				document.write(narnia.toString());
			</script>

  </head>

  <body>
  </body>

</html>
```

*Prototypes. Objects can have prototypes from which they may inherit fields and functions.*

```html
<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8">
    		<title>A Simple Quiz</title>		

			<script type="text/javascript">
function movieToString() {
   return("title: "+this.title+"  director: "+this.director);
   }
function movie(title, director) {
   this.title = title;
   this.director = director || "unknown"; //if null assign to "unknown"
   this.toString = movieToString; //assign function to this method pointer
   }
   
var officeSpace = new movie("OfficeSpace");
var narnia = new movie("Narnia","Andrew Adamson");
movie.prototype.isComedy = false; //add a field to the movie's prototype
document.write(narnia.toString());
document.write("<br />Narnia a comedy? "+narnia.isComedy);
officeSpace.isComedy = true; //override the default just for this object
document.write("<br />Office Space a comedy? "+officeSpace.isComedy);
			</script>

  </head>

  <body>
  </body>

</html>
```

**Error handling**

```js
try {
   obj = null;
   null.to_s();
} catch (e) {
   document.write("Exception: "+e);
} finally {
   document.write("<br />Bye.");
}
```

**To execute a method repeatedly**
```js
function timer(){
     setTimeout('myMethod()',2000);
}

var myId;
...
myId = setInterval('myMethod()',2000);
```

**To close a window**
```html
<a href='javascript:window.close()' class='mainnav'> Close </a>
  </body>
```

**Link**
```html
<a href="https://www.espn.com/nba/">NBA is fun!</a>
```


### Working with HTML Elements

**Buttons**

```html
<form name="buttonGalore" method="get">
Your Name: <input type="text" name="mytext"/>
<br/>
<input type="submit" value ="GO!" />
<input type = "reset" value ="Clear All"/>
</form>
```


**Onclick**
```html
<form  name="buttonsGalore" method="get">
<fieldset style="margin: 1em; text-align: center; padding: 1em;">
<legend>Select a Movie</legend>
<input type="button" value="Godfather" onclick="displayMovie(this)" />
<input type="button" value="Bodyguard" onclick="displayMovie(this)" />
<input type="button" value="Remember the Titans" onclick="displayMovie(this)" />
</fieldset>
</form>

```

**Radio button**
```html
<script>
function findButton() {
var myForm = document.forms.animalForm;
var i;
for(i=0;i<myForm.marsupial.length; i++) {
   if(myForm.marsupial[i].checked) {
      break;
   }
}
alert("You selected \""+myForm.marsupial[i].value+"\".");
}
</script>
<form name="animalForm">
<input type="radio" name="marsupial" value="kangaroo" />Kangaroo
<br /><input type="radio" name="marsupial" value="Opossum" />Opossum
<br /><input type="radio" name="marsupial" value="Tasmanian Tiger" />Tasmanian Tiger
<br />
<input type="button" name="GO" value="GO" onclick="findButton()" />
</form>
```

**Accessing Elements**

```html
<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8">
    		<title>Testing</title>		
		<script type="text/javascript" src="js/main2.js"></script>

<form name="teamForm">
<select name="team">
   <option selected="selected">Select NBA team to win it</option>
   <option>Lakers</option>
   <option>Utah</option>
   <option>Golden State</option>
   <option>Chicago</option>
   <option>Charlotte</option>
</select>

<input type="button" name="submitbutton" value="Team"  
        onclick="showStatus()" />
</form>

  </head>

  <body>


  </body>

</html>

```

```js
function showStatus() {
 var selectWidget = document.forms.teamForm.elements["team"];
 var myValue = selectWidget.options[selectWidget.selectedIndex].value;
  alert('You picked \"'+ myValue +"\"");
  return true;
}
```

#### Dynamic HTML elements

**Creating a grocery store list**
```html
<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8">
    		<title>Testing</title>		

<!-- Adding new elements to a list -->
<script>
function addItem() {
 var myitem = document.getElementById("ItemToAdd").value;
 var mylistItems = document.getElementById("mylist");
 var newP = document.createElement("li");
 var textNode = document.createTextNode(myitem);
 newP.appendChild(textNode);
 document.getElementById("mylist").appendChild(newP);
 return false;
}
</script>
<form onsubmit="return addItem()" action="#">
<span>Grocery Items:</span>
<input type="text" id="ItemToAdd" value="Milk" />
<input type="button" value="Add" onclick="addItem()" />
</form>
<span>Grocery List:</span> 
<ol id="mylist"></ol>

 </head>

  <body>
  </body>

</html>
```



### DOM and events of JS

#### DOM programming
*Document Object Model programming allows JavaScript to make changes to a page after it has been loaded.*

```js


//We've updated substantially the renderQuestion method. Now, rather than
//relying on document.write, which only works when the page is being loaded,
//we're now using DOM programming techniques to create HTML elements 
//on-the-fly from within JavaScript.

com.flametreepublishing.QuizQuestion.prototype.renderQuestion = function() {

	//First we create a <div> element in which to store the question's
	//content. This <div> is created but does not yet exist on the page
	
	var questionDiv = document.createElement("div");
	
	//We'll set an id attribute on the <div> - later, this will help us to
	//identify which question a user clicks on. We'll give the id a leading
	//'q' because it is bad practice to start an id value with a number.
	
	questionDiv.id = "q" + this.questionNum;
	
	//Now we create an <h2> element for the question's title
	
	var questionHeading = document.createElement("h2");
	
	//An element's innerHtml property allows us to write HTML that will be
	//rendered within that element...
	
	questionHeading.innerHTML = "QUESTION " + this.questionNum;
	
	//Now we add the <h2> to the <div>
	
	questionDiv.appendChild(questionHeading);
	
	//Next, we create a <p> to hold the question text itself, and add
	//this to the <div> too.
	
	var questionTextPara = document.createElement("p");
	
	questionTextPara.innerHTML = this.questionText;
	
	questionDiv.appendChild(questionTextPara);

	//Now we'll loop through the QuizQuestion object's 'answers'
	//array, creating a <p> for each and and adding them 
	//to our <div> element

	for(var i = 0; i < this.answers.length; i++) {
		var answerPara = document.createElement("p");
		answerPara.innerHTML = this.answers[i];
		answerPara.id = "a" + i;
		questionDiv.appendChild(answerPara);		
	}
	
//Finally, we add the <div> to the body of the page

	document.body.appendChild(questionDiv);	
}
```
#### Events

**Event Handler**

*W3C event model*


```js

//Create a method for handling user 'click' events

com.flametreepublishing.SimpleQuiz.prototype.clickHandler = function(e) {

	//The handler will always be passed an objec that contains data
	//about the event that triggered the handler. We're using an identifier
	//of 'e' for this. With a 'click' event, 'e.target' will always refer to 
	//the HTML element on which the click event occurred.
	//First we'll get the id attribute of the clicked answer
	
	var clickedAnswerId = e.target.id;
	
	//Now we need to extract the answer index, a Number, from the id value,
	//which is a string. We do this by extracting the second character of the
	//id using the 'substr' method of the String class, and then casting this to
	//a number. Notice that we can do this all within one compound expression.
	
	var clickedAnswerIndex = Number(clickedAnswerId.substr(1, 1));
	
	//Next we need to know which question has been answered. Recall that we added
	//an id attribute to the <div> that contains the question - this <div> is the
	//parent of the answer <p> that was clicked, so we can access it using
	//'e.target.parentNode'.
	//Once we have a reference to the <div> we can extract the question index in
	//much the same way as we did the answer index
	
	var clickedQuestionId = e.target.parentNode.id;
	var clickedQuestionNum = Number(clickedQuestionId.substr(1, 1));
	
	//The question number stored in the containg <div> id attribute is one-based, but
	//we need a zero-based number when accessing the questions array - for this reason
	//we subtract '1' from the clickedQuestionNum when retrieving the QuizQuestion object
	
	var clickedQuestion = com.flametreepublishing.simpleQuiz.questions[clickedQuestionNum -1];
	
	//Now that we have the correct QuizQuestion object we can call its 'checkUserAnswer'
	//method to see if the user was correct. Recall that 'checkUserAnswer' returns
	//'true' or 'false' - all we need do, then, is call the method as the conditional 
	//expression of an 'if' statement.
	
	if(clickedQuestion.checkUserAnswer(clickedAnswerIndex)) {
	
		alert("Correct! Well done.");
		
	} else {
	
		alert("No - that's not correct. Try again.");
		
	}	
}
	
```

**Event Listener**

```js
//The user will click on the answer they believe to be correct, so let's
//add an event listener to the answer's <p>. Note that we don't include
//parentheses '()' after the handler method reference - if we did then
//the handler would be evaluated when the event listener is added to the
//<p>, and this is definitely NOT what we want!

answerPara.addEventListener("click", com.flametreepublishing.simpleQuiz.clickHandler, false)
```

<!--chapter:end:05-HTML_CSS_JS.RMD-->

