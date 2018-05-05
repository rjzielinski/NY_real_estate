---
output:
  html_document: default
  pdf_document: default
---
Group M: Alex Chaffers, Robbie Zelinski 

Neccessary packages

```{r}
library(acs)
library(RMySQL)
library(ggmap) 
library(operators)
library(tidyverse)
library(pbapply)
library(tcltk)
library(lubridate)
library(doParallel)
```
ACS data wrangling 

Establishg specfic counties of interest for ACS data

```{r}
cs_key <- "3f58748f63ce427e4133d32fd5b094e102b4ec31"
big_geo <- geo.make(state = 36, county = c("Albany", "Allegany", "Broome",
                                           "Cattaraugus", "Cayuga",
                                           "Chautauqua", "Chemung", "Chenango",
                                           "Clinton", "Columbia", "Cortland",
                                           "Delaware", "Dutchess", "Erie",
                                           "Essex", "Franklin", "Fulton",
                                           "Genesee", "Greene", "Hamilton",
                                           "Herkimer", "Jefferson", "Lewis",
                                           "Livingston", "Madison", "Monroe",
                                           "Montgomery", "Nassau", "Niagara",
                                           "Oneida", "Onondaga", "Ontario",
                                           "Orange", "Orleans", "Oswego",
                                           "Otsego", "Putnam", "Rensselaer",
                                           "Rockland", "St. Lawrence", "Saratoga",
                                           "Schenectady", "Schoharie", "Schuyler",
                                           "Seneca", "Steuben", "Suffolk",
                                           "Sullivan", "Tioga", "Tompkins",
                                           "Ulster", "Warren", "Washington",
                                           "Wayne", "Westchester", "Wyoming",
                                           "Yates"))
```                                           

Gathering population data for counties of interest
``` {r}
bigPop_2015 <- acs.fetch(geo = big_geo, endyear = 2015,
                         table.number ="B01003", key = acs_key)
bigPop_2014 <- acs.fetch(geo = big_geo, endyear = 2014,
                         table.number ="B01003", key = acs_key)
bigPop_2013 <- acs.fetch(geo = big_geo, endyear = 2013,
                         table.number ="B01003", key = acs_key)
bigPop_2012 <- acs.fetch(geo = big_geo, endyear = 2012,
                         table.number ="B01003", key = acs_key)
bigPop_2011 <- acs.fetch(geo = big_geo, endyear = 2011,
                         table.number ="B01003", key = acs_key)
bigPop_2010 <- acs.fetch(geo = big_geo, endyear = 2010,
                         table.number ="B01003", key = acs_key)
bigPop_2009 <- acs.fetch(geo = big_geo, endyear = 2009,
                         table.number ="B01003", key = acs_key)
```
Gathering median income data from ACS for counites of interest
``` {r}
bigIncome_2015 <- acs.fetch(geo = big_geo, endyear = 2015,
                            table.number =  "B19013", key = acs_key)
bigIncome_2014 <- acs.fetch(geo = big_geo, endyear = 2014,
                            table.number =  "B19013", key = acs_key)
bigIncome_2013 <- acs.fetch(geo = big_geo, endyear = 2013,
                            table.number =  "B19013", key = acs_key)
bigIncome_2012 <- acs.fetch(geo = big_geo, endyear = 2012,
                            table.number =  "B19013", key = acs_key)
bigIncome_2011 <- acs.fetch(geo = big_geo, endyear = 2011,
                            table.number =  "B19013", key = acs_key)
bigIncome_2010 <- acs.fetch(geo = big_geo, endyear = 2010,
                            table.number =  "B19013", key = acs_key)
bigIncome_2009 <- acs.fetch(geo = big_geo, endyear = 2009,
                            table.number =  "B19013", key = acs_key)
```

Converting the population data from the ACS into a data frame to make it more easier to work with                           

``` {r}
pop_2015 <- as.data.frame(bigPop_2015@estimate) %>% 
  mutate(county_full = rownames(.),
         county = str_replace(county_full,"(.+) County.*","\\1")) %>% 
  select(county, B01003_001) %>% 
  rename("population_2015" = B01003_001) 

pop_2014 <- as.data.frame(bigPop_2014@estimate) %>% 
  mutate(county_full = rownames(.),
         county = str_replace(county_full,"(.+) County.*","\\1")) %>% 
  select(county, B01003_001) %>% 
  rename("population_2014" = B01003_001) 

pop_2013 <- as.data.frame(bigPop_2013@estimate) %>% 
  mutate(county_full = rownames(.),
         county = str_replace(county_full,"(.+) County.*","\\1")) %>% 
  select(county, B01003_001) %>% 
  rename("population_2013" = B01003_001) 

pop_2012 <- as.data.frame(bigPop_2012@estimate) %>% 
  mutate(county_full = rownames(.),
         county = str_replace(county_full,"(.+) County.*","\\1")) %>% 
  select(county, B01003_001) %>% 
  rename("population_2012" = B01003_001) 

pop_2011 <- as.data.frame(bigPop_2011@estimate) %>% 
  mutate(county_full = rownames(.),
         county = str_replace(county_full,"(.+) County.*","\\1")) %>% 
  select(county, B01003_001) %>% 
  rename("population_2011" = B01003_001) 

pop_2010 <- as.data.frame(bigPop_2010@estimate) %>% 
  mutate(county_full = rownames(.),
         county = str_replace(county_full,"(.+) County.*","\\1")) %>% 
  select(county, B01003_001) %>% 
  rename("population_2010" = B01003_001) 

pop_2009 <- as.data.frame(bigPop_2009@estimate) %>% 
  mutate(county_full = rownames(.),
         county = str_replace(county_full,"(.+) County.*","\\1")) %>% 
  select(county, B01003_001) %>% 
  rename("population_2009" = B01003_001) 
```  
Merging population data into a single data frame 
``` {r}
acs_pop <- pop_2015 %>%
  inner_join(pop_2014, by = "county") %>%
  inner_join(pop_2013, by = "county") %>%
  inner_join(pop_2012, by = "county") %>%
  inner_join(pop_2011, by = "county") %>%
  inner_join(pop_2010, by = "county") %>%
  inner_join(pop_2009, by = "county") %>% 
  gather("year", "population", 2:8) %>%
  mutate(year = gsub("population_", "", year)) %>%
  mutate(year = as.numeric(year))  
```
Converting the income data from the ACS into a data frame to make it more easier to work with
``` {r}
income_2015 <- as.data.frame(bigIncome_2015@estimate) %>% 
  mutate(county_full = rownames(.),
         county = str_replace(county_full,"(.+) County.*","\\1")) %>% 
  select(county,B19013_001) %>% 
  rename("income_2015" = B19013_001)

income_2014 <- as.data.frame(bigIncome_2014@estimate) %>% 
  mutate(county_full = rownames(.),
         county = str_replace(county_full,"(.+) County.*","\\1")) %>% 
  select(county,B19013_001) %>% 
  rename("income_2014" = B19013_001)

income_2013 <- as.data.frame(bigIncome_2013@estimate) %>% 
  mutate(county_full = rownames(.),
         county = str_replace(county_full,"(.+) County.*","\\1")) %>% 
  select(county,B19013_001) %>% 
  rename("income_2013" = B19013_001)

income_2012 <- as.data.frame(bigIncome_2012@estimate) %>% 
  mutate(county_full = rownames(.),
         county = str_replace(county_full,"(.+) County.*","\\1")) %>% 
  select(county,B19013_001) %>% 
  rename("income_2012" = B19013_001)

income_2011 <- as.data.frame(bigIncome_2011@estimate) %>% 
  mutate(county_full = rownames(.),
         county = str_replace(county_full,"(.+) County.*","\\1")) %>% 
  select(county,B19013_001) %>% 
  rename("income_2011" = B19013_001)

income_2010 <- as.data.frame(bigIncome_2010@estimate) %>% 
  mutate(county_full = rownames(.),
         county = str_replace(county_full,"(.+) County.*","\\1")) %>% 
  select(county,B19013_001) %>% 
  rename("income_2010" = B19013_001)

income_2009 <- as.data.frame(bigIncome_2009@estimate) %>% 
  mutate(county_full = rownames(.),
         county = str_replace(county_full,"(.+) County.*","\\1")) %>% 
  select(county,B19013_001) %>% 
  rename("income_2009" = B19013_001)
 ``` 
 
Merging ACS income data into a single data frame 

``` {r} 
 acs_income <- income_2015 %>%
  inner_join(income_2014, by = "county") %>%
  inner_join(income_2013, by = "county") %>%
  inner_join(income_2012, by = "county") %>%
  inner_join(income_2011, by = "county") %>%
  inner_join(income_2010, by = "county") %>%
  inner_join(income_2009, by = "county") %>%
  gather("year", "income", 2:8) %>%
  mutate(year = gsub("income_", "", year)) %>%
  mutate(year = as.numeric(year))
```
Merging income and population ACS data into a single data frame 

``` {r}
acs <- acs_pop %>%
  inner_join(acs_income, by = c("county", "year"))

names(acs)[1] <- "County"
```
Saving dataframe to file Rda file that a can be accessed by a shiny app

```{r}
save(acs, file="acs.Rda")
```

Web Scraper 

Web scraper that collected real estate transaction data from  http://rochester.nydatabases.com/database/real-estate-database 

code below imported necessary libaries from python and set url variable. 
```{python}
import time
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import TimeoutException
from selenium.common.exceptions import StaleElementReferenceException
import MySQLdb

url = 'http://rochester.nydatabases.com/database/real-estate-database'
countyName = ""
```
Functions to start web browser and define and open web page 

```{python}
def init_driver():
    driver = webdriver.Firefox()
    driver.wait = WebDriverWait(driver, 5)
    return driver

def load(driver):
    driver.get(url)
    try:
        button = driver.wait.until(EC.element_to_be_clickable(
            (By.ID, "search")))
        button.click()
    except TimeoutException:
        print("Button not found")
```
```{python}
def nextPage(driver, current):
    try:
        button = driver.wait.until(EC.element_to_be_clickable(
            (By.XPATH, '//span[@start={0}]'.format(str(current)))))
        button.click()
        time.sleep(4)
        return(current+1)
    except TimeoutException:
        print("No more pages found")
        return(current)

def newYear(driver, current):
    try:
        button = driver.wait.until(EC.element_to_be_clickable(
            (By.XPATH, '//select[@id="sYear"]/option[@value={0}]'.format(str(current-1)))))
        button.click()
        search = driver.wait.until(EC.element_to_be_clickable(
            (By.ID, "search")))
        search.click()
        time.sleep(4)
        return(current-1)
    except TimeoutException:
        print("No more years")
        return(current)

def newCounty(driver, current):
    try:
        counties = driver.find_elements_by_xpath('//select[@id="cnty"]')
        for county in counties:
            names = county.find_elements_by_tag_name('option')
            global countyName
            countyName = [name.text for name in names][current-1]
        button = driver.wait.until(EC.element_to_be_clickable(
            (By.XPATH, '//select[@id="cnty"]/option[@value={0}]'.format(str(current)))))
        button.click()
        search = driver.wait.until(EC.element_to_be_clickable(
            (By.ID, "search")))
        search.click()
        time.sleep(4)
        return(current+1)
    except TimeoutException:
        print("No more counties")
        return(current)
```

functions to change page in table change year, and county we were filtering by

```{python}
if __name__ == "__main__":
    driver = init_driver()
    load(driver)
    time.sleep(5)

    county = newCounty(driver, 5)

    db = MySQLdb.connect(user="root", passwd="stat231project", db="nyhousing", host="localhost")
    cur = db.cursor()

    db.set_character_set('utf8')
    cur.execute('SET NAMES utf8;')
    cur.execute('SET CHARACTER SET utf8;')
    cur.execute('SET character_set_connection=utf8;')

    while True:
        curyear = newYear(driver, 2018)
        if(county == 6):
            curyear = newYear(driver, 2014)
        while True:
            pagenum = 1
            while True:
                data = []
                trs = driver.find_elements_by_tag_name('tr')
                if trs:
                    for tr in trs:
                        tds = tr.find_elements_by_tag_name('td')
                        if tds:
                            text = []
                            text.append(countyName)
                            for td in tds:
                                text.append(td.text)
                            data.append(text)
                cur.executemany("INSERT INTO transactions (County, Town, Address, ZipCode, SalePrice, SaleDate, SellerName, BuyerName) VALUES (%s, %s, %s, %s, %s, %s, %s, %s)", data)
                print "County: {} Year: {} Page: {}".format(county, curyear, pagenum)
                newpage = nextPage(driver, pagenum)
                if (newpage > pagenum):
                    pagenum = newpage
                else:
                    break
            db.commit()
            newyear = newYear(driver, curyear)
            if (newyear < curyear):
                curyear = newyear
            else:
                break
        newcounty = newCounty(driver, county)
        if(newcounty > county):
            county = newcounty
        else:
            break


    driver.quit()
    cur.close()
    db.close()
```
Geocoding 

Creates data frame with address as single variable to allow geocoding of real estate transactions 
```{r}
db <- dbConnect(MySQL(), user = 'stat231', password = 'stat231project', dbname = "nyhousing", host = '148.85.253.214')
transactions_df <- dbReadTable(db, "transactions")
transactions_df <- transactions_df %>%
  mutate(address_full = paste(Address, Town, "NY", sep = ", "))
```

Function to geocode address from real estate database. Contains functions to run geocoding in parallel, and to handle invalid addresses
```{r}
no_cores <- detectCores() - 1
registerDoParallel(cores=no_cores)  
cluster <- makeCluster(no_cores, type="FORK")

full_address <- transactions_df %>%
  select(address_full) %>%
  unlist() %>%
  as.vector()

geocodes <- pblapply(full_address, function(x) tryCatch(geocode(x, output = "latlon", messaging = FALSE, source = "dsk"), error = function(e) data.frame(lon = NA, lat = NA)), cl = cluster)  
stopCluster(cluster)

full_address <- data.frame(full_address)
names(full_address) <- c("address_full")
geocodes <- unlist(geocodes) %>%
  matrix(ncol = 2, byrow = TRUE) %>%
  data.frame
names(geocodes) <- c("lon", "lat")
geocodes <- bind_cols(full_address, geocodes) %>%
  distinct()
geocodes$address_full <- as.character(geocodes$address_full)
transactions2 <- transactions_df %>%
  left_join(geocodes, by = "address_full")

full_address <- NULL
transactions_df <- NULL
geocodes <- NULL
```
Data table with longitude and latitude boundries for our counties of interest in order to help filter out uneeded coordinates 

```{r}
counties <- c("Albany", "Allegany", "Broome", "Cattaraugus", "Cayuga", 
              "Chautauqua", "Chemung", "Chenango", "Clinton", "Columbia",
              "Cortland", "Delaware", "Dutchess", "Erie", "Essex", "Franklin",
              "Fulton", "Genesee", "Greene", "Hamilton", "Herkimer", "Jefferson",
              "Lewis", "Livingston", "Madison", "Monroe", "Montgomery",
              "Nassau", "Niagara", "Oneida", "Onondaga", "Ontario", "Orange",
              "Orleans", "Oswego", "Otsego", "Putnam", "Rensselaer", "Rockland",
              "St. Lawrence", "Saratoga", "Schenectady", "Schoharie",
              "Schuyler", "Seneca", "Steuben", "Suffolk", "Sullivan",
              "Tioga", "Tompkins", "Ulster", "Warren", "Washington", "Wayne",
              "Westchester", "Wyoming", "Yates")
              
long_min <- c(-74.3, -78.3, -76.2, -79.1, -76.8, -79.8, -77.0, -75.9, -74.0, -74.0,
              -76.3, -75.5, -74.0, -79.2, -74.4, -74.8, -74.8, -78.5, -74.6, -74.9,
              -75.3, -76.5, -75.9, -78.1, -76.0, -78.0, -74.8, -73.8, -79.1, -75.9,
              -76.5, -77.7, -74.8, -78.5, -76.7, -75.5, -74.0, -73.8, -74.3, -75.9, 
              -74.2, -74.4, -74.8, -77.1, -77.0, -77.8, -73.5, -75.2, -76.6, -76.7,
              -74.8, -74.3, -73.7, -77.4, -74.0, -78.5, -77.4)

long_max <- c(-73.6, -77.7, -75.3, -78.3, -76.2, -79.0, -76.5, -75.3, -73.3, -73.3,
              -75.8, -74.4, -73.4, -78.4, -73.3, -73.9, -74.1, -77.9, -73.7, -74.0,
              -74.6, -75.4, -75.1, -77.4, -75.2, -77.3, -74.0, -73.4, -78.4, -75.0,
              -75.8, -76.9, -73.9, -77.9, -75.7, -74.6, -73.5, -73.2, -73.9, -74.5,
              -73.5, -73.8, -74.1, -76.6, -76.6, -76.9, -71.8, -74.3, -76.0, -76.2,
              -73.9, -73.4, -73.2, -76.7, -73.4, -77.9, -76.9)

lat_min <- c(42.4, 42.0, 42.0, 42.0, 42.6, 42.0, 42.0, 42.2, 44.4, 41.9, 42.4, 41.8,
             41.4, 42.4, 43.7, 44.0, 42.9, 42.8, 42.0, 43.2, 42.8, 43.6, 43.4, 42.4,
             42.7, 42.9, 42.7, 40.5, 43.0, 42.8, 42.7, 42.5, 41.1, 43.1, 43.1, 42.3,
             41.3, 42.4, 40.9, 44.0, 42.7, 42.7, 42.3, 42.2, 42.5, 42.0, 40.6, 41.4,
             42.0, 42.2, 41.5, 43.2, 42.9, 43.0, 40.8, 42.5, 42.4)

lat_max <- c(42.9, 42.6, 42.5, 42.6, 43.5, 42.6, 42.3, 42.8, 45.0, 42.5, 42.8, 42.6,
             42.1, 43.1, 44.6, 45.0, 43.3, 43.2, 42.5, 44.2, 44.1, 44.4, 44.3, 43.0,
             43.2, 43.4, 43.1, 41.0, 43.4, 43.6, 43.3, 43.0, 41.7, 43.4, 43.7, 42.9,
             41.6, 43.0, 41.4, 45.1, 43.4, 43.0, 42.9, 42.6, 43.1, 42.6, 41.3, 42.0,
             42.5, 42.7, 42.2, 43.8, 43.8, 43.4, 41.4, 42.9, 42.8) 
             
counties_lat_lon <- data.frame(counties, long_min, long_max, lat_min, lat_max)
```
Filtering out coordinates that were outside our counties of interest from our data frame

```{r}
transactions2 <- transactions2 %>%
  left_join(counties_lat_lon, by = c("County" = "counties"))
  
transactions2 <- transactions2 %>%
  mutate(lon = ifelse((lon > long_min & lon < long_max & lat > lat_min & lat < lat_max), lon, NA),
         lat = ifelse((lon > long_min & lon < long_max & lat > lat_min & lat < lat_max), lat, NA))
```
Convert sale price into numeric and select variables needed for further analsyis and created seperate sql table called transactions2

```{r}
transactions2 <- transactions2 %>%
  mutate(SalePrice = gsub(",", "", SalePrice),
         FullSaleDate = SaleDate) %>%
  separate(FullSaleDate, c("month", "day", "year"), sep = "/")
  
transactions2 <- transactions2 %>%
  select(County, Town, Address, ZipCode, SalePrice, SaleDate, SellerName, BuyerName,
         id, address_full, lon, lat, month, day, year)

dbWriteTable(db, "transactions2", transactions2, overwrite = TRUE)
```
# injesting data 

Established connection with sql server in order to injest housing data 

``` {r}
db <- dbConnect(MySQL(), user = 'stat231', password = 'stat231project', dbname = "nyhousing", host = '148.85.253.214')
transactions <- tbl(db, "transactions2")
```
Pulling transactions data frame from sql server and converting data to the right format

```{r}
transactions <- collect(transactions)
transactions <- transactions %>%
mutate(SaleDate = mdy(SaleDate),
month = as.numeric(month),
day = as.numeric(day),
year = as.numeric(year),
SalePrice = as.numeric(SalePrice),
Long = lon,
Lat = lat)
```
Selecting variables to include in our final analysis 
```{r}
transactions <- transactions %>%
select(County, Town, Address, ZipCode, SalePrice, SaleDate, SellerName, BuyerName,
id, address_full, Long, Lat, month, day, year)
```
Merging real estate and ACS data and saving it to be accessable in our Shiny app

```{r}
load("acs.Rda")

transactions <- transactions %>%
  left_join(acs, by = c("County", "year"))
```

```{r}
save(transactions, file="nyhousing_transactions.Rda")
```
Code for the final Shiny App
```{r}
load("nyhousing_transactions.Rda")

transactions <- transactions %>%
  select(year, month, day, County, Address, Town, ZipCode, SalePrice, Long, Lat) %>%
  mutate(County = as.factor(County),
         Town = as.factor(Town),
         ZipCode = as.factor(ZipCode),
         SalePrice = as.numeric(SalePrice),
         month = as.numeric(month),
         day = as.numeric(day),
         year = as.numeric(year))

# create codebook
description <- c("Year", "Month", "Day", "County", "Street Address", "Town Name", 
                 "Zip Code", "Sale Price", "Longitude", "Latitude")
codebook <- data.frame(name=names(transactions), description)
names(codebook) <- c("Variable", "Variable Description")

pal <- colorNumeric(
  palette = "YlOrRd",
  domain = log(transactions$SalePrice))





# UI
ui <- fluidPage(
  titlePanel("Map of Transactions"),
  sidebarLayout(
    
    # Input(s)
    sidebarPanel(
      selectInput("transactionyear", "Year of Transaction", choices = 1993:2017, selected = 2017),
      checkboxGroupInput("transactioncounties", "Counties:", choices = c("All", unique(sort(as.character(transactions$County)))),
                         selected = "All")
      #checkboxInput("crosslistedenrollment", "Include Cross-Listed Enrollments", TRUE)
      #checkboxGroupInput("subject", "Departments",
      #                   choices = unique(sort(as.character(fakeenrolls$subject))),
      #                   selected = unique(sort(as.character(fakeenrolls$subject)))[1]
      #),
      #checkboxGroupInput("type", "Type of course",
      #                   choices = unique(sort(as.character(fakeenrolls$type))),
      #                   selected = unique(sort(as.character(fakeenrolls$type)))
      #)
    ),
    # Output(s)
    mainPanel(
      
      tabsetPanel(id = "tabspanel", type = "tabs",
                  tabPanel(title = "Data Table", 
                           br(),
                           DT::dataTableOutput(outputId = "transactionstable")),
                  tabPanel(title = "Map of Real Estate Transactions", 
                           leafletOutput(outputId = "map"),
                           br(),
                           h4(uiOutput(outputId = "n1"))),
                  #tabPanel(title = "Course Enrollments (by dept and level)", 
                  #         plotOutput(outputId = "boxplot2"),
                  #         br(),
                  #         h4(uiOutput(outputId = "n2"))),
                  # New tab panel for Codebook
                  tabPanel("Codebook", 
                           br(),
                           DT::dataTableOutput(outputId = "codebook"))
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  # Create reactive data frame
  sales <- reactive({
    newval <- transactions %>%
      filter(year == input$transactionyear)
    
    if("All" %in% input$transactioncounties){
      
    } else{
      newval <- newval %>%
        filter(County %in% input$transactioncounties)
    }
    #if (input$crosslistedenrollment) {
    #  newval <- newval %>%
    #    mutate(displayenroll = totalenroll)
    #} else {
    #  newval <- newval %>%
    #    mutate(displayenroll = enroll)
    #}
    #if (! input$specialtopicshonors) {
    #  newval <- newval %>%
    #    filter(! number %in% c(290, 390, 490))
    #}
    return(newval)
  })
  
  output$map <- renderLeaflet({
    m <- sales() %>%
      select(Long, Lat, SalePrice) %>%
      leaflet() %>%
      addTiles() %>%  
      addCircles(lng = ~Long, lat = ~Lat, radius = 10,
                 color = ~pal(log(SalePrice)), opacity = 5)
    
    return(m)
  })
  
  #Create data table
  output$transactionstable <- DT::renderDataTable({
    DT::datatable(data = sales(), 
                  options = list(pageLength = 20), 
                  rownames = FALSE)
  })
  
  # Create data table
  output$codebook <- DT::renderDataTable({
    DT::datatable(data = codebook, 
                  options = list(pageLength = 8), 
                  rownames = FALSE)
  })
}

shinyApp(ui = ui, server = server)