## New York City Shooting Incident Project

**Statement of question of interest:**

I am interested in whether the relationship of the location and the shooting incident can be predicted from previous years. Thus, *Crime distribution per BORO*, *Crime distribution per BORO in Yound Adults*, and a prediction model on 2020 incidents from the previous decade is included in this analysis.

**Source and Description:**

The source of the dataset is from the New York City Government on city shooting incidents. This dataset includes the shooting incidents, locations, the perpetrators, and the victims. The incidents were dated between 2016 to 2023.

*   Dataset location: https://catalog.data.gov/dataset/nypd-shooting-incident-data-historic


```R
#import library
library(tidyverse)
library(lubridate)
library(ggplot2)
```

    ── [1mAttaching core tidyverse packages[22m ──────────────────────── tidyverse 2.0.0 ──
    [32m✔[39m [34mdplyr    [39m 1.1.4     [32m✔[39m [34mreadr    [39m 2.1.5
    [32m✔[39m [34mforcats  [39m 1.0.0     [32m✔[39m [34mstringr  [39m 1.5.1
    [32m✔[39m [34mlubridate[39m 1.9.3     [32m✔[39m [34mtibble   [39m 3.2.1
    [32m✔[39m [34mpurrr    [39m 1.0.2     [32m✔[39m [34mtidyr    [39m 1.3.1
    ── [1mConflicts[22m ────────────────────────────────────────── tidyverse_conflicts() ──
    [31m✖[39m [34mdplyr[39m::[32mfilter()[39m masks [34mstats[39m::filter()
    [31m✖[39m [34mdplyr[39m::[32mlag()[39m    masks [34mstats[39m::lag()
    [36mℹ[39m Use the conflicted package ([3m[34m<http://conflicted.r-lib.org/>[39m[23m) to force all conflicts to become errors



```R
#import data
data=read_csv("/content/NYPD_Shooting_Incident_Data__Historic_.csv")
population=read_csv("/content/ny_population.csv")
```

    [1mRows: [22m[34m28562[39m [1mColumns: [22m[34m21[39m
    [36m──[39m [1mColumn specification[22m [36m────────────────────────────────────────────────────────[39m
    [1mDelimiter:[22m ","
    [31mchr[39m  (12): OCCUR_DATE, BORO, LOC_OF_OCCUR_DESC, LOC_CLASSFCTN_DESC, LOCATION...
    [32mdbl[39m   (7): INCIDENT_KEY, PRECINCT, JURISDICTION_CODE, X_COORD_CD, Y_COORD_CD...
    [33mlgl[39m   (1): STATISTICAL_MURDER_FLAG
    [34mtime[39m  (1): OCCUR_TIME
    
    [36mℹ[39m Use `spec()` to retrieve the full column specification for this data.
    [36mℹ[39m Specify the column types or set `show_col_types = FALSE` to quiet this message.
    [1mRows: [22m[34m6[39m [1mColumns: [22m[34m4[39m
    [36m──[39m [1mColumn specification[22m [36m────────────────────────────────────────────────────────[39m
    [1mDelimiter:[22m ","
    [31mchr[39m (1): BORO
    [32mnum[39m (3): 2000, 2010, 2020
    
    [36mℹ[39m Use `spec()` to retrieve the full column specification for this data.
    [36mℹ[39m Specify the column types or set `show_col_types = FALSE` to quiet this message.



```R
# @title
print("Column Name in the dataset: ")
# List all column names
column_names <- colnames(data)
print(column_names)

# Get the shape of the table (number of rows and columns)
shape <- dim(data)
#print(shape)
```

    [1] "Column Name in the dataset: "
     [1] "INCIDENT_KEY"            "Date"                   
     [3] "Time"                    "BORO"                   
     [5] "LOC_OF_OCCUR_DESC"       "PRECINCT"               
     [7] "JURISDICTION_CODE"       "LOC_CLASSFCTN_DESC"     
     [9] "LOCATION_DESC"           "STATISTICAL_MURDER_FLAG"
    [11] "PERP_AGE_GROUP"          "PERP_SEX"               
    [13] "PERP_RACE"               "VIC_AGE_GROUP"          
    [15] "VIC_SEX"                 "VIC_RACE"               
    [17] "X_COORD_CD"              "Y_COORD_CD"             
    [19] "Latitude"                "Longitude"              
    [21] "Lon_Lat"                 "YEAR"                   
    [23] "DECADE"                 



```R
# List all column names from the population dataset
column_names <- colnames(population)
print(column_names)
population
```

    [1] "BORO" "2000" "2010" "2020"



<table class="dataframe">
<caption>A spec_tbl_df: 6 × 4</caption>
<thead>
	<tr><th scope=col>BORO</th><th scope=col>2000</th><th scope=col>2010</th><th scope=col>2020</th></tr>
	<tr><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>NYC Total    </td><td>8008278</td><td>8242624</td><td>8550971</td></tr>
	<tr><td>BROXN        </td><td>1332650</td><td>1385108</td><td>1446788</td></tr>
	<tr><td>BROOKLYN     </td><td>2465326</td><td>2552911</td><td>2648452</td></tr>
	<tr><td>MANHATTAN    </td><td>1537195</td><td>1585873</td><td>1638281</td></tr>
	<tr><td>QUEENS       </td><td>2229379</td><td>2250002</td><td>2330295</td></tr>
	<tr><td>STATEN ISLAND</td><td> 443728</td><td> 468730</td><td> 487155</td></tr>
</tbody>
</table>



## Visualization Section

There are two visualizations included:

1.   Incident per BORO
2.   Incident per BORO by teenager


```R
# Group data by BORO and calculate the total number of incidents
cases_per_BORO_total <- data %>% group_by(BORO) %>%
  summarize(incidents = n())

# Create bar graph
ggplot(cases_per_BORO_total, aes(x=BORO, y=incidents, fill=BORO)) +
  geom_bar(stat="identity") +
  xlab("BOROUGH") + ylab("INCIDENCES") +
  ggtitle("INCIDENCES IN VARIOUS BOROUGHS") +
  theme_minimal()
```


    
![png](NYPD_files/NYPD_6_0.png)
    



```R
# look at incident per age group
data %>%
  group_by(PERP_AGE_GROUP) %>%
  summarise(Total = n()) %>%
  arrange(desc(Total))
```


<table class="dataframe">
<caption>A tibble: 12 × 2</caption>
<thead>
	<tr><th scope=col>PERP_AGE_GROUP</th><th scope=col>Total</th></tr>
	<tr><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;int&gt;</th></tr>
</thead>
<tbody>
	<tr><td>NA     </td><td>9344</td></tr>
	<tr><td>18-24  </td><td>6438</td></tr>
	<tr><td>25-44  </td><td>6041</td></tr>
	<tr><td>UNKNOWN</td><td>3148</td></tr>
	<tr><td><span style=white-space:pre-wrap>&lt;18    </span></td><td>1682</td></tr>
	<tr><td>(null) </td><td>1141</td></tr>
	<tr><td>45-64  </td><td> 699</td></tr>
	<tr><td>65+    </td><td>  65</td></tr>
	<tr><td>1020   </td><td>   1</td></tr>
	<tr><td>1028   </td><td>   1</td></tr>
	<tr><td>224    </td><td>   1</td></tr>
	<tr><td>940    </td><td>   1</td></tr>
</tbody>
</table>




```R
# filter out data with rows that perp are teen age range between 18-24
young_adult_data <- subset(data, PERP_AGE_GROUP == "18-24")
```


```R
# Group data by BORO and calculate the total number of incidents by teenager
cases_per_BORO <- young_adult_data %>% group_by(BORO) %>%
  summarize(incidents = n())

# Create bar graph
ggplot(cases_per_BORO, aes(x=BORO, y=incidents, fill=BORO)) +
  geom_bar(stat="identity") +
  xlab("BOROUGH") + ylab("INCIDENCES") +
  ggtitle("INCIDENCES IN VARIOUS BOROUGHS by Young Adults") +
  theme_minimal()
```


    
![png](NYPD_files/NYPD_9_0.png)
    


## Model Building Section


```R
#change the column data type for date/time
data <- data %>%
  rename(Date = "OCCUR_DATE",
         Time = "OCCUR_TIME")
data$Date <- as.Date(data$Date, format = "%m/%d/%Y")
class(data$Date)
```


'Date'



```R
summary(data$Date)
```


<style>
.dl-inline {width: auto; margin:0; padding: 0}
.dl-inline>dt, .dl-inline>dd {float: none; width: auto; display: inline-block}
.dl-inline>dt::after {content: ":\0020"; padding-right: .5ex}
.dl-inline>dt:not(:first-of-type) {padding-left: .5ex}
</style><dl class=dl-inline><dt>Min.</dt><dd><time datetime="2006-01-01">2006-01-01</time></dd><dt>1st Qu.</dt><dd><time datetime="2009-09-04">2009-09-04</time></dd><dt>Median</dt><dd><time datetime="2013-09-20">2013-09-20</time></dd><dt>Mean</dt><dd><time datetime="2014-06-07">2014-06-07</time></dd><dt>3rd Qu.</dt><dd><time datetime="2019-09-29">2019-09-29</time></dd><dt>Max.</dt><dd><time datetime="2023-12-29">2023-12-29</time></dd></dl>




```R
# Extract the year from the DATE column
data$YEAR <- year(data$Date)
# Create a new column for 10-year intervals
data$DECADE <- cut(data$YEAR, breaks = seq(1900, 2030, by = 10), right = FALSE, labels = seq(1900, 2020, by = 10))
# Summarize the number of incidents per BORO per decade
summary_table <- data %>%
    group_by(DECADE,BORO) %>%
    summarise(Incidents = n())
# Display the summary table
print(summary_table)
```

    [1m[22m`summarise()` has grouped output by 'DECADE'. You can override using the
    `.groups` argument.


    [90m# A tibble: 15 × 3[39m
    [90m# Groups:   DECADE [3][39m
       DECADE BORO          Incidents
       [3m[90m<fct>[39m[23m  [3m[90m<chr>[39m[23m             [3m[90m<int>[39m[23m
    [90m 1[39m 2000   BRONX              [4m2[24m150
    [90m 2[39m 2000   BROOKLYN           [4m3[24m238
    [90m 3[39m 2000   MANHATTAN           976
    [90m 4[39m 2000   QUEENS             [4m1[24m138
    [90m 5[39m 2000   STATEN ISLAND       227
    [90m 6[39m 2010   BRONX              [4m4[24m047
    [90m 7[39m 2010   BROOKLYN           [4m5[24m677
    [90m 8[39m 2010   MANHATTAN          [4m1[24m674
    [90m 9[39m 2010   QUEENS             [4m2[24m091
    [90m10[39m 2010   STATEN ISLAND       419
    [90m11[39m 2020   BRONX              [4m2[24m179
    [90m12[39m 2020   BROOKLYN           [4m2[24m431
    [90m13[39m 2020   MANHATTAN          [4m1[24m112
    [90m14[39m 2020   QUEENS             [4m1[24m042
    [90m15[39m 2020   STATEN ISLAND       161



```R
# Create a pivot table to match with population table
incident_decade <- summary_table %>%
  group_by(BORO, DECADE) %>%
  summarise(Total_Incidents = sum(Incidents)) %>%
  pivot_wider(names_from = DECADE, values_from = Total_Incidents, values_fill = list(Total_Incidents = 0))

# Display the pivot table
print(incident_decade)

```

    [1m[22m`summarise()` has grouped output by 'BORO'. You can override using the
    `.groups` argument.


    [90m# A tibble: 5 × 4[39m
    [90m# Groups:   BORO [5][39m
      BORO          `2000` `2010` `2020`
      [3m[90m<chr>[39m[23m          [3m[90m<int>[39m[23m  [3m[90m<int>[39m[23m  [3m[90m<int>[39m[23m
    [90m1[39m BRONX           [4m2[24m150   [4m4[24m047   [4m2[24m179
    [90m2[39m BROOKLYN        [4m3[24m238   [4m5[24m677   [4m2[24m431
    [90m3[39m MANHATTAN        976   [4m1[24m674   [4m1[24m112
    [90m4[39m QUEENS          [4m1[24m138   [4m2[24m091   [4m1[24m042
    [90m5[39m STATEN ISLAND    227    419    161



```R
# Join two table together for further analysis
# Perform a left join on BORO column
merged_data <- left_join(incident_decade, population, by = "BORO", suffix = c("_incidents", "_population"))
# Calculate the incident rate per borough per decade
merged_data <- merged_data %>%
  mutate(incident_rate_per_BORO_per_decade = `2000_incidents` / `2000_population`)
# Display the resulting data frame
  print(merged_data)
```

    [90m# A tibble: 5 × 8[39m
    [90m# Groups:   BORO [5][39m
      BORO      `2000_incidents` `2010_incidents` `2020_incidents` `2000_population`
      [3m[90m<chr>[39m[23m                [3m[90m<int>[39m[23m            [3m[90m<int>[39m[23m            [3m[90m<int>[39m[23m             [3m[90m<dbl>[39m[23m
    [90m1[39m BRONX                 [4m2[24m150             [4m4[24m047             [4m2[24m179                [31mNA[39m
    [90m2[39m BROOKLYN              [4m3[24m238             [4m5[24m677             [4m2[24m431           2[4m4[24m[4m6[24m[4m5[24m326
    [90m3[39m MANHATTAN              976             [4m1[24m674             [4m1[24m112           1[4m5[24m[4m3[24m[4m7[24m195
    [90m4[39m QUEENS                [4m1[24m138             [4m2[24m091             [4m1[24m042           2[4m2[24m[4m2[24m[4m9[24m379
    [90m5[39m STATEN I…              227              419              161            [4m4[24m[4m4[24m[4m3[24m728
    [90m# ℹ 3 more variables: `2010_population` <dbl>, `2020_population` <dbl>,[39m
    [90m#   incident_rate_per_BORO_per_decade <dbl>[39m



```R
## data table finished -> model building cell

# Prepare the data & Split the data into training and testing sets
train_data <- merged_data

# Fit a linear regression model
model <- lm(`2020_incidents` ~ `2000_incidents` + `2010_incidents`, data = train_data)

# Summarize the model
summary(model)

# Predict on the training set
train_data$predicted_2020 <- predict(model, newdata = train_data)

# Calculate evaluation metrics
mse <- mean((train_data$`2020_incidents` - train_data$predicted_2020)^2)
rmse <- sqrt(mse)
r2 <- cor(train_data$`2020_incidents`, train_data$predicted_2020)^2

cat("Mean Squared Error:", mse, "\n")
cat("Root Mean Squared Error:", rmse, "\n")
cat("R-squared:", r2, "\n")

# Plot the results
ggplot(train_data, aes(x = `2020_incidents`, y = predicted_2020)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = 'red') +
  labs(title = "Predicted vs Actual Shooting Incidents in 2020",
       x = "Actual 2020 Incidents",
       y = "Predicted 2020 Incidents") +
  theme_minimal()

```


    
    Call:
    lm(formula = `2020_incidents` ~ `2000_incidents` + `2010_incidents`, 
        data = train_data)
    
    Residuals:
          1       2       3       4       5 
      79.43  -99.15  287.70  -76.48 -191.49 
    
    Coefficients:
                     Estimate Std. Error t value Pr(>|t|)
    (Intercept)       158.490    216.113   0.733    0.540
    `2000_incidents`   -1.582      1.935  -0.818    0.499
    `2010_incidents`    1.320      1.088   1.214    0.349
    
    Residual standard error: 265.9 on 2 degrees of freedom
    Multiple R-squared:  0.9586,	Adjusted R-squared:  0.9172 
    F-statistic: 23.15 on 2 and 2 DF,  p-value: 0.04141



    Mean Squared Error: 28285.64 
    Root Mean Squared Error: 168.1834 
    R-squared: 0.958585 



    
![png](NYPD_files/NYPD_16_2.png)
    


## Conclusion & Bias

**Conclusion**

The location definitely plays a role in where shooting incidents occur. The Bronx and Brooklyn have higher rates of shooting incidents. An interesting part of the analysis is that the Bronx has a higher number of incidents contributed by the young adult group, which is between 18-24 years old.

In addition, there is a relationship between the location of shootings across decades. Therefore, we can predict the 2020 shooting incident rate from the previous years.


**Bias**

Since there are only incidents recorded between 2016 and 2023 with no additional socioeconomic background information, there are many biases and it is difficult to draw conclusions from this analysis alone.
