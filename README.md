# Tutorial for Spatial Autocorrelation Analysis in R 
This tutorial uses Global Moran’s I and Local Interpretation of Spatial Autocorrelation (LISA) testing methods to determine the spatial correlation between median total income and percent French knowledge speakers in Kelowna, British Columbia. 

## Introduction
Spatial autocorrelation is a spatial analysis technique that allows us to determine whether an observation is positively correlated (nearby observations have similar values) or negatively correlated (near things have dissimilar values). This gives us an idea of the spatial distribution of the dataset indicating whether observations are distributed randomly, clustered, or dispersed. Global Moran's I is measures spatial association based on location and value simultaneously, giving us a generalized inferential statistic. Local Moran's I or LISA, compares all values and locations within the context of all neighbouring values, providing a comprehensive statistic describing 'hotspots' and significant outliers. In each of these tests we will examine how similar or dissimilar observations at a location (i) are from observations at neighbouring locations (j).  Spatial Autocorrelation is preferred over other measures of spatial association as it accounts for Tobler's Law, which states that observations are not independent and will influenced by observations at nearby locations (Miller H.J., 2004).

### Data 
The data for this analysis was obtained from the Statistics Canada Open Government Portal (2016), consisting of a shapefile containing the census boundaries of multiple Canadian cities in a multi-polygon format, and a CSV file with census data. Census data is widely used in spatial analysis as it provides us with a large amount of data about population demographics, socioeconomics, and other key population factors for the regions where it is collected. In this tutorial we compare medain total income with percent French knowledge speakers to explore the relationships between French culture and socioeconomic status in our study area. These relationships are important from a census context as they influence access to resources, public policy, policing, and locations of high density immigrant populations (Statistics Canada, 2014).

### Packages & Libraries

One of the best things about using R is that we can access a variety of open-source code packages to help us execute 
a multitude of tasks. For example, the "tmap" package library allows us to access functions to create maps that visualize our spatial data (Tennekes M., 2020), "spdep" allows us to calculate spatial staistics (Bivand R., 2020), and "knitr" allows us to embed R code within a pdf, html, or word document output (Yihuei X., 2024). With a better sense for how packages expand our capabilities in R, we can now begin by installing the necessary packages and calling them from our package library using the "library()" function. Whenever we install a new package in R it will be saved to our package library, so we must simply use the "library()" function to access them at anytime. Provided below are the necessary packages for spatial autocorrelation analysis:

``` {r Libraries, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE}
install.packages("st")
install.packages("knitr"
install.packages("tmap")
install.packages("spdep")
install.packages("raster")
install.packages("e1071")
install.packages("shinyjs")

#Load in libraries:
library("st")
library("knitr")
library("tmap")
library("spdep")
library("raster")
library("e1071")
library("shinyjs")
```

## Analysis
### Step 1: Creating a working directory

Before starting our spatial analysis, we need to create a working directory to pull our data from. The working directory is the folder containing the census and shapefiles we downloaded onto our desktop. The assignment operator "<-" is used to assign the file path to our data to the object "dir", which tells R where to access and read the data. To set the working directory we use the function "setwd(dir)". At any point, we can verify the current working directory using the function "get(wd)".

```{r working directory, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE}
dir <- "C:/Users/Eve/OneDrive/Assignment 3"
setwd(dir)
get(wd)
```


### Step 2: Read-in Datasets

This first step in preparing our spatial and census data for analysis is by ‘reading in’ the csv and shapefile necessary for our spatial autocorrelation analysis. These files represent the census data (csv) and study area (shp) used in the analyses. The process of bringing these datasets in R allows us to create two data frames to work within. We can check to see if we’ve successfully loaded these datasets into R by looking at the Global Environment Pane on the top right corner of the R window.  

```{r Read in data, echo=TRUE, eval=TRUE, warning=FALSE}
#From the working dir read in the census data (csv.)
csv <- read.csv("C:/Users/Eve/OneDrive/Assignment 3/ucgsJQnBVLvP_data.csv") 
#Read in shapefile
shp <- st_read("C:/Users/Eve/OneDrive/Assignment 3/lda_000b16a_e.shp") 
```
### Step 3: Data cleanup

Next, we’ll want to clean up the census data to make it easier to work with. A simple way to do this is by applying new column names to the census data frame  so that we know what attributes they refer to. "GEO UID" stands for geographic unique identifier, which is a code assigned to the geographic areas in our census data set. The shapefile equivalent of GEO UID is the "DA UID" (dissemination area unique identifier), which in our dataset consists of eight characters. To maintain consistency between the two geographic identifiers we must remove any ID’s with less than eight characters from our census data frame. This process, and the process for renaming the csv columns is shown in the code below: 

```{r change columns, echo=TRUE, eval=TRUE, warning=FALSE}
#New column names
cols <- c("GEO UID", "Province code", "Province name", "CD code",
        "CD name", "DA name", "Population", "Land area", 
        "Median total income", "Income Sample Size", "French Knowledge", 
        "Language Sample Size")

#Apply those names to dataframe
colnames(csv) <- cols

#Add column to count number of GE0 UID charactors
csv$len <- nchar(csv$`GEO UID`)

#Remove GEO UIDs with less than 8 numbers
csv_clean <- subset(csv, csv$len == 8)

```
### Step 4: Merge spatial and aspatial data

The relationship between "GEOUID" and "DAUID" is shown in the code below, where we will use the "merge" function to combine our aspatial (census data) and spatial (shape file) together into one data frame called "census_DAs".

```{r merge data, echo=TRUE, eval=TRUE, warning=FALSE}
#Merge spatial and aspatial data
census_DAs <- merge(shp, csv_clean, 
                    by.x = "DAUID", 
                    by.y = "GEO UID", 
                    all.x = TRUE)
```
### Step 5: Subset variables to the study area and remove NA values

Next we will want to subset our data to include only the census data applicable to the study area, which in this case is Kelowna. This process produces a new data frame containing the subset data called "Municp". Then we will create a new variable called "PercFrench" in our "Municp" data frame based on the rate between french knowledge and language sample size, as shown in the code below. To maintain accuracy in our results, any missing data in the form of 0 or NA values must be removed. This will make sure that the polygons we are interested in will contain values for our variables of interest.  

```{r subset data, echo=TRUE, eval=TRUE, warning=FALSE}
#Subset for Vancouver
Municp <- subset(census_DAs, census_DAs$CMANAME == "Kelowna")

#Convert to rate
Municp$PercFrench <- (Municp$`French Knowledge`/Municp$`Language Sample Size`)*100
```
```{r NA Remove, echo=TRUE, eval=TRUE, warning=FALSE}
#Remove Income NA
Income_noNA <- Municp[which(!is.na(Municp$`Median total income`)),]

#Remove French NA
French_noNA <- Municp[which(!is.na(Municp$PercFrench)),]
```

### Step 6: Calculate descriptive statistics

Next, we will take a closer look at the two variables we are interested in: median total income and Percentage of respondents with French language knowledge. We will look at some descriptive stats and do a final check for NA values in the data.

```{r DescriptiveStats, echo=TRUE, eval=TRUE, warning=FALSE}
#Calculate descriptive stats for Income
meanIncome <- mean(Income_noNA$`Median total income`)
stdevIncome <- sd(Income_noNA$`Median total income`)
skewIncome <- skewness(Income_noNA$`Median total income`)

#Calculate descriptive stats for French
meanFrench <- mean(French_noNA$PercFrench)
stdevFrench <- sd(French_noNA$PercFrench)
skewFrench <- skewness(French_noNA$PercFrench)

#Create dataframe for display in table
data1 <- data.frame(Variable = c("Income", "French Language"),
                   Mean = c(round(meanIncome,2), round(meanFrench,2)),
                   Std.Dev = c(round(stdevIncome,2), round(stdevFrench,2)),
                   Skewness = c(round(skewIncome,2), round(skewFrench,2)))
#produce table 
kable(data1, caption = paste0("Descriptive statistics for selected ", 2016, " census variables"))
```
<img width="1000" alt="table1" src="https://github.com/user-attachments/assets/a54a5582-09a9-473b-a4ac-72ec23714446">


### Step 7: Map variables of interest

The first step in mapping our variables of interest is by creating two ‘map objects’ called map_Income, and map_French. We use the "tmap" package to access our mapping functions. For example, "tm_shape" to tell R what variable we are mapping and the function "tm_polygons" to customize the parameters for our map design elements. Next, we will print the maps side by side for comparison, using the function "tmap_arrange". The code and output maps for median total income and percentage of population with French knowledge are shown below: 

```{r StudyArea, echo=TRUE, eval=TRUE, warning=FALSE}

#Map median Income
map_Income <- tm_shape(Income_noNA) + 
  tm_polygons(col = "Median total income", 
              title = "Median total income", 
              style = "jenks", 
              palette = "PuBu", n = 6,
              border.alpha = 0,
              colorNA = "grey") +
  tm_layout(legend.position = c("RIGHT", "TOP"))

#Map French Knowledge
map_French <- tm_shape(French_noNA) + 
  tm_polygons(col = "PercFrench", 
              title = "Percentage with \n French Knowledge", 
              style = "jenks", 
              palette = "RdPu", n = 6,
              border.alpha = 0,
              colorNA = "grey") +
  tm_layout(legend.position = c("RIGHT", "TOP"))

#Print maps side by side
tmap_arrange(map_Income, map_French, ncol = 2, nrow = 1)

```
<img width="1000" alt="Rplot02" src="https://github.com/user-attachments/assets/2048fd60-6fb6-4f5d-9dec-06d819d9f199">
Figure 1. Kelowna census dissemination areas showing median total income (left) and percentage of respondants with knowledge of french (right).

### Step 8: Calculate Queen and Rook weights

In order to calculate Moran's I, we need to define which locations fall within our neighbourhood. We'll use the Queen weighting scheme to define another neighbourhood as the eight neighbouring polygons located vertically, horizontally, and diagonally from our main point (I). Then we will use the Rook weighting scheme to define a neighbourhood as the four neighbouring polygons located vertically and horizontally from our main point (I). By selecting these neighbourhoods, we can compare how either might affect our results. From this comparison, we can decide which is most suitable for our analysis.

```{r Neighbours, echo=TRUE, eval=TRUE, warning=FALSE}
#Income Neighbours - Queens weight
Income.nb <- poly2nb(Income_noNA)
Income.net <- nb2lines(Income.nb, coords=st_coordinates(st_centroid(Income_noNA)))
crs(Income.net) <- crs(Income_noNA)

#Income Neighbours - Rooks weight
Income.nb2 <- poly2nb(Income_noNA, queen = FALSE)
Income.net2 <- nb2lines(Income.nb2, coords=st_coordinates(st_centroid(Income_noNA)))
crs(Income.net2) <- crs(Income_noNA)
```

```{r Neighbours, echo=TRUE, eval=TRUE, warning=FALSE}
#French Neighbours - Queens weight
French.nb <- poly2nb(French_noNA)
French.net <- nb2lines(French.nb, coords=st_coordinates(st_centroid(French_noNA)))
crs(French.net) <- crs(French_noNA)

#French Neighbours - Rooks weight
French.nb2 <- poly2nb(French_noNA, queen = FALSE)
French.net2 <- nb2lines(French.nb2, coords=st_coordinates(st_centroid(French_noNA)))
crs(French.net2) <- crs(French_noNA)
```
<img width="200" alt="weights" src="https://github.com/user-attachments/assets/fcbf6529-bb84-46fc-81cd-4159ec365172">

The list above shows the first few values of the weighted matrices we just created.

### Step 9: Mapping weighted neighbourhoods

Next, we will visualize the weighted neighbourhood links for the Rook and Queen weighting schemes for both median total income and percent French speaking population by superimposing them onto our study area. In the figures below, we can see that the Queens weighting scheme provides more neighbourhood links than the Rook scheme, thereby capturing a broader range of spatial relationships. This is in part due to the fact that the Rook scheme only connects with polygons that share a direct edge border, whereas the Queen scheme considers polygons that share both edge and corner borders.

```{r Neighboursmap, echo=TRUE, eval=TRUE, warning=FALSE}
#Make queens map
IncomeQueen <- tm_shape(Income_noNA) + tm_borders(col='lightgrey') + 
              tm_shape(Income.net) + tm_lines(col='blue')

#Make rooks map
IncomeRook <- tm_shape(Income_noNA) + tm_borders(col='lightgrey') + 
              tm_shape(Income.net2) + tm_lines(col='red', lwd = 2)

#Make combined map
IncomeBoth <- tm_shape(Income_noNA) + tm_borders(col='lightgrey') + 
               tm_shape(Income.net) + tm_lines(col='blue', lwd = 2) +
               tm_shape(Income.net2) + tm_lines(col='red', lwd = 2)

#Print maps in a three pane figure
tmap_arrange(IncomeQueen, IncomeRook, IncomeBoth, ncol = 3, nrow = 1)
```
<img width="1000" alt="Rplot04" src="https://github.com/user-attachments/assets/39771e49-ba9d-428f-a578-229e5975573d">
Figure 2. Kelowna census dissemination areas showing median total income queens weight (left)  rooks weight (middle) and the combination of the two (right)

``` {r Neighboursmap2, echo=TRUE, eval=TRUE, warning=FALSE}
#Make queens map french
FrenchQueen <- tm_shape(French_noNA) + tm_borders(col='lightgrey') + 
  tm_shape(French.net) + tm_lines(col='blue')

#Make rook map
FrenchRook <- tm_shape(French_noNA) + tm_borders(col='lightgrey') + 
  tm_shape(French.net2) + tm_lines(col='blue')

#Make combined French map
FrenchBoth <- tm_shape(French_noNA) + tm_borders(col='lightgrey') + 
  tm_shape(French.net) + tm_lines(col='blue', lwd = 2) +
  tm_shape(French.net2) + tm_lines(col='red', lwd = 2)

#Print maps in a three pane figure
tmap_arrange(FrenchQueen, FrenchRook, FrenchBoth, ncol = 3, nrow = 1)
```
<img width="1000" alt="Rplot03" src="https://github.com/user-attachments/assets/698148d9-2cb0-4378-b8d7-1be7cfd032c4">
Figure 3. Kelowna census dissemination areas showing percent French knowledge speakers queens weight (left)  rooks weight (middle) and the combination of the two (right)

### Step 10: Create a weighted matrices

By creating a weighted neighbourhood matrix we are finding how much nearby observations deviate from the mean. There is a variety of weighted matrix possibilities including, inverse distance, nearest neighbour, and contiguity (ESRI, 2024). For this analysis we are using a basic binary contiguity weighting scheme, where each neighbour is given a weight of 1, and all other polygons are given a weight of 0.

To create a weights matrix in R we'll use the “nb2listw" function from the “spdep” package library. We can apply this function to the Income.nb, and French.nb variables we created above, as they contain the Queen scheme neighbourhood links to which we will assign weights. To avoid any issues in running the code caused by the existence of polygons in our weights matrix file with zero neighbour links, we define “zero.policy” as equal to “TRUE”. This will assign a weight vector of zero for regions with no neighbours. To assess the distribution of weights for each observation (i) and its neighbours (j), we can print of off our list of weights matrices using the function “print(subset_weights)”. The code below uses the binary contiguity weighted scheme type "B" we used to define our weighted neighbourhoods in the previous section.

```{r Final weights, echo=TRUE, eval=TRUE, warning=FALSE}.
#Create Income weights matrix
Income.lw <- nb2listw(Income.nb, zero.policy = TRUE, style = "B")

#Create French weights matrix
French.lw <- nb2listw(French.nb, zero.policy = TRUE, style = "B")

#Print of list of Income weights
subset_weights<-head(Income.lw[["weights"]])[c(1:3)]
print(subset_weights)

#Print of list of French weights
subset_weights<-head(French.lw[["weights]])[c.(1:3)]
print(subset_weights)

```
# Spatial Autocorrelation Statistics

## Global Moran’s I
Now that we have chosen and created weight matrices for our neighbours, we can calculate the Global Moran’s I statistic. This method of spatial autocorrelation provides us with an idea of how correlated our data is over the entire data set, representing our spatial pattern at a global scale (ESRI, 2024). Where all observations (i) are compared with all of their neighbouring locations (j) to produce a global measure of variance, I, expected I, and z-score. These values and how they are calculated, are explained in the following section.

The equation for this statistic is:

$$
I = \frac{\sum_{i=1}^n\sum_{j=1}^nW_{i,j}(x_i - \bar{x})(x_j - \bar{x})}{(\sum_{i=1}^n\sum_{j=1}^nW_{i,j})\sum_{i=1}^n(x_i - \bar{x})^2}
$$

Here, if $x$ is the variable being assessed, $x_i$ is the variable value at a point of interest (i) and $x_j$ represents a neighbour to $x_i$ (here determined by the queen weighting scheme). The spatial weighting applied to the weighting matrix $W_{i,j}$ is multiplied by both the differences of $x_i$ and the mean value of variable $x$, and $x_j$ and the mean value of variable $x$. The denominator is used to standardize our values, which tells us that high values of I (>1) correspond with positive spatial autocorrelation, and low values of I (<1) correspond with negative spatial autocorrelation. We'll use the function "moran.test()" from the "spedep" package library to avoid typing out the entire calculation. 

```{r Global Morans I, echo=TRUE, eval=TRUE, warning=FALSE}
#Calculate Global Moran's I for Income
miIncome <- moran.test(Income_noNA$`Median total income`, Income.lw, zero.policy = TRUE)

#Extract Global Moran's I results for Income
mIIncome <- miIncome$estimate[[1]]
eIIncome <- miIncome$estimate[[2]]
varIncome <- miIncome$estimate[[3]]

#Calculate Global Moran's I for French
miFrench <- moran.test(French_noNA$PercFrench, French.lw, zero.policy = TRUE)

#Extract Global Moran's I results for French
mIFrench <- miFrench$estimate[[1]]
eIFrench <- miFrench$estimate[[2]]
varFrench <- miFrench$estimate[[3]]

#Create data frame for display in table
data2 <- data.frame(Variable = c("Income", "French Language"),
                   I = c(round(mIIncome,2), round(mIFrench,2)),
                   Expected = c(round(eIIncome,3), round(eIFrench,3)),
                   Variance = c(round(varIncome,3), round(varFrench,3)))
#Produce table 
kable(data2, caption = paste0("Moran's I statistics for selected ", 2016, " census variables"))
```
<img width="1000" alt="table2" src="https://github.com/user-attachments/assets/eb065ad5-201d-4d38-ae0e-752cea3447a2">

The results for the Moran's I values for median total income and percent French knowledge speakers are 0.58 and 0.27, respectively. Since these values are greater than the expected Moran's I value of -0.004, we can infer that both variables exhibit a moderate to highly clustered distribution. The low variance values indicate that most of our observed values are relatively close to the mean. Next, we will explore the range of Moran's I values for both variables using the following code:

```{r Global Morans Range, echo=TRUE, eval=TRUE, warning=FALSE}
#Function to calculate the range of global Moran's I
moran.range <- function(lw) {
  wmat <- listw2mat(lw)
  return(range(eigen((wmat + t(wmat))/2)$values))
}

#Calculate the range for the Income variable
range <- moran.range(Income.lw)
minRange <- range[1]
maxRange <- range[2]

#Calculate the range for the French variable
range <- moran.range(French.lw)
minRange2 <- range[1]
maxRange2 <- range[2]
```
<img width="1000" alt="table3" src="https://github.com/user-attachments/assets/5394b5ac-eaa2-4aa6-ad8b-a5a1c3013028">

The purpose of calculating the range of Moran's I values is to give us an idea of what values we might see for perfectly dispersed and clustered distributions for both median total income an percent French knowledge speakers. These ranges give us an idea of where our calculated Moran's I values fall within the entire range of values across the dataset, allowing us to determine the pattern of spatial distribution and the kind of spatial autocorrelation (positive or negative) that exists between the two variables.

To go one step further, we can determine whether or not these spatial patterns are statistically significant. To do this, we use the Z-test. Here our null hypothesis is that values for median total income and percent French knowledge speakers are randomly distributed, and the alternate hypothesis is that they are not randomly distributed. Using an $\alpha$ value of 0.05 (95% confidence interval), if our Z-score falls above or below +/-1.96, we can reject the null hypothesis. A value greater than +1.96 would imply that our variables are significantly clustered, and a value less than -1.96 would imply that they are significanlty dispersed.

We can calculate a Z-test using the following code:

```{r Global Morans ZScore, echo=TRUE, eval=TRUE, warning=FALSE}
#Calculate z-test for Income
zIncome <- (mIIncome - eIIncome) / (sqrt(varIncome))

#Calculate z-test for French
zFrench <- (mIFrench - eIFrench) / (sqrt(varFrench))

#Create data frame for dispay in table
data4 <- data.frame(Variable = c("Income", "French Language"),
                   Zscore = c(round(zIncome,2), round(zFrench,2)))
#produce table 
kable(data4, caption = paste0("Moran's I Z-score for selected ", 2016, " census variables"))
```
<img width="1000" alt="table4" src="https://github.com/user-attachments/assets/9313b532-e89c-4b5c-9a79-d7809283835e">

The Z-scores for both variables confirm that we can reject the null hypothesis for both variables as the Z-scores are >1.96 with 95% confidence. This indicates that both variables exhibit significant clustering and strong positive spatial autocorrelation.

## Local Moran's I

Local Moran's I or LISA, is different from Global Moran's I in that it provides us with a statistic for each location with an assessment of significance (Anselin, 2020). The main difference between the two is that instead of providing single measures of Moran's I, expected I, variance, and z-score, LISA provides us with these statistics for every single location (i) in the dataset.

The calculation for Local Moran’s I has many of the same features as the global calculation, although arranged differently.
$$
I_i = \frac{x_i - \bar{x}}{S_i^2}\sum{_{j=1}^n}W_{i,j}(x_j - \bar{x})\space \space where \space \space S_i^2 = \frac{\sum_{i=1}^n (x_i - \bar{x})^2}{n-1} 
$$

Again, instead of typing out these calculations, we'll use the "localmoran()" function from the "spdep" package library and input our variables and weighting scheme. This process is shown in the code below:

```{r Local Morans I, echo=TRUE, eval=TRUE, warning=FALSE}
#Calculate LISA test for Income
lisa.testIncome <- localmoran(Income_noNA$`Median total income`, Income.lw)

#Extract LISA test results for Income
Income_noNA$Ii <- lisa.testIncome[,1]
Income_noNA$E.Ii<- lisa.testIncome[,2]
Income_noNA$Var.Ii<- lisa.testIncome[,3]
Income_noNA$Z.Ii<- lisa.testIncome[,4]
Income_noNA$P<- lisa.testIncome[,5]

#Calculate LISA test for Income
lisa.testFrench <- localmoran(French_noNA$PercFrench, French.lw)

#Extract LISA test results for Income
French_noNA$Ii <- lisa.testFrench [,1]
French_noNA$E.Ii<- lisa.testFrench [,2]
French_noNA$Var.Ii<- lisa.testFrench [,3]
French_noNA$Z.Ii<- lisa.testFrench [,4]
French_noNA$P<- lisa.testFrench [,5]
```
This time, instead of printing out a table like we did for the Global Moran's I results, we'll create a map so we can visualize and understand what this test has done.

```{r MappingLocalMoransI, echo=TRUE, eval=TRUE, warning=FALSE}
#Map LISA z-scores for Income
map_LISA_Income <- tm_shape(Income_noNA) +
  tm_polygons(col = "Z.Ii",
              title = "Local Moran's I Z-Scores",
              style = "fixed",
              border.alpha = 0.1,
              midpoint = NA,
              colorNA = NULL,
              breaks = c(min(Income_noNA$Z.Ii),-1.96,1.96,max(Income_noNA$Z.Ii)),
              palette = "-RdBu", n = 3)+
  tm_compass(position=c("left", "top"))+
  tm_scale_bar(position=c("left", "bottom"))+
  tm_legend(position = c("right", "top"))

#Map LISA z-scores for French
map_LISA_French <- tm_shape(French_noNA) +
  tm_polygons(col = "Z.Ii",
              title = "Local Moran's I Z-Scores",
              style = "fixed",
              border.alpha = 0.1,
              midpoint = NA,
              colorNA = NULL,
              breaks = c(min(French_noNA$Z.Ii),-1.96,1.96,max(French_noNA$Z.Ii)),
              palette = "-RdBu", n = 3)+
  tm_compass(position=c("left", "top"))+
  tm_scale_bar(position=c("left", "bottom"))+
  tm_legend(position = c("right", "top"))

#Plot maps in a 2 pane figure
tmap_arrange(map_LISA_Income, map_LISA_French, ncol = 2, nrow = 1)
```
![Rplot05](https://github.com/user-attachments/assets/1f0840c1-7bca-4874-80da-3704d3cf244c)

Figure 4. Kelowna census dissemination areas showing LISA z-scores for median total income (left) and percentage of respondents with French knowledge (right).

The above map shows that there are census tracts in Kelowna that exhibit significant distribution patterns for both median total income and percent French knowledge speakers. The red polygons represent tracts that exhibit significant clustering, and the blue polygons represent area of significant disperion. Although these maps are great for visualizing which polygons in our study area are significantly positively or negatively spatially autocorrelated, it will be even more informative if we graph the Local Moran's I Z-values. This process is shown in the code below where we'll use the function "moran.plot()" from the "spdep" package library to create scatterplot.

```{r MoransIScatter, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap= "Moran's I scatter plot for median total income."}
#Create Moran's I scatter plot for Income
moran.plot(Income_noNA$`Median total income`, Income.lw, zero.policy=TRUE, spChk=NULL, labels=NULL, xlab="Median Total Income ($)", 
           ylab="Spatially Lagged Median Total Income ($)", quiet=NULL)
```
```{r MoransIScatter2, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap= "Moran's I scatter plot for percentage of respondants with knowledge of french."}
#Create Moran's I scatter plot for French
moran.plot(French_noNA$PercFrench, French.lw, zero.policy=TRUE, spChk=NULL, labels=NULL, xlab="Respondants with knowledge of French (%)", 
           ylab="Spatially Lagged knowledge of French (%)", quiet=NULL)
```
In these plots, the points with diamonds are considered statistically significant, and the regression line shows the overall trend. For both plots we can see that the trend shows strong positive spatial autocorrelation, which tells us that there exists spatial clustering of these census variables in Kelowna, B.C.


## Summary

This tutorial provides a basic introduction to spatial autocorrelation in R and provides us with the tools to assess spatial correlation and significance. Although the results of the Global and Local Moran's I tests provide valuable insights into the spatial distributions of our variables of interest, there are still opportunities to refine our results. One key observation is that census tracts increase in size from the centre of the region, which could introduce boundary effects (Griffith, D.A & Amrhein, C.G, 1983). Boundary effects come into play when census tracts have fewer neighbouring data points, as is the case in our study area. This can distort our analysis by underestimating these areas in our results and skewing significance. To address this, we could switch from a binary contiguity weighting scheme "B" to an inverse distance weighting (IDW) scheme to account for census tracts with fewer observations. Additionally, considering population density when preforming Global and Local Moran's I tests could provide further insights into the observed spatial patterns of the variables assessed in this analysis, and in effect refine our results. In short, suggested improvements include, experimenting with alternative weighting schemes to account for boundary edge effects and scaling the results to a measure of population density.

## References

Bivand, R. (2020). spdep: spatial dependence: weighting schemes, statistics. https://CRAN.R-project.org/
package=spdep, R package version 1.1-5

ESRI. (2024). How spatial autocorrelation (Global Moran's I) works. https://pro.arcgis.com/en/pro-app/latest/tool-reference/spatial-statistics/h-how-spatial-autocorrelation-moran-s-i-spatial-st.htm

ESRI. (2024). Spatial weights. https://pro.arcgis.com/en/pro-app/latest/tool-reference/spatial-statistics/spatial-weights.htm

Government of Canada. (2017). Statistical portrait of the French-speaking immigrant population outside Quebec (1911-2011). Canada.ca. https://www.canada.ca/en/immigration-refugees-citizenship/corporate/reports-statistics/research/statistical-portrait-french-speaking-immigrant-population-outside-quebec-1991-2011.html 

Griffith, D.A. and Amrhein, C.G. (1983). An evaluation of correction techniques for boundary effects in spatial statistical analysis: traditional methods. Geographical Analysis, 15, 352-360. https://doi.org/10.1111/j.1538-4632.1983.tb00794.x

Miller, H. J. (2004). Tobler’s First Law and Spatial Analysis. Annals of the Association of American Geographers, 94(2), 284–289. http://www.jstor.org/stable/3693985 

Statistics Canada. (2016). Open Governement Portal. https://open.canada.ca/data/organization/statcan

Tennekes, M. (2020). tmap: thematic maps. https://CRAN.R-project.org/package=tmap, R package version
3.1

Yihuei, X. (2024). knitr: elegant, flexible, and fast dynamic report generation in R. https://yihui.org/knitr/
