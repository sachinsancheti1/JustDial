# JustDial

To run the code, open R and run the following code.

####First time installation
Install R from [cran](cran.r-project.org)
After installation, run *R* from the desktop

####Packages required
Install the packages *shiny* and *Rcpp* by pasting the following code in the console
```r
install.packages("shiny")
install.packages("Rcpp")
install.packages("shiny")
install.packages("shinythemes")
install.packages("RJSONIO")
install.packages("XML")
install.packages("tidyr")
install.packages("dplyr")
install.packages("RCurl")
install.packages("bitops")
install.packages("stringr")
```

####Run the module in a web browser
```r
require(shiny)
shiny::runGitHub("JustDial","sachinsancheti1")
```
