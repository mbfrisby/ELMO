# ELMO Version 0.1.3
Welcome to the Endogenous Latent Moderator (ELMO) Variance Calculator.

This app is currently hosted and can be run online at https://mbfrisby.shinyapps.io/elmo/. In the event that ELMO's limited bandwidth is exceeded, the online application will be unavailable. Users will need to run ELMO locally.

To run ELMO locally, open Rstudio or an R console and paste/run the following code: 

```{r eval = FALSE}
library(shiny)
library(MplusAutomation)
library(DT)
runGitHub("ELMO", "mbfrisby", ref = "main")
```

If you do not have the required libraries installed, you may need to first run the following:

```{r eval = FALSE}
install.packages("shiny")
install.packages("MplusAutomation")
install.packages("DT")
```

Please feel free to contact the author with any questions: mbfrisby@umich.edu
