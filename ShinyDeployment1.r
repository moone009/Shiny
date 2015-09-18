#########################################################################################################
# Name             : Shiny Deployment
# Date             : 05-20-2015
# Author           : Christopher M
# Dept             : We Energies: BEI
#########################################################################################################
# ver    user        date(YYYYMMDD)        change  
# 1.0    CMooney      20150520             initial
#########################################################################################################
#http://rstudio.github.io/shiny/tutorial/#dynamic-ui
#server.R contains the R codes to generate the plots present tables of results
#ui.R contains the R codes to design the user interface for facilitating the user interactivity with the plots to add typical widgets such as slider, check box, select box et
#library(shiny)
#library(devtools)
#devtools::install_github("rstudio/shinyapps")
#http://www.unomaha.edu/mahbubulmajumder/data-science/fall-2014/lectures/19-interactive-shiny-ggvis/19-interactive-shiny-ggvis.html#/9
#http://ggvis.rstudio.com/cookbook.html

library(devtools)
library(shinyapps)
shinyapps::setAccountInfo(name='christophertestaccount', token='536271AF8A8758538E61F3936668602D', secret='rBNd7byxFD2B5gbxzOuzXz/a2PAMxxdZrgBeH5qh')

# Directory of Server and UI
ShinyWorkingDirectory = 'F:/Shiny/TestCMD'
# Deploy to shiny
shinyapps::deployApp(ShinyWorkingDirectory)









