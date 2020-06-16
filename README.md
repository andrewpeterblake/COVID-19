# COVID-19

Directory contains two Shiny apps that draw a variety of COVID-19 data, mostly replicating versions of the excellent 
FT visualisations of John Burn-Murdoch (@jburnmurdoch) and collaborators. You should go and check out their pioneering work.

The two apps look at a) the data collated by Johns Hopkins CSSE, allowing you to plot one or more countries of confirmed 
cases and deaths on a total and daily basis and b) the FT excess death data set analysed by Chris Giles, (@ChrisGiles_) for 
many less coutries but with some regional content. These are available from the respective GitHub repos that are wired 
into the code.

The easiest way to use this code is in Rstudio. Both apps are pretty straightforward, although they use the tidyverse which 
users will need to familiarise themselves with, and each has a bit of code so that they download the data at most every 
four hours.

These apps are provided on an 'as is' basis. They are to help you visualise the data, not to provide publication quality 
graphics. However I have found them useful to understand the data and its inadequacies. I do not guarantee the accuracy 
of the data which is acknowledged to be flakey and can't even guarantee the graphs are always right -- but I have tried to 
make them so. For example I include a basic rule to replicate the shading on the excess deaths graphs but sometimes this 
goes wrong. There is a warning in the app menu.