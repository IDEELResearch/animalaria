# animalaria

The code in this project accompanies an analysis to understand the relationship between domesticated animal ownership and _P. falciparum_ malaria infection in the Democratic Republic of Congo (DRC), which has the world's second highest malaria burden and where animal husbandry is a key part of local economies and the population's livelihood. Survey and biospecimen data used in this project originate from the nationally representative 2013-14 Demographic and Health Survey (DHS) conducted in DRC, which was the most recently conducted DHS as of October 2022. Survey data are publicly available by request from the DHS Program (https://dhsprogram.com/methodology/survey/survey-display-421.cfm), and Plasmodium biospecimen data are being made publicly available as well. 

This study has been published and is available at: Morgan CE, Topazian HM, Brandt K, Mitchell C, Kashamuka MM, Muwonga J, Sompwe E, Juliano JJ, Bobanga T, Tshefu A, Emch M, Parr JB. Association between domesticated animal ownership and Plasmodium falciparum parasite prevalence in the Democratic Republic of the Congo: a national cross-sectional study. Lancet Microbe. 2023 Jul;4(7):e516-e523. doi: 10.1016/S2666-5247(23)00109-X. Epub 2023 May 31. PMID: 37269868; PMCID: PMC10319634.

# https://rstudio.github.io/renv/index.html

# This project uses renv to manage package versions and dependencies:
```
The packages (and all the metadata needed to reinstall them) are recorded into a lockfile, renv.lock, and a .Rprofile ensures that the library is used every time you open that project.

As you continue to work on your project, you will install and upgrade packages, either using install.packages() and update.packages or renv::install() and renv::update(). After you’ve confirmed your code works as expected, use renv::snapshot() to record the packages and their sources in the lockfile.

Later, if you need to share your code with someone else or run your code on new machine, your collaborator (or you) can call renv::restore() to reinstall the specific package versions recorded in the lockfile.
```
