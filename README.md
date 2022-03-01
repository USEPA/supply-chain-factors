<!-- README.md is generated from README.Rmd. Please edit that file -->

# Supply Chain GHG Emission Factors

Code and reproductions of the [Supply Chain GHG Emission
Factors](https://cfpub.epa.gov/si/si_public_record_Report.cfm?dirEntryId=349324).

## Software and Hardware Requirements

1.  [R](https://www.r-project.org/) software, version &gt;= 3.6
2.  A Window, Mac, or Linux computer with an Internet connection and
    permissions set to install R packages and write files to a local
    project folder.

### Optional Requirements

1.  [git](https://github.com/git-guides/install-git), if users wish to
    pull the code from this repository using git. Otherwise users can
    download a zip file of the code under Clone or Releases.
2.  [Rstudio](https://www.rstudio.com/), which provides a graphical
    interface and button to run this code (see Usage).

## Usage

The code in this package can be used to generate the USEEIO-based supply
chain GHG emission factors, based on options selected by the user at
run-time.

This factors can also be generated by creating a project with the files
in this repository within [Rstudio](https://www.rstudio.com/), opening
the `CalculateEmissionFactors.Rmd` in the editor, clicking on *Knit* and
selecting *Knit with Parameters…*.

Alternatively, a user can run this code in an R command line

``` r
install.packages("rmarkdown")
rmarkdown::render("CalculateEmissionFactors.Rmd", params = "ask")
```

In both cases, if the option to generate a csv is selected, a
comma-separated file of the supply chain factors will be written out in
the main folder of the project, with a name like
*SupplyChainGHGEmissionFactors-{USEEIOmodelname}.csv* where the
USEEIOmodelname is based on the options selected by the user and matches
one of the [model specifications](model-specs/) stored in this project.
The fields used to defined data are define in the [format
specifications](format-specs/), and are identical to the fields used in
the original Supply Chain Factors dataset releases.

# Disclaimer

The United States Environmental Protection Agency (EPA) GitHub project
code is provided on an “as is” basis and the user assumes responsibility
for its use. EPA has relinquished control of the information and no
longer has responsibility to protect the integrity , confidentiality, or
availability of the information. Any reference to specific commercial
products, processes, or services by service mark, trademark,
manufacturer, or otherwise, does not constitute or imply their
endorsement, recommendation or favoring by EPA. The EPA seal and logo
shall not be used in any manner to imply endorsement of any commercial
product or activity by EPA or the United States Government.
