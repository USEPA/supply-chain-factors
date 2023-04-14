# Supply Chain GHG Emission Factors
Code to produce the most recent version of the [Supply Chain GHG
Emission Factors](https://cfpub.epa.gov/si/si_public_record_Report.cfm?dirEntryId=349324).
Code to produce older versions can be found in [releases](https://github.com/USEPA/supply-chain-factors/releases),
where the release version number corresponds with Supply Chain GHG
Emission Factors dataset version.

Watch this [EPA webinar](https://www.youtube.com/watch?v=pJ8gvZPdcgc) to
understand how to produce the Factors and how to use them for
organizational GHG reporting.

Check out
[Discussions](https://github.com/USEPA/supply-chain-factors/discussions)
for FAQs. If you don’t see your questions in it, feel free to post them.
If you want answer a posted question or add comments to a question, you
are welcome to submit them.

## Software and Hardware Requirements

1.  [R](https://www.r-project.org/) software, version \>= 3.6
2.  A Windows, Mac, or Linux computer with an Internet connection and
    permissions set to install R packages and write files to a local
    project folder.
3.  A local version of this repository, preferably a
    [release](https://github.com/USEPA/supply-chain-factors/releases)
    version to create EPA-verified factors.

### Optional Requirements

1.  [Git](https://github.com/git-guides/install-git), if users wish to
    clone the code from this repository using Git, instead of
    downloading a zip file of the code.
2.  [RStudio](https://www.rstudio.com/products/rstudio/download/#download),
    which provides a graphical interface and button to run this code
    (see Usage).

## Usage

The code in this repository can be used to generate the USEEIO-based
Supply Chain GHG Emission Factors, based on options selected by the user
at run-time.

If users choose to download a zip file of the code, they can unzip the
file then [create a project from the existing directory within
RStudio](https://support.rstudio.com/hc/en-us/articles/200526207-Using-RStudio-Projects)
by selecting the unzipped local directory of files.

If users have Git installed, they can [create a project by cloning the
repository with
RStudio](https://resources.github.com/whitepapers/github-and-rstudio/#:~:text=Clone%20the%20repository%20with%20RStudio).

Once a project is created, double click to open it, then open the
*CalculateEmissionFactors.Rmd* in the editor, click on *Knit* and select
*Knit with Parameters…*.

Alternatively, a user can run this code in an R command line

``` r
install.packages("rmarkdown")
rmarkdown::render("CalculateEmissionFactors.Rmd", params = "ask")
```

In both cases a comma-separated file of the supply chain factors will be
written out in the main folder of the project, with a name like
*SupplyChainGHGEmissionFactors\_\[…\].csv* based on the options selected
by the user and matches one of the [model specifications](model-specs/)
stored in this project. The fields used to defined data are described in
the [format specifications](format-specs/). A markdown file,
CalculateEmissionFactors.md, is automatically generated during
execution, but it contains no original content.

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
