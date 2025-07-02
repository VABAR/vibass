## Resubmission
This is a resubmission. In this version I have:

* Reset graphical parameters (`par()`) after modification in vignettes.

* Removed unnecessary check for package availability in a vignette, via
`installed.packages()`.

* Removed unnecessary conditional installation of a package in a vignette.

* Confirmed that the package is meant for teaching in the long term. NOT only
for this year's edition of VIBASS. We have been using the package for several
years, and we want to continue to do so while improving it and making it 
more widely available for others to use and improve upon.

* Fixed some missing LaTeX delimiters in vignette p7.

* Add ORCID for one of the package contributors in DESCRIPTION.

* Improved code style in vignette p6.




## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

* Suggests or Enhances not in mainstream repositories:
    INLA
  Availability using Additional_repositories specification:
    INLA   yes   https://inla.r-inla-download.org/R/stable    

  The non-CRAN Suggested package INLA has been extensively tested
  locally and in github actions for both Linux, Windows, and macOS.
  The needed repository specification is included in the package DESCRIPTION.
