# Tool Accelerating Dataset Assembly (TADA)
General purpose tool for assembling data from a range of SQL server tables into a single rectangular format.

## Overview
Analytic and research projects often begin with a period of data wrangling. As the number of data sources increases, so does the complexity of preparation. In response to this challenge, we have developed the Tool Accelerating Dataset Assembly. TADA provides a consistent and fast method for assembling datasets for our analytic projects. TADA is now available for other researchers and analysts to benefit from.

## Reference material
This repository provides the code for TADA. It should be used alongside the overview document: **Accelerating dataset assembly: Primer and guide to the Tool Accelerating Datast Assembly**. This contains discussion of the design principles, architecture and workings of TADA. As well as a worked example of its processing.

Users should also review the training presentation and manual: **The Tool Accelerating Dataset Assembly (TADA) - Intro and training**. In addition to the video presentation, the extensive speakers notes in this document serve as a users' manual for the tool. This guide covers in installation of TADA and the steps to creating a new assembly.

## Installation
To install the tool, download this repository, copy it to your working location and unzip it. There are then two steps to get the tool running:

1. Find the name of the SQL server, default database and port number. Enter these details in dbplyr_helper_functions.R. These details are used by the tool to connect from R to SQL.

2. Run the automated testing scripts. Open the automated_tests.R file, update the file with the path to the tool and the schema name to test in, and then run the tests. This will confirm whether the tool is running correctly.

TADA has been built to work in the Integrated Data Infrasturcture (IDI) by Stats NZ. This environment uses SQL server and RStudio. If you are wanting to use TADA in a different environment, you may have to adapt parts of the code due to differences in syntax between SQL server and other versions of SQL.

TADA has some dependency on specific R packages. A list of packages (and their version) when TADA was last updated is included in the documentation folder. As at August 2020, all of these required R packages are installed in the IDI by default so users do not have to do anything.

## Adoption and adaptation
We encourage researchers to adopt TADA into their own research projects. To enhance this adoption, the overview docuemtn notes a range of patterns that shape our wider workflow that we have found to enhance our research. We hope other researchers and analysts also find these valuable, but we accept that others may wish to contiue using their existing practices, simply adopting TADA to speed things up.

We discourage researchers in the IDI from editting the code of the tool. Part of the value of a tool is the consistency it provides between researchers. When researchers begin to edit the tool, then we no longer have a shared tool but instead have a collection of bespoke code that with a common origin. The overview document provides some guidance for other ways you can build upon TADA without editting its code.

## Getting Help
If you have any questions email info@swa.govt.nz
