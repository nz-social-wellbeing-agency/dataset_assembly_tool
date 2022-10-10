# Dataset Assembly Tool
General purpose tool for assembling data from a range of SQL server tables into a single rectangular format.

## Overview
Analytic and research projects often begin with a period of data wrangling. As the number of data sources increases, so does the complexity of preparation. In response to this challenge, we have developed the Dataset Assembly Tool. The tool provides a consistent and fast method for assembling datasets for our analytic projects. This tool is now available for other researchers and analysts to benefit from.

## Reference material
This repository provides the code for the Dataset Assembly Tool. It should be used alongside the overview document: **Accelerating dataset assembly: Primer and guide to the Dataset Assembly Tool**. This contains discussion of the design principles, architecture and workings of the tool. As well as a worked example of its processing.

Users should also review the training presentation and manual: **The Dataset Assembly Tool - Intro and training**. In addition to the video presentation, the extensive speakers notes in this document serve as a users' manual for the tool. This guide covers in installation of the assembly tool and the steps to creating a new assembly.

Included with the assembly tool are our tools for summarising, confidentilaising, and checking summary output. These tools can be used with the assembly tool, or separately. Training material for these tools is available on the Agency's guidance page ([here](https://swa.govt.nz/publications/guidance/)).

List of reference material:
* [Assembly tool primer and guide](https://swa.govt.nz/assets/Publications/guidance/Introduction-to-the-Dataset-Assembly-tool-primer-and-guide.pdf)
* [Assembly tool intro and training](https://swa.govt.nz/assets/Publications/guidance/Dataset-Assembly-Tool-introduction-and-training-presentation.pdf)
* [Assembly tool training presentation](https://vimeo.com/490565559)
* [Assembly tool demonstration](https://vimeo.com/561152732/435a570079)
* [Summarise and confidentialise training](https://swa.govt.nz/assets/Publications/guidance/summarise-and-confidentialise-tools-training-guide-v2.pdf)
* [Self-checking tools training](https://swa.govt.nz/assets/Publications/guidance/self-checking-tools-training-guide.pdf)
* [IDI exemplar project](https://swa.govt.nz/assets/Publications/guidance/IDI-Exemplar-project-guide.pdf)

## Installation
To install the tool, download this repository, copy it to your working location and unzip it. There are then two steps to get the tool running:

1. Find the name of the SQL server, default database and port number. Enter these details in dbplyr_helper_functions.R. These details are used by the tool to connect from R to SQL.

2. Run the automated testing scripts. Open the automated_tests.R file, update the file with the path to the tool and the schema name to test in, and then run the tests. This will confirm whether the tool is running correctly.

The Dataset Assembly Tool has been built to work in the Integrated Data Infrastructure (IDI) by Stats NZ. This environment uses SQL server and RStudio. If you are wanting to use the tool in a different environment, you may have to adapt parts of the code due to differences in syntax between SQL server and other versions of SQL.

The assembly tool has some dependency on specific R packages. A list of packages (and their version) when the tool was last updated is included in the documentation folder. As at August 2020, all of these required R packages are installed in the IDI by default so users do not have to do anything.

## Adoption and adaptation
We encourage researchers to adopt the assembly tool into their own research projects. To enhance this adoption, the overview document notes a range of patterns that shape our wider workflow that we have found to enhance our research. We hope other researchers and analysts also find these valuable, but we accept that others may wish to continue using their existing practices, simply adopting the tool to speed things up.

We discourage researchers in the IDI from editing the code of the tool. Part of the value of a tool is the consistency it provides between researchers. When researchers begin to edit the tool, then we no longer have a shared tool but instead have a collection of bespoke code that with a common origin. The overview document provides some guidance for other ways you can build upon the Dataset Assembly Tool without editing its code.

## Citation

Social Wellbeing Agency (2020). Dataset assembly tool. Source code. https://github.com/nz-social-wellbeing-agency/dataset_assembly_tool

## Getting Help
Enquiries can be sent to info@swa.govt.nz
