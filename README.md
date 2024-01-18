# Sharpe Ratio Project

### Overview

In this project demo, we will import price data for several assets from FRED and Yahoo! Finance. Then those data series will be cleaned, merged, and analyzed. After the data is prepared for financial analysis, we will calculate and compare the nominal returns, real returns, and risk premiums. In addition to analyzing the performance of these assets in regard to the average annual returns, we will also calculate the standard deviations of the returns to measure the volatility of each asset. Then the asset return correlations will be explored to determine the extent to which the assets move together. This leads into a comparison of the risk-adjusted returns, which will be measured using the Sharpe ratio, and the CAPM betas, which will further examine the relationships between the assets.

### Repository Structure

The data work for this project demo is contained in the R Notebook directory of this repository. On GitHub, the webpage should display the README.md file, which contains the compiled output of the R Notebook. If you wish to explore the source code locally, then you can open the sharpe.Rmd file in RStudio and execute the code chunks to replicate the data work. Note the `output: html_notebook` line in the header of that file, which indicates that the R Markdown document is an R Notebook. 

After exploring the R Notebook and making any desired changes, you can then create a copy that will appear on GitHub. To do this, save a copy of the R Notebook and name it README.Rmd. Then, change the header line to `output: github_document`, which will switch the file from being an R Notebook to an R Markdown file that will compile into a generic [Markdown](https://www.markdownguide.org/) file (.md). This format (along with the README name) will automatically be recognized by GitHub and displayed in-browser. This will also replace the Preview button with an option to Knit the Markdown file. This knitting process will re-run all the code chunks and generate a new README.md file inside of the R Notebook folder, which will display on GitHub.
