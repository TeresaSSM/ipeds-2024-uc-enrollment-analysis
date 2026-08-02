# UC San Diego IPEDS Enrollment Benchmarking Report

An institutional research report benchmarking UC San Diego's enrollment composition 
and demographic representation against its eight UC system peer campuses, using 
federal IPEDS data. Built end-to-end in R, from raw data cleaning and visualization, to a PDF
report and a formatted Excel deliverable.

## Key Findings

UCSD is the third-largest UC campus by total enrollment, behind only UCLA and 
  Berkeley, and has the third-highest graduate enrollment share (21%) in the system. UCSD draws an unusually large international graduate student population; 44% of 
  graduate enrollment is nonresident, compared to a 32% UC system average. Hispanic students are underrepresented at UCSD relative to the UC average at both 
  the undergraduate level (27% vs. 31%) and, more sharply, the graduate level (9% vs. 
  15%). Black students are the least-represented racial group across the UC system as a 
  whole, and UCSD's Black enrollment (2%) falls even below that already-low systemwide 
  average (3%) at both levels.


## Tools

R, R Markdown, tidyverse, openxlsx

## Files

- `ipeds_analysis.R` — full analysis script
- `IPEDS-report.Rmd` — report source
- `IPEDS-report.html` — rendered report
- `excel/IPEDS-project-excel-generation.r` — script generating the Excel deliverable
- `excel/IPEDS_tables.xlsx` — formatted Excel output
