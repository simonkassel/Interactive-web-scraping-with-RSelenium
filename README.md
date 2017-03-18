# Interactive-web-scraping-with-RSelenium
RSelenium web-scraping example

Simon Kassel
From MUSA620: Data Wrangling and Visualization @UPenn
Instructor: Max Galka

This script scrapes home value and square footage data from the Philadelphia
 Property Assessor's database. I have determined the prices per square foot
 for approximately 900 homes within the proximity of Rittenhouse Square.
 
 <br>
 
 It takes as an input:
   - A dataset of home addresses and unit numbers (columns 1 and 2 identify
     the address and unit number's respectively)
 
 <br>
 
 And it returns:
   - The original csv with added fields for home value, square footage, price
     per square foot and X/Y coordinates
   - Data visualizations associated with home value/square foot in the area
