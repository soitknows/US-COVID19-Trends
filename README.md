# U.S. COVID-19 Trends

R Shiny app to plot COVID-19 confirmed cases over time for United States counties. 


**Parameters:**

* Mode
	* `Single County` - single line plot output
	* `County Comparison` - two line plot output
* Output
	* `Cases` - confirmed case count
	* `Cases / Population` - confirmed case count divided by the county's estimated 2019 population.
* State
	* State Name
* County
	* County Name (choices dependent upong State parameter)

**Data Sources:**

* [Johns Hopkins CSSE COVID-19 Data Repository](https://github.com/CSSEGISandData/COVID-19)
* [United States Census](https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/)

