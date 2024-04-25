# Project Proposal 2

## 1. High-level Goal
In this project, we explore the different facets of gender pay gaps in the United Kingdom. Despite advancements in workplace equality, wage disparities between genders remain a significant challenge across various sectors. Utilizing data from the UK Pay Gap initiative, our objective is to create an interactive dashboard that not only highlights these disparities but also provides insights into their dynamics over time and across different industries. This visual tool aims to foster a deeper understanding of the underlying factors contributing to pay inequality.

## 2. Description
- **Objective:** The primary goal is to construct a comprehensive interactive dashboard that visualizes different aspects of the gender pay gap across various sectors—ranging from technology and finance to education and healthcare. This dashboard will not only display current disparities but also track changes over time, providing a temporal dimension to our analysis.
- **Approach:** We intend to use Shiny framework to present this complex data in an accessible and engaging manner, which is a dashboard. Through interactive charts and graphs, users will be able to explore different layers of the data, such as comparisons by sector and changes over the years.
- **Impact:** The intended impact of our visualization is to increase awareness and understanding of the pay gaps within different sectors of the UK economy.
- **Innovation:** Our approach is innovative in its use of interactive elements to engage users actively. By allowing users to navigate through different aspects of the data, users will have a more profound engagement with the issues and develop better understanding of how pay gaps affect various sectors differently.
- **Variables used:**

| Variables | Description |
| --- | --- |
| `EmployerName` | The name of the employer at the time of reporting |
| `DiffMeanHourlyPercent` | Mean % difference between male and female hourly pay (negative = women’s mean hourly pay is higher) |
| `MaleLowerQuartile` | Percentage of males in the lower hourly pay quarter |
| `FemaleLowerQuartile` | Percentage of females in the lower hourly pay quarter |
| `MaleLowerMiddleQuartile` | Percentage of males in the lower middle hourly pay quarter |
| `FemaleLowerMiddleQuartile` | Percentage of females in the lower middle hourly pay quarter |
| `MaleUpperMiddleQuartile` | Percentage of males in the upper middle hourly pay quarter |
| `FemaleUpperMiddleQuartile` | Percentage of females in the upper middle hourly pay quarter |
| `MaleTopQuartile` | Percentage of males in the top hourly pay quarter |
| `FemaleTopQuartile` | Percentage of females in the top hourly pay quarter |
| `EmployerSize` | Number of employees employed by an employer

- **Github Link:** https://github.com/rfordatascience/tidytuesday/tree/master/data/2022/2022-06-28  



## 3. Question for Analysis 
**Theme:** On each sector, what are the paygap, paygap change over year, and distribution of population among gender? 
- Question 1: How is the difference in paygap with respect to the hourly pay in these sectors (i.e. Technology, Airline, Pharmaceutical/Medical, Financial, Bank, Hotel, Consulting, Accounting, Marketing, School/Education, Travel, etc.)?
    - Variables: `current_name`, `diff_mean_hourly_percent`
    - Approach: 
        - Utilize a bar chart, which displays as a pyramid, on each sector
        - Create a data frame for all companies in the same sector. Place `diff_mean_hourly_percent` on x_axis and `current_name` known as the company name on y_axis. This could be achieved by using ggplot2.
        - Ensure the chart provides a consistent theme among each sector’s plot and use a contrast color scheme to distinguish between the genders.

- Question 2:
    - Variables:
    - Approach:
- Question 3:
    - Variables:
    - Approach:


## 4. Data Preprocessing 
As we want to do our analysis broadly by sectors, we will create a new data frame to filter out companies belonging to targeted sectors. Potential notable sectors may include, but not limited to Technology, Airline, Pharmaceutical/Medical, Finance, Banking, Hospitality, Consulting, Accounting, Marketing, Education, and Travel. This new data frame consists of three columns: `EmployerName`, `EmployerID`, and the `Sector` it belongs to. Companies that cannot be categorized into our target sectors will be categorized as “Others”. Our filtering and categorizing plan involves three steps:

**Step 1:** Group by EmployerID, store SIC code, EmployerID, EmployerName, count occurrence. The raw pay gap data frame has record of company over many years, so grouping them reduces repeated categorizing. Also, we want to only analyze companies that have complete records from 2017 to 2023. Assuming that companies only submit one report each year, remove companies that have < 6 count.

**Step 2:** In code, employ keyword-based filtering. Create a list of keywords that each company belonging to a sector may have in their name and categorize accordingly. For instance, Employer with EmployerName == Bank of England belongs to Banking sector. 

**Step 3:** For the remaining uncategorized companies, we will merge by SIC code with `SIC07_CH_condensed_list_en` data frame to get `description`, which describes the employer's purpose and sectors of work at the time of reporting, to manually categorize. Recursively add new keywords representing sectors that we may have missed in step 2.

## 5. Weekly plan
| Week | Milestone | Description |
| --- | --- | --- |
| 1    | Kick-off | - Meet the new team <br> - Establish the working style <br> - Research & brainstorm the idea |
| 2    | Project Proposal | - Choose the idea via combining and selecting each member's idea <br> - Draft the proposal |
| 3    | Project Proposal | - Finalize the proposal (including the research question and main method) <br> - Submit proposal and its related documents |
| 4    | Revised Proposal & Peer Review | - Conduct peer review <br> - Revise the proposal and reply to our peers and instructors' feedback <br> - Set up the code structure <br> - (If necessary) Learning the framework Shiny to build a dashboard |
| 5    | Hands-on & Implementation | - Learn the framework Shiny <br> - Plot all graphs (its content is prioritized) |
| 6    | Hands-on & Implementation | - Build a dashboard to display all graphs <br> - Preparing the theme for both graph and hosting dashboard |
| 7    | Hands-on & Implementation | - Finalize the dashboard and graph (content and theme must be completed) <br> - Prepare the content of report and presentation |
| 8    | Finalizing stage | - Finalize source code/repository & report & presentation <br> - Submit our work |

