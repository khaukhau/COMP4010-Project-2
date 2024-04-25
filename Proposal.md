Init proposal file
## 4.	Data Preprocessing – Categorizing by Sector

As we want to do our analysis broadly by sectors, we will create a new data frame to filter out companies belonging to targeted sectors. Potential notable sectors may include, but not limited to Technology, Airline, Pharmaceutical/Medical, Finance, Banking, Hospitality, Consulting, Accounting, Marketing, Education, and Travel. This new data frame consists of three columns: `EmployerName`, `EmployerID`, and the `Sector` it belongs to. Companies that cannot be categorized into our target sectors will be categorized as “Others”. Our filtering and categorizing plan involves three steps:

**Step 1**. Group by EmployerID, store SIC code, EmployerID, EmployerName, count occurrence. The raw pay gap data frame has record of company over many years, so grouping them reduces repeated categorizing. Also, we want to only analyze companies that have complete records from 2017 to 2023. Assuming that companies only submit one report each year, remove companies that have < 6 count.

**Step 2.** In code, employ keyword-based filtering. Create a list of keywords that each company belonging to a sector may have in their name and categorize accordingly. For instance, Employer with EmployerName == Bank of England belongs to Banking sector. 

**Step 3.** For the remaining uncategorized companies, we will merge by SIC code with `SIC07_CH_condensed_list_en` data frame to get `description`, which describes the employer's purpose and sectors of work at the time of reporting, to manually categorize. Recursively add new keywords representing sectors that we may have missed in step 2.
