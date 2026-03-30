**Data Entry Guide - How to enter your data into the Data Template**

1. **Transfer relevant data from your datasheet to the STOICH data template **

   a. Begin by entering required data. If not all required data is available, see FAQs for more details on what to do with an incomplete dataset. 

      i. Pay special attention to making sure the C, N, and/or P values are associated with whole-organism or whole-resource (homogenous) samples.

      ii. The STOICH project is most interested in whole-organism samples for accurate representation of whole-body stoichiometry. If you include tissue-specific samples or other partial-organism samples in your dataset, you must indicate this in the column "Stoichiometry\_Origin\_Organism." 

      iii. Make sure the organism or resource sample is collected from the field or a natural setting.

   b. Expand hidden cells by selecting all cells and then using the menu bar to select Format → Column → Unhide

   c. Fill out the template with your remaining data, making notes when your dataset has additional data that does not fit into the template (see FAQs).

   d. If you are submitting multiple publications worth of data with distinct datasets, please duplicate the "Template" tab and save each distinct dataset in its own tab or as it's own document.

2. **Email the completed data template to STOICH.project.contact@gmail.com** 

   a. Indicate whether you have interest in co-authorship on the manuscript accompanying the initial data release.

   b. Your response to STOICH.project.contact@gmail.com with data, either raw or in the STOICH data template, indicates that you consent to publication of your data in the STOICH database and acknowledge its public use under Creative Commons Attribution-NonCommercial 4.0 International License.

**FAQs**

1. **What data is required for a complete dataset?**

Complete datasets must have the data enterer's name and the date of data entry as well as the site name, the sample date, and stoichiometric measurements of organic material that include at least two of the following elements with their associated units: carbon, phosphorus, and/or nitrogen.

Columns within the STOICH data template that indicate required data contain red cells until the cells are filled. These columns are as follows: 

Data\_entry\_name

Data\_entry\_date

Site\_name

Date

At least two of the three following, *with their associated units (indicated in yellow)*: 

C\_content\_Organism

P\_content\_Organism

N\_content\_Organism

2. **How do I reveal hidden columns?**

To reveal all hidden columns in the excel spreadsheet, select all of the columns and then use "Format" in the menu bar to select "Column", and click on "Unhide."

3. **What if I don't have all of the required data?**

If you don't have the required data, but think your dataset is still relevant to STOICH, please email us at STOICH.project.contact@gmail.com.

4. **What if my dataset contains extraneous or additional data?**

If the dataset you are attempting to enter into the template contains extra data (data that doesn't fit into the template), make the comment "additional data" in the appropriate notes sections (i.e. Notes\_Water) and create a new tab in the data template labeled "additional data." Add your additional data to the "additional data tab." Please include any relevant metadata.

5. **What do the colors Mean?**

**Column Heading Colors**

Each color correlates to a data type (i.e. blue represents water chemistry data). 

**Black Column Heading **

Information regarding the data enterer

**Gray Column Heading **

Data specific to the site location, spatial information

**Green Column Heading **

Data specific to each individual sampling event, temporally influenced information

**Yellow Column Heading **

Organism (living or dead at the time of collection) data and metadata

**Blue Column Heading **

Water chemistry data

**Orange Column Heading **

Data source information

**Yellow Subheadings**

Yellow subheadings   
Indicate a validated cell that is not required to be filled, but does contain specific requirements for entry (i.e. selection via dropdown menu). 

**Red Cells**

      Blank red cells indicate compulsory data. These red columns are validated cells which will remain red until filled. 

Exception: All *C\_content\_Organism*, *N\_content\_Organism*, and *P\_content\_Organism* cells are red. However, you are only required to have data for two of these fields.

**Yellow Cells**

      Blank yellow cells indicate compulsory metadata ONLY when there is associated data. These yellow columns are validated cells which will remain yellow until filled. 

Example: Completing the cell for the column, C\_units\_Organism, is only required when there is associated data in the C\_content\_organism column.

6. **Experimental/lab and field-collected data**

If you have both lab or experimental data as well as field data, only enter the data collected in the field into the STOICH data template. 

See additional data for more detailed instruction on options for what to do with lab data.

See the document, "Data Inclusion Requirements" in the Contributor Resources folder for more details on specific data requirements.

7. **Marine and inland data**

If you have both inland and non-inland data, only enter the inland or estuarine data into the STOICH data template. 

See additional data for more detailed instruction on options for what to do with lab data.

See the document, "Data Inclusion Requirements" in the Contributor Resources folder for more details on specific data requirements.

8. **Data is in the wrong units**

**Wrong Units**

Please transform your units to match those that we have provided in the "units" column. We have provided units that are common for each type of measurement, and would like the units to remain standardized across the STOICH database for optimized user experience. 

See below for resources for unit conversion in R

https://www.rdocumentation.org/packages/units/versions/0.8-0

https://r-quantities.github.io/units/reference/units.html

**Wrong Coordinate System**

Lat or Long   
If the site location information is not lat/long in decimal degrees (DD.DDDDD), use google maps (https://www.google.com/maps) to convert the current coordinate system to lat/long. 

9. **My data is transformed data, now what?**

**Corrected data **

If your data has been transformed in order to standardize or correct the data to a more accurate value (i.e. due to drifting analytical equipment), please use this data, and not the raw data. Add a note in the associated note column. 

If you would like to add additional transparency, please consider following additional data guidelines.

**Statistical transformations**

In some data columns, the mean and standard deviations are accepted. However, log transformation and other similar changes to raw data are not accepted in the template. Please use only your raw data, unless your data falls under the category of "corrected data." 

10. **Partial organism or tissue-specific stoichiometry - does it count?**

At least some of your stoichiometry data needs to be derived from a whole-organism or whole-body sample. The STOICH project is most interested in whole-organism or whole-resource stoichiometry. If there is partial organism stoichiometry in addition to whole-organism stoichiometry, make a note in the Stoichiometry\_Origin\_Organism column. 

Exceptions to the whole-organism rule:

**Gut clearance** is acceptable

**Shell removal** is acceptable

11. **Multiple water samples correlate with a single organism sample**

Example scenarios - multiple water samples at various depths, but single plankton tow covering the entire water column.   
Solution: correlate one water sample with the single plankton sample, leave the rest of the organism samples blank to avoid replication. 

12. **Multiple organism samples correlate with a single water sample**

Example scenarios - single water sample, but multiple plankton species measured for stoichiometry from a single tow.   
Solution: correlate water sample with only one plankton species, leave the rest of the water samples blank to avoid replication. 

