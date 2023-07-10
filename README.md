# AlbatrossPhillipinesLWR
**Length-weight relationship and condition factor of museum specimens preserved in ethanol**

<br>

### Purpose 

----

To examine the effect ethanol preservation shrinkage has on reported length-weight relationship and condition factor, and to establish a correction factor for each species.

<br>

### Methods

----

Individual specimens were taken out of solution, with the mouth and gill cavities drained before being dried using Kimtech wipes. Individuals who had their abdominal cavities cut were drained and gently squeezed to remove excess ethanol. The standard and total lengths (mm) were measured using mechanical calipers. The mass (g) was measured using an analytical balance. Individuals were then returned to their respective lot jars and topped off with 75% EtOH if necessary. This process was kept consistently within 1-3 minutes, during which time ethanol also evaporated. For individuals with tags, the original string was cut in placed back in the lot jar, while the tag was retied using clear fishing wire after measurements were taken.

<br>

The portion of the _Albatross_ Phillipine exedition collection sampled from was that which was available to me in the Carpenter Molecular Systematics lab (ODU) as of March 2023. The selection of species in the lab were what was being tageted by the Phillipines PIRE project. Species were selected based on the availability of reported fresh-caught length-weight relationship (LWR) from within the Phillipine archipelago.

<Br>
 
 Length-weight Relationship W=aL^b
 
 Fulton's Condition Factor  K=100(W/SL^3) - for comparison to an ideal weight
 
 Le Cren's Relative Condition Factor K_n = W/aL^n - for comparison to the average weight
 
 Goal of project in R is to produce graphs of these three formulas and to compare the resulting _a_ and _b_ values to those from fresh studies found in the rfishbase database using log10a and b.

 <br>

## Navigating Files

----

### Albatross_Raw_Data

Includes the length and weight measurement data for each species, organized by species as well as together in the Albatross_LWR_Raw_Data.xlsx folder. The Calculated_variables.xlsx lists values of _a_ and _b_, residual standard error (RSE), min and max collected lengths, location, and collection date.


### Commands

Contains the R code that produces figures for each species. Running commands in the order listed will produce the ..._LWR_SL figure with no annotation first, the ..._log10a_b figure showing the comparison to other reported LWR studies, and the ..._LWR_SL_2 figure showing annotations.


### Figures

Contains figures for each species for which Albatross LWR data was collected. 



