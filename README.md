# Linking humans, dogs, and patterns of dispersal to stress physiology of California ground squirrels in a semi-urban park

## Code and data for analyzing fecal glucocorticoids of California ground squirrels in an urban park

## NOTE: Data is available upon request by the corresponding author!! 

### 1) Data:
- Data_for_2013_2024_model.csv (cort data at both sites between 2013-2024 - model 1 in code)
- Data_for_2018_2024_model_disturbed_site.csv (cort data within disturbed site only between 2018-2024 - model 2 in code)
- Data_for_dispersal_model.csv (cort data of individuals dispersing permanently between the two sites - model 3 in code)

  columns:
  - lnCort: natural log of fecal glucocorticoids
  - day.s: scaled day of the year (numeric)
  - site: 1 for more disturbed, 0 for less disturbed
  - stage: A for adult, P for pup (juvenile)
  - sex: F for female, M for male
  - mass_div_100: body mass [g] divided by 100
  - year: year of capture
  - area: name of natural landmark where squirrel was trapped
  - uid: unique identifier of squirrel
 
### 2) CODE_brms model cort:
contains the code necessary to replicate analyses and recreate figures. Note that the model outputs were too large to upload to github, they will be uploaded to dryad upon acceptance of the manuscript. Models can be run with the supplied code and data. 

### 3) Output tables:
contains all output tables of VIF calculations, effect sizes, comparisons of marginal means etc.

### 4) figures:
contains all figures
