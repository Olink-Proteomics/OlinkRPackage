# Olink Analyze 4.3.1
## Bug Fixes
* `olink_lod()` call to `checkNPX()` now correctly uses data instead of df, allowing the function to run without error (#579, @klevdiamanti)
* broken link in bridging tutorial has been fixed (@amrita-kar)
* minor edits have been made to the bridging tutorial and `olink_bridgeselector()` to clarify how to set `sampleMissingFreq` argument for different products and biological contexts (@amrita-kar, @kathy-nevola)
* minor edits have been made to the bridging tutorial to clarify that Ct and count data is not normalized during bridging and to define a batch (@amrita-kar)

# Olink Analyze 4.3.0
## Minor Changes
* Recommended Olink Reveal bridge sample numbers have been added to the bridging tutorial (#523, @kathy-nevola)
* Missing meta-data in parquet files now result in a warning instead of an error (#540, @OlaCaster)
* Pathway enrichment updated to support msigdbr v10.0 (#542, @klevdiamanti)
* Support for PanelVersion and LODNPX was added (#551, #552, #559, @klevdiamanti, @MasoumehSheikhi)
* Support for new version of warning message when converting LME results to tidy format has been added (#565, @kathy-nevola)
* Improvements made to user interface for olink_bridgeability_plot() (#525, #522, @klevdiamanti)

## Bug Fixes
* Fixed bug causing missing significance stars in olink_boxplot (#536, @boxizhang)
* Files with multiple quantification files will now select the quantification method to use (#563, @kristynchin-olink, @amrita-kar)
* OlinkIDs with multiple UniProt IDs across different panel versions will now be harmonized to one UniProt ID to allow for assays to be analyzed together (#564, @kathy-nevola)

# Olink Analyze 4.2.0
## Minor Changes
* Cross-product bridging is now supported for bridging Olink Explore 3072 to Olink Reveal (#509, @kathy-nevola)
* Standard data exports from NPX Signature 2.0 are now supported (#511, @KangD-dev)
* Kang Dong and Stephen Pollo have been added as authors (#514, @kathy-nevola)
* MaxLOD will be recalculated after bridging to reflect the MaxLOD across all projects (#505, @klevdiamanti)

## Bug Fixes
* A warning message will be displayed when fewer than the recommended number of bridging samples are used in cross product bridging (#507, @klevdiamanti)
* If count data is not provided in cross-product bridging, an error message will occur (#510, @kristynchin-olink)
* For cross-product bridging, if the sample type and assay warning columns do not have the same column naming (ex: SampleType and Sample_Type), both columns will be included in the output. (#515, @spollo-olprot, @MasoumehSheikh)


# Olink Analyze 4.1.1
## Minor Changes
* Images were decreased in dimension to meet CRAN Size requirements (#500, @kathy-nevola)

## Bug Fixes
* Dataframe name was corrected in Olink Explore 3072 to Olink Explore HT bridging tutorial Downstream Analysis section. (#500, @kathy-nevola)


# Olink Analyze 4.1.0
## Minor Changes
* Functionality has been added to read_NPX, olink_lod, and olink_plate_randomizer to support additional data types (#487, @kathy-nevola)
* A new function has been added to visualize the criteria used to establish the recommended bridging method for an assay in cross-product bridging (#468, @amrita-kar)
* A new function has been added to simplify data wrangling on bridged data after cross-product bridging (#472, @dtopouza)
* The tutorial on Olink Explore 3072 to Explore HT bridging has been updated to reflect the new functions (#473, @kristynchin-olink)
* Documentation for plate randomization when keeping samples from the same studies together has been improved in function documentation and the Plate Randomization tutorial (#483, @kathy-nevola)
* Contact us and legal disclaimer information has been updated and added to all tutorials (#490, @kathy-nevola)
* A Research Use Only (RUO) message is now printed when reading parquet files (#487, @kathy-nevola)
* References to NPX Explore have been generalized to Olink Software in tutorials (#490, @kathy-nevola)
* The overview tutorial has been updated and streamlined (#473, @kristynchin-olink)

## Bug Fixes
* Assay CTRL is no longer characterized as a control assay (#484, @dtopouza)
* An error that had previously limited Explore HT to Explore HT bridging has been resolved (#491, @klevdiamanti)
* The minimum number of bridge samples for QS normalization has been corrected to 40 samples.
* The Block column is now transformed into a character value to ensure consistency across products.
* An error that had previously impacted paired Wilcoxon tests has been resolved. (#485, @boxizhang)


# Olink Analyze 4.0.2
## Bug Fixes
* Product is now identified by column name for cross-product normalization, rather than OlinkIDs. This allows for non-overlapping assays to be present in input files. (#469, @kathy-nevola)
* Added many-to-many mapping in joining Explore HT data to the mapping file to support correlation assays (#469, @kathy-nevola)
* Corrected spelling in error message. (#469, @kathy-nevola)
* Fixed typo in NotBridgeable category name in tutorial (#471, @dtopouza)


# Olink Analyze 4.0.1
## Minor Changes
* larger rds files used for unit tests have been excluded from build to abide by CRAN size limitations (#462, @kathy-nevola)

# Olink Analyze 4.0.0
## Major Changes
* olink_normalization can now be used for bridging Explore 3072 data to Explore HT data (#453, #452, #439, #449, #447,#446, #441, #440, #434, #422, #436, #435 @klevdiamanti, @kathy-nevola, @kristynchin-olink, @dtopouza, @amrita-kar, @MasoumehSheikh)
* olink_normalization has undergone major changes to improve speed, reproducibility, and extending functionality (#418, @klevdiamanti, @kristynchin-olink, @MasoumehSheikh)


## Minor Changes
* New tutorial has been added to explain and support Explore 3072 to Explore HT bridging (#448, #428, #427, #426, #425, #424, #423, @kristynchin-olink, @boxizhang, @dtopouza, @kathy-nevola, @MasoumehSheikh)
* Update FAQ links in tutorials with queries links (#451, @kathy-nevola)
* Decrease package size on Mac and Linux (#445, @kathy-nevola)
* Removed redundant information from overview vignette (#458, @kathy-nevola)

## Bug Fixes
* Add support back for files with quotes to read_NPX (#431 @kathy-nevola)

# Olink Analyze 3.9.1
## Bug Fix
* Extra set of parentheses removed from vignette (#430, @kathy-nevola)

# Olink Analyze 3.9.0
## Minor Changes
* Explore HT recommended bridging samples have been added to Introduction to Bridging tutorial (#409, @kathy-nevola)
* Support for CSVs with SampleQC column was added to read_NPX (#406, @kathy-nevola)
* Support for Olink Analyze Export parquets was added to read_NPX (#408, @kathy-nevola)
* Quantitative value csvs will now give a warning about limited support for Quant data (#406, @kathy-nevola)
* Instructions for importing multiple NPX files has been added to the overview tutorial (#403, @dtopouza)
* Additional background information was added to the LOD tutorial to clarify how LOD is calculated from counts (#404, @kathy-nevola)
* LOD can now be calculated using fixed LOD, negative controls, or both methods (#390, @kathy-nevola)
* An error message will now appear when running anova and control assays are present (#416, @dtopouza)
* Danai Topouza's role has been changed from contributor to author (#415, @kathy-nevola)

## Bug Fixes
* URLs in tutorials will now direct to updated olink.com locations (#402,  @kathy-nevola)
* Instructions to export parquet files with LOD have been updated (#408, @kathy-nevola)
* removed scale_name argument when ggplot2 3.5+ is installed (#421, @kathy-nevola)

# Olink Analyze 3.8.2
## Bug Fixes 
* update to URL hyperlink in LOD tutorial to include https
* data with Plate_LQL and PlateLQL columns in long format will now be supported (#392, @klevdiamanti)

# Olink Analyze 3.8.1
## Bug Fixes 
* update to outdated URL in LOD tutorial

# Olink Analyze 3.8.1
## Minor Changes
* Function support for calculating LOD for Explore data (#355, @kathy-nevola, @klevdiamanti, @leiliuC)
* Tutorial and documentation for calculating LOD for Explore data (#364, @MasoumehSheikh, @kristynchin-olink, #388, @kathy-nevola, @amrita-kar)
* Addition of Danai Topouza as contributor and Kristyn Chin as author (previously contributor) (#365, @kathy-nevola)

## Bug Fixes
* Removed outdated reference to NPX Manager (#354, @dtopouza)
* Removed mention of Welch's t-test in Mann Whitney documentation (#354, @dtopouza)
* Assays that include apostrophes will now be read in correctly from long format csvs (#381, @kathy-nevola)

# Olink Analyze 3.7.0
## Minor Changes
* Support for Explore 3072 data in parquet form added (#327, @kathy-nevola, @klevdiamanti)
* Support for pathway enrichment when LOD is not present added (#329, @kathy-nevola)
* Clarification to remove controls when selecting bridging samples added in bridging tutorial (#330, @kathy-nevola)
* Addition of Kristyn Chin as contributor (#331, @kathy-nevola)
* Addition of URLs and Contact information in Description (#331, @kathy-nevola)

# Olink Analyze 3.6.2
## Bug Fixes
* Packages in Suggest field are now called conditionally in vignettes, tests, and examples. (#319, @kathy-nevola)
* read_NPX will now work with Target 96 data that does not contain "Target 96" in the panel name (#320, @kathy-nevola)
* olink_lmer will no longer require the presence of an Index column in the data (#321, @kathy-nevola)
* corrected examples for subset normalization (#312 @kathy-nevola)
* added additional information on example data to outlier vignette (#313 @kathy-nevola)
* clarified documentation for longitudinal randomization (#314 @kathy-nevola)
* corrected warning message for olink_ordinalRegression (#296 @boxizhang)

# Olink Analyze 3.6.1
## Minor Changes
* Install package Matrix from source in CI. (#299 @klevdiamanti, @AskPascal)

# Olink Analyze 3.6.0
## Minor Changes
* Plate LOD will be chosen over Max LOD when both are present (#288, @kathy-nevola)
* Data from NPX Signature 1.8+ is now supported by read_NPX when in long format csv (#293, @Orbmac, @kathy-nevola)
* olink_pca_plot will now be faster (#289, @MasoumehSheikh)

## Bug Fixes
* olink_normalization and related function will now compare normalization strategy by assay (#291, klevdiamanti)
* Excluded assays will not cause warnings in olink_normalization (#291, klevdiamanti)
* olink_pca_plot will now show the same label and text for point when SampleID is numeric (#289, @MasoumehSheikh)

# Olink Analyze 3.5.1
## Bug Fixes
* read_NPX will now support additional formats of parquet files

# Olink Analyze 3.5.0
## Minor Changes
* read_NPX will now detect and import Flex excel files in wide format (@kathy-nevola, #234)
* Add example of bridge samples selector and minor updates for clarity to Intro to bridging vignette (@Orbmac, @kathy-nevola, #260)
* Increase plate randomizer unit test coverage (@amrita-kar, #264)
* Add support for parquet files in read_NPX (@kathy-nevola, #265, @klevdiamanti, #270)
* Add support for alternative forms of LOD including Max and Plate LOD (@kathy-nevola, #267)
* Add support for SampleQC column (alternative to QC_Warning) (@kathy-nevola, #267, @MasoumehSheikhi, @268)
* Add support for plate randomization with variable control numbers (@kathy-nevola, @AskPascal, #269)
* Controls can now be randomized across the plate with olink_plate_randomizer (@kathy-nevola, @AskPascal, #269)
* Minor updates for clarity to Plate Randomization vignette (@kathy-nevola, @AskPascal, #269)
* Change formatting for long format Target CSVs to match excel (@kathy-nevola, #273)

## Bug Fixes
* Project name is now consistent and generic throughout bridging vignette (@kathy-nevola, #257)
* Estimate is now included in output when Paired Mann-Whitney U test is performed (@boxizhang, #259)

# Olink Analyze 3.4.1
## Bug Fixes
* Skip PCA snapshot tests in R version > 4.2.3 (@AskPascal, #254)
* Bridge selector will now return all samples when requesting max number of samples based on missing frequency (@klevdiamanti, #252)
* Fixed unit tests to expect NA as logical instead of "NA" as character (@MasoumehSheikhi, #250)

# Olink Analyze 3.4.0
## Minor Changes
* User can now specify label for proteins in olink_heatmap_plot() (@simfor, #228)
* Updates to unit test and CI based on dependency changes (@AskPascal, #231, #236, #240)
* Addition of normalization functions to simplify bridge, subset, and multi-batch normalization (@klevdiamanti, #201)
* Introduction to bridging vignette added (@leiliuC, @kathy-nevola, #174)
* Plate Randomization vignette added (@boxizhang, #241)
* Outlier Exclusion vignette added (@kathy-nevola, #229)
* Added Lei Conze as author (@kathy-nevola, #246)
* Addition of citation (@kathy-nevola, #246)

## Bug Fixes
* Update to olink_wilcox documentation and UniProt description in documentation has been corrected (@boxizhang, #235)


# Olink Analyze 3.3.1
## Bug Fixes
* olink_pathway_enrichment now prints a message when there are non matching names when using method = "ORA" (@MasoumehSheikhi, #222)
* olink_pca_plot will now generate PCA when data is missing from the first OlinkID (@kathy-nevola, #221)
* read_NPX now supports csv files with Sample_Type column but not ExploreVersion column (@klevdiamanti, #220)
* extra columns in input file will no longer result in a warning message (@kathy-nevola, #223)


# Olink Analyze 3.3.0
## Minor Changes
* Support for additional versions of Olink data - Read_NPX now supports a wider range of Olink data types (@AskPascal, @kathy-nevola, #207, #208, #211, #216)
* Automatic support for normalizing 2 datasets with different column configurations - olink_normalization will now automatically add missing columns to datasets (filled NA) to allow data with different configurations to be more easily normalized together (@kathy-nevola, #212)
* Automatic checking that datasets used the same normalization method before bridging - Datasets that were normalized with different methods (different values in normalization column) will now warn before performing normalization (@klevdiamanti, #210)

## Bug Fixes
* Data with '#' in SampleID column is now supported (@AskPascal, #208)
* PCA can now be generated when indices are not consistent across SampleIDs (@amrita-kar, #206)

# Olink Analyze 3.2.2
## Bug Fixes
* remove www. from links in vignette to prevent rerouting of URL (@kathy-nevola, #188)
* update set_plot_theme() to use linewidth instead of size per ggplot2 3.4

# Olink Analyze 3.2.1
## Bug Fixes
* Change in unit test to write to temporary directory (@AskPascal, #181)


# Olink Analyze 3.2.0

## Minor Changes
* Addition of functions to perform Uniform Manifold Approximation and Projection (UMAP) dimensional reduction and plots (@simfor, #139)
* Add additional install methods to Readme (@AskPascal, #153)
* ggrepel can now be disabled when outlier lines are present in the PCA plot (@klevdiamanti, #158)
* Long running unit tests are now skipped on CRAN (@AskPascal, #163)
* Internal functions were added to read_NPX to support future development (@klevdiamanti, #167)
* CI workflows were refactored to utilize external actions (@AskPascal, #169)
* Read_NPX will now warn the user when NAs are detected in the NPX column (@AskPascal, #170)
* Friedman test interface and documentation was updated to be more intuitive (@boxizhang, #171)


## Bug fixes
* Pathway enrichment p-values are now in the correct order when plotting (@klevdiamanti, #164)
* PCAs now behave the same with any locale (@AskPascal, #173)
* Read_NPX now accepts either Panel_Version or Panel_Lot_Nr in input files (@klevdiamanti, #156)
* Assays that only have NPX = NA will now be excluded from all analyses and figures (@simfor, @kathy-nevola, @AskPascal, #176)
* Refactor code to use tidyselect::all_of() in recommended way (@AskPascal, #177)
* vdiffr based unit tests were reactivated (@AskPascal, #172)

# Olink Analyze 3.1.0

## Minor Changes

* Non-parametric functions are now available (@boxizhang, #114, #142)
* Updated installation instructions to reflect CRAN acceptance (@kathy-nevola, #107)
* Zipped files from MyData can now be used as input for read_NPX (@klevdiamanti, #115)
* LME and ANOVA formulas can now be customized (@jrguess, #120)
* Pathway enrichment functions and visualizations are now available (@kathy-nevola, #125)
* Heatmaps can now be generated (@Orbmac, $127)
* olink_boxplot function now has significance bars and stars (@boxizhang, #132)
* PCA preprocessing was moved to internal function (@simfor, #133)
* olink_plate_randomizer will now return a warning if there are duplicate SampleIDs or if SubjectColumn is missing (@kristianHoden, #146)
* olink_plate_randomizer can now keep studies together on plates (@kristianHoden, #146)
* Added Masoumeh Sheikhi and Boxi Zhang as authors
* Added Kristian Hodén as contributor

## Bug Fixes

* olink_pca_plot by Panel will now show correct colors when a variable is missing (@MasoumehSheikhi, Issue #117, Commit 0f2f157) 
* olink_ttest will now return a warning message if an assay has less than 2 datapoints in a group. (@marisand, #110)
* LMER class is now checked using inherits (@MasoumehSheikhi, #134)
* License was corrected to AGPL-3 (@Orbmac, #138)
* Correct output type of olink_dist_plot in vignette to ggplot object (@kathy-nevola, Issue #112, #141)
* Previously called "intensity normalization" has been clarified as a special type of subset normalization and an example has been added to the documentation and vignette (@Orbmac, #144)


# Olink Analyze 3.0.0

## Major Changes

* displayPlateDistributions and displayPlateLayout names were updated to olink_displayPlateDistributions and olink_displayPlateLayout (@simfor, #98)

## Minor Changes

* Update CI to Ubuntu 20.04 (@AskPascal, #97)
* olink_bridgeselector will now give an error if less than n (number of) bridge samples can be selected based on set sampleMissingFreq (@marisand, #100)
* update return value documentation for all functions to specify columns and class of output (@kathy-nevola, #102)
* replaced or removed cat/print with message/stop so messages printed to the console can be suppressed (@jrguess, #103)

## Bugfixes

* fixed spelling mistakes in documentation (@kathy-nevola, #92)
* updated DESCRIPTION to fit CRAN specification (@kathy-nevola, #94)
* change T/F to TRUE/FALSE for stability (@AskPascal, #96)
* updated documentation to olink_plate_randomizer, olink_displayPlateDistributions and olink_displayPlateLayout to link related functions and clarify olink_plate_randomizer documentation (@simfor, #98)
* fixed keywords in documentation (@kathy-nevola, #99)


# Olink Analyze 2.0.1

## Bug Fixes

* Remove hexagon from Readme (@kathy-nevola, #86)
* Replace OlinkAnalyze with Olink® Analyze (@kathy-nevola, #86)
* Add Ola Caster to author list
* Update documentation to change olinkR to Olink Analyze (@jrguess, #89)

# Olink Analyze 2.0.0

## Major Changes

* Package is in the process of being submitted to CRAN
* Added ability to prevent OlinkAnalyze from loading fonts and setting to themes (@OlaCaster, #73)
* Decreased the size of npx_data1 and npx_data2 to 2 panels instead of 12 (@kathy-nevola, #76)
* Updates to the olink_qc_plot and olink_pca_plot functions (@simfor, #78)

## BugFixes

* Moved NEWS.md to correct level (@kathy-nevola, #74)
* Added cran-comments.md file to document Notes for CRAN submission (@kathy-nevola, #74)
* Update Vignette to reflect new functionality (@kathy-nevola, #78)


# Olink Analyze 1.3.0

## Major Changes

* DESCRIPTION file updated to include all authors and maintainers (@kathy-nevola, #66)
* Unit testing was added (@marisand, @simfor, #65, #55, #47)
* Continuous Integration (CI) was added (@AskPascal, #57, #53, #40, #31)
* `read_NPX` now converts Panel Names to Sentence case except Roman Numerals (@kathy-nevola, #44)
* Output order of `olink_bridgeselector()` is now randomized (@marisand, #60)
* PlateSize argument was added to `displayPlateLayout()` and `olink_plate_randomizer` for T48 and T96. Hardcoded sizes have been removed. (@marisand, #47)
* Changed `olink_pal()` to have gray instead of light blue (@marisand, #22)

## Bug Fixes

* `set_plot_theme()` will now load Swedish Gothic Thin if available on all OS (@marisand, @AskPascal, #70, #39)
* Help documentation was updated to correct typos and clarify notation for ANOVA and LME models (@kathy-nevola, @marisand,@AskPascal #67, #33, #37)
* Fix `olink_dist_plot()` from showing multiple bars when sample has QC warning in some assays (@marisand, @AskPascal, #64)
* Fix `olink_qc_plot()` from showing multiple points/labels when sample has QC warning in some assays (@marisand, @AskPascal, #63)
* Panel names in npx_data1 and npx_data2 were fixed to match new `read_NPX()` formatting (@kathy-nevola, #62)
* `olink_bridgeselector()` QC warning filtering changed to match `olink_qc_plot()` and `olink_dist_plot()` (@marisand, @AskPascal, #61)
* global variables are now specified (@kathy-nevola, #45)
* Function documentation updated to remove notes on R CMD check (@kathy-nevola, #39)
* Fix summary casting "tukey" to "sidak" adjustment warning in `olink_anova_posthoc()` and `olink_lmer_posthoc()` functions (@marisand, #38)
* Update functions to import selectively (@kathy-nevola, @Orbmac, @AskPascal, @marisand, #21, #20, #19, #18, #15, #29)
* Fix guides size argument in `olink_pca_plot()` (#17) (@AskPascal, #24)






