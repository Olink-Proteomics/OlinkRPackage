## Resubmission
This is a resubmission. In this version I have:

* Updated keywords to fix previous notes
* Removed the word 'Package' from the start of the Package title and description.
* Added descriptive details about the packages functionality and methods in the description text.
* Replaced all instances of T and F with TRUE and FALSE
* Improved \value in all functions to include structure of output (class) and what the output means including documenting columns of output. For functions that do not return a value, the \value has documented that as well.
* Removed print()/cat() calls and replaced with message()/stop(). Function olink_pca_plot() uses print() to print ggplot objects but is controlled by the quiet argument (TRUE/FALSE) so can be surpressed
* Removed the License file and reference to the license file in the description file.
* displayPlateDistributions and displayPlateLayout names were updated to olink_displayPlateDistributions and olink_displayPlateLayout
* updated documentation to olink_plate_randomizer, olink_displayPlateDistributions and olink_displayPlateLayout to link related functions and clarify olink_plate_randomizer documentation
*  olink_bridgeselector will now give an error if less n number of bridge samples can be selected based on set sampleMissingFreq

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Kathleen Nevola <biostattools@olink.com>'

New submission

Possibly misspelled words in DESCRIPTION:
  MyData (6:16)
  NPX (5:27, 5:70)
  Olink (3:51, 5:10, 5:64, 6:67, 11:56)
  Proteomic (3:31)
  QUANT (6:51, 7:22)
  proteomic (4:66, 11:30)
  
This is a new submission. Flagged words are not misspelled but rather refer to subject specific lexicon. Olink, MyData, QUANT, and NPX refer to specifics with Olink data, while proteomic refers to omic data with proteins of which Olink proteomic data is a subset of.
