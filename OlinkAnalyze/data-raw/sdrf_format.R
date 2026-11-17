## code to prepare internal dataset `sdrf_column_dict` goes here
## based on https://r-pkgs.org/data.html#sec-data-sysdata
##
## Column specification for SDRF affinity-proteomics template
## Source: https://sdrf.quantms.org/templates/affinity-proteomics.html
## Inheritance: base → sample-metadata → affinity-proteomics
##
## Notes:
## - Official SDRF headers are the first entry in each col_names vector.
## - col_miss = FALSE means the column is required by the template.
## - comment[panel name] is recommended, but MUST be provided when
##   comment[platform] is a platform family that cannot uniquely imply
##   the panel (e.g. "Olink Target 96"). That conditional rule is not
##   encoded in col_miss and should be handled in validation later.
## - This is a draft design artifact; not yet wired into sysdata.R.

sdrf_column_dict <- dplyr::tibble(
  # internal keys used for alternative column names
  col_key = c(
    # required (base)
    "source_name",
    "assay_name",
    "technology_type",
    "technical_replicate",
    "data_file",
    # required (sample-metadata)
    "organism",
    "organism_part",
    "biological_replicate",
    # required (affinity-proteomics)
    "sample_type",
    "platform",
    # recommended (base)
    "sdrf_version",
    # recommended (sample-metadata)
    "cell_type",
    "disease",
    # recommended (affinity-proteomics)
    "panel_name",
    "sample_matrix",
    # optional (base)
    "sdrf_template",
    "sdrf_annotation_tool",
    "sdrf_validation_hash",
    # optional (sample-metadata)
    "tissue_supergroup",
    "pooled_sample",
    "material_type",
    "tissue_mass",
    "biosample_accession",
    "sampling_time",
    "treatment",
    "synthetic_peptide",
    "spiked_compound",
    "enrichment_process",
    # optional (affinity-proteomics)
    "instrument",
    "quantification_unit",
    "plate",
    "normalization_method",
    "dilution",
    "lot_number"
  ),
  # list of alternative column names matching the keys
  # first name = official SDRF affinity-proteomics header
  col_names = list(
    "source_name" = c(
      "source name",
      "source_name",
      "SourceName"
    ),
    "assay_name" = c(
      "assay name",
      "assay_name",
      "AssayName"
    ),
    "technology_type" = c(
      "technology type",
      "technology_type",
      "TechnologyType"
    ),
    "technical_replicate" = c(
      "comment[technical replicate]",
      "comment[technical_replicate]",
      "technical_replicate"
    ),
    "data_file" = c(
      "comment[data file]",
      "comment[data_file]",
      "data_file"
    ),
    "organism" = c(
      "characteristics[organism]",
      "organism"
    ),
    "organism_part" = c(
      "characteristics[organism part]",
      "characteristics[organism_part]",
      "organism_part"
    ),
    "biological_replicate" = c(
      "characteristics[biological replicate]",
      "characteristics[biological_replicate]",
      "biological_replicate"
    ),
    "sample_type" = c(
      "characteristics[sample type]",
      "characteristics[sample_type]",
      "sample_type",
      "Sample_Type",
      "SampleType"
    ),
    "platform" = c(
      "comment[platform]",
      "platform"
    ),
    "sdrf_version" = c(
      "comment[sdrf version]",
      "comment[sdrf_version]",
      "sdrf_version"
    ),
    "cell_type" = c(
      "characteristics[cell type]",
      "characteristics[cell_type]",
      "cell_type"
    ),
    "disease" = c(
      "characteristics[disease]",
      "disease"
    ),
    "panel_name" = c(
      "comment[panel name]",
      "comment[panel_name]",
      "panel_name",
      "Panel"
    ),
    "sample_matrix" = c(
      "characteristics[sample matrix]",
      "characteristics[sample_matrix]",
      "sample_matrix"
    ),
    "sdrf_template" = c(
      "comment[sdrf template]",
      "comment[sdrf_template]",
      "sdrf_template"
    ),
    "sdrf_annotation_tool" = c(
      "comment[sdrf annotation tool]",
      "comment[sdrf_annotation_tool]",
      "sdrf_annotation_tool"
    ),
    "sdrf_validation_hash" = c(
      "comment[sdrf validation hash]",
      "comment[sdrf_validation_hash]",
      "sdrf_validation_hash"
    ),
    "tissue_supergroup" = c(
      "characteristics[tissue supergroup]",
      "characteristics[tissue_supergroup]",
      "tissue_supergroup"
    ),
    "pooled_sample" = c(
      "characteristics[pooled sample]",
      "characteristics[pooled_sample]",
      "pooled_sample"
    ),
    "material_type" = c(
      "characteristics[material type]",
      "characteristics[material_type]",
      "material_type"
    ),
    "tissue_mass" = c(
      "characteristics[tissue mass]",
      "characteristics[tissue_mass]",
      "tissue_mass"
    ),
    "biosample_accession" = c(
      "characteristics[biosample accession number]",
      "characteristics[biosample_accession_number]",
      "biosample_accession"
    ),
    "sampling_time" = c(
      "characteristics[sampling time]",
      "characteristics[sampling_time]",
      "sampling_time"
    ),
    "treatment" = c(
      "characteristics[treatment]",
      "treatment"
    ),
    "synthetic_peptide" = c(
      "characteristics[synthetic peptide]",
      "characteristics[synthetic_peptide]",
      "synthetic_peptide"
    ),
    "spiked_compound" = c(
      "characteristics[spiked compound]",
      "characteristics[spiked_compound]",
      "spiked_compound"
    ),
    "enrichment_process" = c(
      "characteristics[enrichment process]",
      "characteristics[enrichment_process]",
      "enrichment_process"
    ),
    "instrument" = c(
      "comment[instrument]",
      "instrument"
    ),
    "quantification_unit" = c(
      "comment[quantification unit]",
      "comment[quantification_unit]",
      "quantification_unit"
    ),
    "plate" = c(
      "comment[plate]",
      "plate",
      "PlateID"
    ),
    "normalization_method" = c(
      "comment[normalization method]",
      "comment[normalization_method]",
      "normalization_method",
      "Normalization"
    ),
    "dilution" = c(
      "comment[dilution]",
      "dilution"
    ),
    "lot_number" = c(
      "comment[lot number]",
      "comment[lot_number]",
      "lot_number"
    )
  ),
  # requirement level from the affinity-proteomics template
  col_requirement = c(
    "required",     # source_name
    "required",     # assay_name
    "required",     # technology_type
    "required",     # technical_replicate
    "required",     # data_file
    "required",     # organism
    "required",     # organism_part
    "required",     # biological_replicate
    "required",     # sample_type
    "required",     # platform
    "recommended",  # sdrf_version
    "recommended",  # cell_type
    "recommended",  # disease
    "recommended",  # panel_name
    "recommended",  # sample_matrix
    "optional",     # sdrf_template
    "optional",     # sdrf_annotation_tool
    "optional",     # sdrf_validation_hash
    "optional",     # tissue_supergroup
    "optional",     # pooled_sample
    "optional",     # material_type
    "optional",     # tissue_mass
    "optional",     # biosample_accession
    "optional",     # sampling_time
    "optional",     # treatment
    "optional",     # synthetic_peptide
    "optional",     # spiked_compound
    "optional",     # enrichment_process
    "optional",     # instrument
    "optional",     # quantification_unit
    "optional",     # plate
    "optional",     # normalization_method
    "optional",     # dilution
    "optional"      # lot_number
  ),
  # which template layer defines the column
  col_source = c(
    "base",                  # source_name
    "base",                  # assay_name
    "base",                  # technology_type
    "base",                  # technical_replicate
    "base",                  # data_file
    "sample-metadata",       # organism
    "sample-metadata",       # organism_part
    "sample-metadata",       # biological_replicate
    "affinity-proteomics",   # sample_type
    "affinity-proteomics",   # platform
    "base",                  # sdrf_version
    "sample-metadata",       # cell_type
    "sample-metadata",       # disease
    "affinity-proteomics",   # panel_name
    "affinity-proteomics",   # sample_matrix
    "base",                  # sdrf_template
    "base",                  # sdrf_annotation_tool
    "base",                  # sdrf_validation_hash
    "sample-metadata",       # tissue_supergroup
    "sample-metadata",       # pooled_sample
    "sample-metadata",       # material_type
    "sample-metadata",       # tissue_mass
    "sample-metadata",       # biosample_accession
    "sample-metadata",       # sampling_time
    "sample-metadata",       # treatment
    "sample-metadata",       # synthetic_peptide
    "sample-metadata",       # spiked_compound
    "sample-metadata",       # enrichment_process
    "affinity-proteomics",   # instrument
    "affinity-proteomics",   # quantification_unit
    "affinity-proteomics",   # plate
    "affinity-proteomics",   # normalization_method
    "affinity-proteomics",   # dilution
    "affinity-proteomics"    # lot_number
  ),
  # boolean: TRUE if column is allowed to be absent
  # (recommended + optional → TRUE; required → FALSE)
  col_miss = c(
    FALSE, # source_name
    FALSE, # assay_name
    FALSE, # technology_type
    FALSE, # technical_replicate
    FALSE, # data_file
    FALSE, # organism
    FALSE, # organism_part
    FALSE, # biological_replicate
    FALSE, # sample_type
    FALSE, # platform
    TRUE,  # sdrf_version
    TRUE,  # cell_type
    TRUE,  # disease
    TRUE,  # panel_name (conditionally required; see file header note)
    TRUE,  # sample_matrix
    TRUE,  # sdrf_template
    TRUE,  # sdrf_annotation_tool
    TRUE,  # sdrf_validation_hash
    TRUE,  # tissue_supergroup
    TRUE,  # pooled_sample
    TRUE,  # material_type
    TRUE,  # tissue_mass
    TRUE,  # biosample_accession
    TRUE,  # sampling_time
    TRUE,  # treatment
    TRUE,  # synthetic_peptide
    TRUE,  # spiked_compound
    TRUE,  # enrichment_process
    TRUE,  # instrument
    TRUE,  # quantification_unit
    TRUE,  # plate
    TRUE,  # normalization_method
    TRUE,  # dilution
    TRUE   # lot_number
  ),
  # boolean: TRUE if multiple same-key columns are allowed (SDRF multi-cardinality)
  col_multi = c(
    FALSE, # source_name
    FALSE, # assay_name
    FALSE, # technology_type
    FALSE, # technical_replicate
    FALSE, # data_file
    FALSE, # organism
    TRUE,  # organism_part
    FALSE, # biological_replicate
    FALSE, # sample_type
    FALSE, # platform
    FALSE, # sdrf_version
    TRUE,  # cell_type
    FALSE, # disease
    FALSE, # panel_name
    FALSE, # sample_matrix
    TRUE,  # sdrf_template
    FALSE, # sdrf_annotation_tool
    FALSE, # sdrf_validation_hash
    FALSE, # tissue_supergroup
    FALSE, # pooled_sample
    FALSE, # material_type
    FALSE, # tissue_mass
    FALSE, # biosample_accession
    FALSE, # sampling_time
    FALSE, # treatment
    FALSE, # synthetic_peptide
    TRUE,  # spiked_compound
    FALSE, # enrichment_process
    FALSE, # instrument
    FALSE, # quantification_unit
    FALSE, # plate
    FALSE, # normalization_method
    FALSE, # dilution
    FALSE  # lot_number
  ),
  # expected R class after reading (character for ontology / free text)
  col_class = c(
    "character", # source_name
    "character", # assay_name
    "character", # technology_type
    "integer",   # technical_replicate
    "character", # data_file
    "character", # organism
    "character", # organism_part
    "character", # biological_replicate (integer as string, or "pooled")
    "character", # sample_type
    "character", # platform
    "character", # sdrf_version
    "character", # cell_type
    "character", # disease
    "character", # panel_name
    "character", # sample_matrix
    "character", # sdrf_template
    "character", # sdrf_annotation_tool
    "character", # sdrf_validation_hash
    "character", # tissue_supergroup
    "character", # pooled_sample
    "character", # material_type
    "character", # tissue_mass
    "character", # biosample_accession
    "character", # sampling_time
    "character", # treatment
    "character", # synthetic_peptide
    "character", # spiked_compound
    "character", # enrichment_process
    "character", # instrument
    "character", # quantification_unit
    "character", # plate
    "character", # normalization_method
    "character", # dilution
    "character"  # lot_number
  ),
  # whether to enforce col_class in checks (draft; can tighten later)
  col_class_check = c(
    FALSE, # source_name
    FALSE, # assay_name
    FALSE, # technology_type
    TRUE,  # technical_replicate
    FALSE, # data_file
    FALSE, # organism
    FALSE, # organism_part
    FALSE, # biological_replicate
    FALSE, # sample_type
    FALSE, # platform
    FALSE, # sdrf_version
    FALSE, # cell_type
    FALSE, # disease
    FALSE, # panel_name
    FALSE, # sample_matrix
    FALSE, # sdrf_template
    FALSE, # sdrf_annotation_tool
    FALSE, # sdrf_validation_hash
    FALSE, # tissue_supergroup
    FALSE, # pooled_sample
    FALSE, # material_type
    FALSE, # tissue_mass
    FALSE, # biosample_accession
    FALSE, # sampling_time
    FALSE, # treatment
    FALSE, # synthetic_peptide
    FALSE, # spiked_compound
    FALSE, # enrichment_process
    FALSE, # instrument
    FALSE, # quantification_unit
    FALSE, # plate
    FALSE, # normalization_method
    FALSE, # dilution
    FALSE  # lot_number
  ),
  # example values from the affinity-proteomics template (documentation aid)
  col_examples = list(
    "source_name" = character(),
    "assay_name" = character(),
    "technology_type" = c(
      "protein expression profiling by antibody array",
      "protein expression profiling by aptamer array"
    ),
    "technical_replicate" = c("1", "2"),
    "data_file" = character(),
    "organism" = c(
      "homo sapiens",
      "mus musculus",
      "rattus norvegicus"
    ),
    "organism_part" = c("liver", "brain", "heart", "blood"),
    "biological_replicate" = c("1", "2", "pooled"),
    "sample_type" = c(
      "study sample",
      "negative control",
      "positive control",
      "calibrator",
      "plate control",
      "quality control sample",
      "buffer control",
      "reference"
    ),
    "platform" = c(
      "Olink Explore HT",
      "Olink Target 96",
      "SomaScan Assay 11K",
      "NULISAseq"
    ),
    "sdrf_version" = c("v1.1.0", "v2.0.0-dev"),
    "cell_type" = c("hepatocyte", "neuron", "fibroblast", "T cell"),
    "disease" = c("normal", "breast cancer", "infection"),
    "panel_name" = c(
      "Target 96 Inflammation",
      "Explore 1536",
      "SomaScan 7K",
      "NULISAseq Inflammation Panel 250"
    ),
    "sample_matrix" = c(
      "serum",
      "plasma",
      "cerebrospinal fluid",
      "urine",
      "saliva"
    ),
    "sdrf_template" = c(
      "NT=affinity-proteomics;VV=v1.1.0",
      "affinity-proteomics v1.1.0"
    ),
    "sdrf_annotation_tool" = c(
      "NT=OlinkAnalyze;VV=v5.1.0",
      "manual curation"
    ),
    "sdrf_validation_hash" = character(),
    "tissue_supergroup" = c("heart", "liver", "brain"),
    "pooled_sample" = c("not pooled", "pooled", "SN=sample1;SN=sample2"),
    "material_type" = c(
      "tissue",
      "cell",
      "cell line",
      "organism part",
      "whole organism",
      "synthetic"
    ),
    "tissue_mass" = c("50 mg", "1 g", "500 ug"),
    "biosample_accession" = c("SAMN12345678", "SAMEA12345678"),
    "sampling_time" = c("0 hour", "24 hour", "7 day"),
    "treatment" = c("untreated", "LPS stimulation"),
    "synthetic_peptide" = c("synthetic", "not synthetic"),
    "spiked_compound" = c("CT=peptide;PS=PEPTIDESEQ;QY=10 fmol"),
    "enrichment_process" = character(),
    "instrument" = c(
      "Illumina NovaSeq X",
      "Alamar ARGO HT System"
    ),
    "quantification_unit" = c("NPX", "RFU", "NPQ"),
    "plate" = c("1", "2"),
    "normalization_method" = c(
      "plate control normalized",
      "bridge normalized",
      "median normalization",
      "not normalized"
    ),
    "dilution" = c("0.005%", "0.5%", "20%", "40%", "1:200"),
    "lot_number" = c("SS-2023-001", "lot_12345")
  )
)

## Template-level metadata (not per-column)
sdrf_affinity_template <- list(
  name = "affinity-proteomics",
  url = "https://sdrf.quantms.org/templates/affinity-proteomics.html",
  inheritance = c("base", "sample-metadata", "affinity-proteomics"),
  mutually_exclusive_with = c("ms-proteomics", "ms-metabolomics"),
  # Olink-relevant defaults for future validation / export helpers
  technology_type_olink = "protein expression profiling by antibody array",
  quantification_unit_olink = "NPX"
)
