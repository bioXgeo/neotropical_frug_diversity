# Project Title

# Project Summary

## Funding
NASA FINESST Grant #80NSSC19K1332

## Collaborators
- Beth E. Gerstner: PhD Candidate, Michigan State University (MSU)
- Phoebe L. Zarnetske: PI, [MSU Spatial & Community Ecology Lab (SpaCE Lab)](http://www.communityecologylab.com)

## Directories

All directories are named for the data level, in accordance with guidelines from the [Environmental Data Initiative](http://www.environmentaldatainitiative.org) where Level 0 (L0) raw data are read in and cleaned, then output as Level-1 (L1) data, which are subsequently evaluated and summarized as Level-2 (L2) data.

## L0

The L0 subfolder contains scripts for Level-0 (raw data) analysis, mainly pulling and compiling data. This contains the following scripts: 

- L0_1_Frugivoria_tropical_andes_subset - generates IUCN-based Frugivoria subsets to the Tropical Andes
- L0_2_range_map_subsetting - subsets range maps from IUCN and BirdLife International for species in the Tropical Andes
- L0_3_imputation_gen_time - imputes missing traits
- L0_4_presence_absence_richness - generates presence absence matrix and generates rasters of taxonomic diversity
- L0_5_FD_mam_foraging - data prep and calculation of functional diversity for mammals
- LO_6_FD_bird_foraging - data prep and calculation of functional diversity for birds

## L1
The L1 subfolder contains scripts for Level-1 analysis, mainly calculating database statistics. Specifically:

- L1_1_Thresholding_diversity_maps - creates thresholded diversity maps for mammals and birds 
- L1_2_joint_species_raster_frug_diversity - creates rasters of Frugivore diversity at different thresholds (birds and mammals joined).
- L1_3_FD_TD_overlap_analysis - calculates spatial overlap between functional and taxonomic diversity maps (for mammals, birds, and combined metrics) and protected areas.
- L1_4_FUSE_species_calculations - processes range maps for FUSE species across birds, mammals, and frugivores
- L1_5_FD_FUSE_overlap_analysis - calculates spatial overlap between functional and taxonomic diversity maps (for mammals, birds, and frugivores) and protected areas
- L1_6_Kruskal_Wallace_calculations - performs Kruskal-Wallis tests and post-hoc Dunn's tests (if applicable) to evaluate the differences in biodiversity metrics (FUSE, FD, TD) and Forest Integrity across park categories.
- L1_7_community_diversity - performs Mann-Whitney U tests to evaluate the differences in biodiversity metrics (FUSE, FD, TD, and forest integrity) across park categories and community managed lands.

## L2
The L2 subfolder contains scripts for Level-2 analysis, mainly visualization of the database. Specifically:

- L2_3D_FD_plots - generates 3D maps to visualize the relationship between elevation and functional diversity (FD)
- L2_FUSE_plots - generates lollipop plot of endemic FUSE species as well as 
- L2_FUSE_species_park_overlay - generates a map of FUSE species richness in the Tropical Andes, including an inset map, and calculates the extent of protected areas in the region based on IUCN categories
- L2_diversity_park_distribution_maps - generates density plots and maps for functional and taxonomic diversity (FD and TD) of mammals and birds within protected areas



*This readme last modified by BEG on 12 2 2024*
