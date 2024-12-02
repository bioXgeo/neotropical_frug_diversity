## Project Title
Evaluating the effectiveness of protected areas and community-managed lands in capturing multiple dimensions of frugivorous biodiversity in the Tropical Andes

## Project Summary
The Tropical Andes is known for its remarkable biodiversity; however, this region faces significant anthropogenic pressures such as habitat loss and climate change, making it a conservation priority. Conservation efforts often concentrate on protecting areas with high taxonomic diversity (TD), overlooking the essential roles played by species that contribute to ecosystem functioning– functional diversity (FD). To ensure comprehensive conservation priorities, a broader perspective considering various dimensions of diversity is necessary. Further, there needs to be a better understanding of how community-managed lands contribute to the protection of biodiversity as they are essential for reaching spatial conservation targets. Here, We utilized the Frugivoria trait database to evaluate the functional diversity of frugivorous birds and mammals, which perform the vital role of seed dispersal, an essential ecosystem service necessary to maintain forest structure and ecosystem health. Across the Tropical Andes region, We quantified the spatial alignment and mismatch between taxonomic and functional diversity for areas with the highest diversity values in the region. Our findings revealed many spatial misalignments between the highest levels of FD and TD, emphasizing the limitations of relying solely on TD for conservation and the potential biodiversity trade-offs of doing so. Nevertheless, some alignment between taxonomic and functional diversity emerged for mammals and birds, identifying potential areas for multidimensional biodiversity conservation. Further, we found that some stricter protected areas (PAs) encapsulate different diversity dimensions better than less-strict areas and this differed among taxa. Similarly, we examined the distribution of Functionally Unique, Specialized, and Endangered (FUSE) species and their protection status across strict and less strict PAs. Some stricter protected zones had better coverage of FUSE species distributions, though many areas of high FUSE species richness remain unprotected. we also found that community-managed lands had higher levels of FD than other protected areas within the same protected area category. This has strong implications for the utility of other effective conservation measures (OECMs) such as Indigenous Lands and community-managed areas in protecting areas of high functional diversity. Our results highlight the need for a more holistic conservation approach that considers multiple dimensions of diversity in the context of varying degrees of protection and PA management. As global conservation goals target the protection of 30% of the Earth's land by 2030, our study underscores the importance of considering multiple dimensions of diversity and emphasizes the potential of OECMs in ensuring effective and sustainable conservation strategies within the Tropical Andes.

## Funding
NASA FINESST Grant #80NSSC19K1332

NSF CC* Compute #2200792

Michigan State University

## Collaborators
- Beth E. Gerstner, Ph.D (Michigan State University). Current Postion: Staff scientist, Yale University
- Phoebe L. Zarnetske, Ph.D.: PI, [Michigan State University Spatial & Community Ecology Lab (SpaCE Lab)](http://www.communityecologylab.com)
- Pat Bills, MSU Data Scientist

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



*This readme last modified by BEG and PLZ on 12 2 2024*
