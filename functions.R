

# Function: Creates STD + BKG rows depending on gc_model, only called in the 
#           createData function. Seperate for clarity.
# ------------------------------------------------------------------------------
tissue_options <- c(
  "Blood_whole", "Plasma", "Serum", "Bone", "Bone_marrow", "Heart", "Lungs", 
  "Brain_whole",  "Liver", "Gallbladder", "Spleen", "Kidneys", "Kidney_left", 
  "Kidney_right", "Bladder", "Bladder_wall", "Urine", "Stomach", "Stomach_wall", 
  "Stomach_contents", "Small_intestines", "Small_intestines_wall", "Small_intestines_contents", 
  "Large_intestines", "Large_intestines_wall", "Large_intestines_contents", "Cecum", 
  "Cecum_wall", "Cecum_contents", "Feces", "Pancreas", "Adrenal_glands", "Lymph_node", 
  "Thymus", "Thyroid", "Salivary_gland", "Ovaries", "Uterus", "Testes", "Muscle", 
  "Skin", "Tumor", "Tail", "Spinal_column", "Periphyseal_bone", "Plasma_SUVR",
  "Blood_pellet", "Cerebrospinal_fluid", "Brain_right_hemisphere", 
  "Brain_left_hemisphere", "Brain_cerebrum", "Brain_cerebellum", "Brain_brainstem", 
  "Brain_hippocampus", "Brain_striatum", "Brain_pons", "Brain_medulla", "Eyes", 
  "Skull_cap", "Spinal_cord", "Spinal_cord_cervical", "Spinal_cord_thoracic", 
  "Spinal_cord_lumbar"
)

createStds <- function(gc_model){
  
  stds <- {
    if (gc_model == "2480") {
      return(data.frame(
        animal_id = c(NA, NA, NA, NA, NA, NA),
        group = c(NA, NA, NA, NA, NA, NA),
        Timepoint = c(NA, NA, NA, NA, NA, NA),
        Tissue = c("Standard-1", "Standard-2", "Standard-3", "BKG", "BKG", "BKG"),
        Rack = c(1, 1, 1, 2, 2, 2),
        Pos = c(1, 2, 3, 1, 2, 3),
        stringsAsFactors = FALSE
      ))
    } else if (gc_model == "2470") {
      return(data.frame(
        animal_id = c(rep(NA, 13)),
        group = c(rep(NA, 13)),
        Timepoint = c(rep(NA, 13)),
        Tissue = c("Standard-1", "Standard-2", "Standard-3", rep("BKG", 10)),
        Rack = c(1, 1, 1, rep(2, 10)),
        Pos = c(1, 2, 3, 1:10),
        stringsAsFactors = FALSE
      ))
    } else if (gc_model == "1480") {
      return(data.frame(
        animal_id = c(NA, NA, NA, NA, NA, NA),
        group = c(NA, NA, NA, NA, NA, NA),
        Timepoint = c(NA, NA, NA, NA, NA, NA),
        Tissue = c("Standard-1", "Standard-2", "Standard-3", "BKG", "BKG", "BKG"),
        Rack = c(1, 1, 1, 2, 2, 2),
        Pos = c(1, 2, 3, 11, 12, 13),
        stringsAsFactors = FALSE
      ))
    }
    return(data.frame())
  }
  
  return(stds)
}
# ------------------------------------------------------------------------------

createStructure <- function(col, rack_type) {
  
  struct <- vector("list", col) 
  
  if(rack_type == "large"){
    maxRow <- 5
  }
  else{
    maxRow <- 10
  }
  
  for (i in seq_along(struct)) {
    struct[[i]] <- vector("list", maxRow) # Each column is a list of 10 rows
  }
  
  return(struct)
}

# Function: Creates a matrix of tissues
createData <- function(tissueLists, gc_model, sortType, group_by, new_rack_per_tissue, new_rack_per_animal) { #data = subject_data, gc_model = gc_model)
  
  # Create standard+BKG rows based on gc_model
  stds <- createStds(gc_model)
  
  # Create smallTissue structure, if it's not empty
  if(!identical(tissueLists$smallTissue, character(0))){
    createStructure(len(tissueLists$smallTissue), tissueType)
  }
  
  # Create largeTissue structure, if it's not empty
  if(!identical(tissueLists$largeTissue, character(0))){
    createStructure(len(tissueLists$largeTissue), tissueType)
  }
  
  
  if (gc_model == "2480" && sortType == "Tissues"){
    
    for (i in data){
      
    }
    
    
  }
  else if (gc_model == "2480" && sortType == "Subject"){
  }
  if (gc_model == "2470" && sortType == "Tissues"){
    
    for (i in data){
      
    }
    
    
  }
  else if (gc_model == "2470" && sortType == "Subject"){
  }
  if (gc_model == "1480" && sortType == "Tissues"){
    
    
    for (i in data){
      
    }
    
    
  }
  else if (gc_model == "1480" && sortType == "Subject"){
  }
  
  return(processed)
}