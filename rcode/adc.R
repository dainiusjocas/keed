# This method computes Asymetric Dependency Coefficients for the dimensions
#   of a given dataset. 
# DATASET SHOULD BE ALREADY NORMALIZED!!!
# input: dataset: rows - features, columns - patients
# input: normal - indexes of normal patients in the dataset
# input: tumor - indexes of patients with tumor in the dataset
# output: list of Asymetric Dependency Coefficiets dimentions.
get_adc <- function(dataset, normal, tumor)
{
  # I need somehow to universalize this
  total_number_of_items <- length(dataset[ , 1]) 
  class_probability <- c(length(normal) / total_number_of_items,
                         length(tumor)  / total_number_of_items)
  
  # MI(Y, X)
  values_of_mutual_information <- apply(dataset, 
                                     1, # rows - features
                                     get_mutual_information,
                                     indexes_normal=normal,
                                     indexes_tumor=tumor,
                                     probability_normal=class_probability[1],
                                     probability_tumor=class_probability[2])
  # H(Y)
  entropy_of_classes <- entropy(c(length(normal), length(tumor))) 
  # ADC(Y, X) = MI(Y, X) / H(Y)
  adc <- values_of_mutual_information / entropy_of_classes;
  return(adc)
}

# This method computes mutual information of the feature.
# input: values_of_feature - values of a specific feature
# input: indexes_normal - indexes in the list values_of_feature
#   of normal patients
# input: indexes_tumor - indexes in the list values_of_feature
#   of patients with tumor
# input: probability_normal - probability that patient is normal
# input: probability_tumor - probability thet patient has tumor
get_mutual_information <- function(values_of_feature,
                                   indexes_normal,
                                   indexes_tumor,
                                   probability_normal,
                                   probability_tumor)
{
  counts_of_values_of_feature_normal <- 
    hist(values_of_feature[indexes_normal], seq(-6, 6, 0.25))$counts
  # Probability that specific value belongs to class of normal patients
  probability_of_normal <- 
    counts_of_values_of_feature_normal / 
    sum(counts_of_values_of_feature_normal)
  counts_of_values_of_feature_tumor <- 
    hist(values_of_feature[indexes_tumor], seq(-6, 6, 0.25))$counts
  # probability that specific value belongs to class of patients with tumor
  probability_of_tumor <- 
    counts_of_values_of_feature_tumor / 
    sum(counts_of_values_of_feature_tumor)
  joint_probability <- matrix(cbind(probability_of_normal * probability_normal,
                                    probability_of_tumor * probability_tumor),
                              ncol=2)
  mutual_information <- mi.plugin(joint_probability)
  return(mutual_information)
}


