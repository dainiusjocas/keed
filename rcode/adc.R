# This method computes Asymetric Dependency Coefficients for the dimensions
#   of dataset
# input: dataset: rows - features, columns - patients
# input: normal - indexes of normal patients in the dataset
# input: tumor - indexes of patients with tumor in the dataset
# output: indexes of ranked dimentions.
get_adc <- function(dataset, normal, tumor)
{
  # I need somehow to universalize this
  total_number_of_items <- length(dataset[ , 1]) #length(normal) + length(tumor) 
  class_probability <- c(length(normal) / total_number_of_items,
                         length(tumor)  / total_number_of_items)
  normalized_values <- dataset / max(dataset)
  # MI(Y, X)
  values_of_mutual_information <- apply(normalized_values, 
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
    hist(values_of_feature[indexes_normal], seq(0, 1, 0.05))$counts
  probability_of_normal <- counts_of_values_of_feature_normal / 
    sum(counts_of_values_of_feature_normal)
  counts_of_values_of_feature_tumor <- 
    hist(values_of_feature[indexes_tumor], seq(0, 1, 0.05))$counts
  probability_of_tumor <- counts_of_values_of_feature_tumor / 
    sum(counts_of_values_of_feature_tumor)
  joint_probability <- matrix(cbind(probability_of_normal * probability_normal,
                                    probability_of_tumor * probability_tumor),
                              ncol=2)
  mutual_information <- mi.plugin(joint_probability)
  return(mutual_information)
}

# This method computes entropy of classes
# input: classes_and_sizes - data frame of two columns where first column is
#   the name of the class, second - number entities in that class 
# output: value of entropy of the class
get_entropy_of_classes <- function(classes_and_sizes)
{
  total_number_of_entities <- sum(classes_and_sizes[ , 2])
  probabilities_of_classes <- classes_and_sizes[ , 2] / total_number_of_entities
  classes_with_probabilities <- data.frame(
      class_name = classes_and_sizes[1],
      probabilities = probabilities_of_classes)
  entropy_of_classes <- get_entropy(classes_with_probabilities)
  return(entropy_of_classes)
}

# This method computes entropy from the set pof probabilities.
# input: set_of_probabilities - a data frame of two columns where first 
#   column is a name of event, second column is a probability of a event
# input: base - logarithmic base, by default base = 2
# output: value of entropy of a set of probabilities
get_entropy <- function(set_of_probabilities, base = 2) 
{
  entropy <- -sum(set_of_probabilities[, 2] * 
    log(set_of_probabilities[, 2], base))
  return(entropy)
}
