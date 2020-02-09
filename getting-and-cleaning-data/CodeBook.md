
Steps of tidying up the data that was done -

1.Collecting all the data that is required -
  train_X and test_X are the predicted variables
  train_Y and test_Y are the predictor variables 
  Total of 30 subjects were there in creating and helping in the dataset.
  
2.Here are the activity labels for the inference :

  1            WALKING
  2   WALKING_UPSTAIRS
  3 WALKING_DOWNSTAIRS
  4            SITTING
  5           STANDING
  6             LAYING

3.Activity labels were not mapped currently because when we do predictive analytics , the predictor labels only needed in the analysis. 
Therefore for the data set I have not currently labeled them.

4.Column Names were mapped to the data frame and all variable descriptions were already given :
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

5.Descriptive column names 
 - I converted "-" to "_" , since it is a common nomenclature 
 - "()" were also removed
 - Rest I left it as it is pretty descriptive since it evaluates based on the all the axes.
 
 6.File was saved to cleaned_data.txt




Reference : Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. A Public Domain Dataset for Human Activity Recognition Using Smartphones. 21th European Symposium on Artificial Neural Networks, Computational Intelligence and Machine Learning, ESANN 2013. Bruges, Belgium 24-26 April 2013.
