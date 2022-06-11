This folder contains two files to test model reproducibility.

# obfuscate_data.R (do not run)

Is the script used to obfuscate the data (EHRs data below 10 are set to 0). To run this script you need the original dfEDS data.

# 01_train_model_test_deploy.R

This model load the obfuscated data that should be put in "extdata/publication_datasets/dfEDSobfuscated.rds". Then it runs the prediction algorithm for the last date available and gives a dfpred output. This output is then compared to the prediction made in Bordeaux with the same obfuscated dataset. All values should be equal to true.
