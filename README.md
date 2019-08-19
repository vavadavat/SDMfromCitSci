# SDMfromCitSci
Partial code and associated data files for publication

TASK
1. Assess model performance for 4 benchmark datasets (Askins, BBS, Klingbeil, and SHARP) using cross-validated (assess model performance on data withheld from model building) AUC results.
  * Askins.Models.CV.R: Runs cross-validation on Askins data.
  
  * BBS.Models.CV.R: Runs cross-validation on BBS data.
  
  * Klingbeil.Models.CV.R: Runs cross-validation on Klingbeil data.
  
  * SHARP.Models.CV.R: Runs cross-validation on SHARP data. 

2. Assess model performance for eBird models using the "full" (="all") eBird dataset, "culled" eBird data subsets based on stringent filtering, and "random" eBird data subsets. Model performance is assessed based on predictions to benchmark data (Askins, BBS, Klingbeil, and SHARP). Files with "All" in the name call function Models_RF_function.All.R; files with "Culled" or "Random" in the name call function Models_RF_functions.Subsets.R.
  * Model_RF_call.function.Askins.All.R: Runs eBird species distribution models using ‘all’ data, tests on Askins data, and returns model performance metrics including AUC (ROC).
  * Model_RF_call.function.BBS.All.R: Runs eBird species distribution models using ‘all’ data, tests on BBS data, and returns model performance metrics including AUC (ROC).
  * Model_RF_call.function.Klingbeil.All.R: Runs eBird species distribution models using ‘all’ data, tests on Klingbeil data, and returns model performance metrics including AUC (ROC).
  * Model_RF_call.function.SHARP.All.R: Runs eBird species distribution models using ‘all’ data, tests on SHARP data, and returns model performance metrics including AUC (ROC).  
  * Model_RF_call.function.Askins.CulledSubsets.R: Runs eBird species distribution models using 16 ‘culled’ datasets, tests on Askins data, and returns model performance metrics including AUC (ROC). 
  * Model_RF_call.function.BBS.CulledSubsets.R: Runs eBird species distribution models using 16 ‘culled’ datasets, tests on BBS data, and returns model performance metrics including AUC (ROC). 
  * Model_RF_call.function.Klingbeil.CulledSubsets.R: Runs eBird species distribution models using 16 ‘culled’ datasets, tests on Klingbeil data, and returns model performance metrics including AUC (ROC). 
  * Model_RF_call.function.SHARP.CulledSubsets.R: Runs eBird species distribution models using 16 ‘culled’ datasets, tests on SHARP data, and returns model performance metrics including AUC (ROC). 
  * Model_RF_call.function.Askins.RandomSubsets.R: Runs eBird species distribution models using 10 reps each of 16 random subsets of data, tests on Askins data, and returns model performance metrics including AUC (ROC). 
  * Model_RF_call.function.BBS.RandomSubsets.R: Runs eBird species distribution models using 10 reps each of 16 random subsets of data, tests on BBS data, and returns model performance metrics including AUC (ROC). 
  * Model_RF_call.function.Klingbeil.RandomSubsets.R: Runs eBird species distribution models using 10 reps each of 16 random subsets of data, tests on Klingbeil data, and returns model performance metrics including AUC (ROC). 
  * Model_RF_call.function.SHARP.RandomSubsets.R: Runs eBird species distribution models using 10 reps each of 16 random subsets of data, tests on SHARP data, and returns model performance metrics including AUC (ROC). 

  * Models_RF_function.All.R: Function called to run Random Forest models by the ‘…All.R’ scripts.
  * Models_RF_functions.Subsets.R: Function called to run Random Forest models by the ‘Subsets.R’ scripts.

3. Run linear mixed effects models on the difference in AUC results between CV benchmark AUC (1) and eBird AUC (2). The code reads in previously saved AUC results from steps 1 and 2.  
  * LMM_call.function.R: Runs linear mixed effects models by calling function AIC.Lmm.Fcn.R), shows estimates for top (AIC) models.
  
  * AIC.Lmm.Fcn.R: Function called by LMM_call.function.R. Runs linear mixed effects models and returns AIC results for model comparison.



