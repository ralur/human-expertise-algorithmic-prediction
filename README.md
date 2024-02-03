# README 

## Getting Started
This supplementary material contains code and data to reproduce the results in our paper "Distinguishing the Indistinguishable: Human Expertise in Algorithmic Prediction" (http://arxiv.org/abs/2402.00793).

The two main entry points are chest_xray.R and visual_prediction.R. These reproduce our experiments related to chest x-ray classification (adapted from [2]) and the escape the room visual prediction task (adapted from [3]), respectively. These experiments are described in section 5 of our paper. Additional detail is provided in Appendix C and Appendix D.

## Details
Each file is self-contained, and assumes the working directory is set to the root of this repository. All experiments can be run in 10-20 minutes on a standard personal laptop; most of the runtime is dedicated to producing bootstrap confidence intervals, and can be improved by reducing the number of bootstrap replicates. The number of replicates is a hyperparameter at the top of both code files. Upon completion, both files produce all the figures in our manuscript and save them to the ./plots/ directory.

The x-ray classification task relies on data (released with [1]) which is available in ./xray-data/. The visual prediction task relies on data (released with [3]) which is available in ./visual-prediction-data/.

The visual prediction task also relies on the predictions of the boosting algorithm proposed in [4]; code to produce these predictions is available in mcboosting.ipynb, and the predictions themselves are stored in mc_predictions.csv. This notebook is adapted from https://github.com/Declancharrison/Level-Set-Boosting.

helpers.R contains helper functions which are called in chest_xray.R and visual_prediction.R. LSboost.py and helper_functions.py contain helper functions for the mcboosting.ipynb notebook.

## Libraries
The R scripts depend on the following libraries:

tidyverse, glue, data.table, cluster, Cairo, boot, mltools

The python notebook depends on the following libraries

pandas, numpy, sklearn

## References

[1] Irvin, J., Rajpurkar, P., Ko, M., Yu, Y., Ciurea-Ilcus, S., Chute, C., ... Ng, A. Y. (2019). CheXpert: A Large Chest Radiograph Dataset with Uncertainty Labels and Expert Comparison.

[2] Rajpurkar, P., Joshi, A., Pareek, A., Ng, A. Y., & Lungren, M. P. (2021). CheXternal: Generalization of Deep Learning Models for Chest X-ray Interpretation to Photos of Chest X-rays and External Clinical Settings.

[3] Saveski, M., Awad, E., Rahwan, I., & Cebrian, M. (2021). Algorithmic and human prediction of success in human collaboration from visual features. Scientific Reports, 11, 2756.

[4] Globus-Harris, I., Harrison, D., Kearns, M., Roth, A., & Sorrell, J. (2023). Multicalibration as Boosting for Regression.


