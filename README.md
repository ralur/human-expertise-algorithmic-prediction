#README

This supplementary material contains code and data to reproduce the results in our paper "Distinguishing the Indistinguishable: Human Expertise in Algorithmic Prediction".

The two main code files are chest_xray.R and visual_prediction.R. These reproduce our experiments related to chest x-ray classification and the escape the room visual prediction task, respectively. Each file is self-contained, and assumes the working directory is set to the root of this repository. All experiments can be run in 10-20 minutes on a standard personal laptop; most of the runtime is dedicated to producing bootstrap confidence intervals, and can be improved by reducing the number of bootstrap replicates. The number of replicates is a hyperparameter at the top of both code files. Upon completion, both files produce all the figures in our manuscript and save them to the ./plots/ directory.

The x-ray classification task relies on data (from \cite{chexternal-2021}) which is available in ./xray-data/. The visual prediction task relies on data (from \cite{human-collaboration-visual-2021}) which is available in ./visual-prediction-data/.

The visual prediction task also relies on the predictions of the boosting algorithm proposed in \cite{multicalibration-boosting-regression-2023}; code to produce these predictions is available in mcboosting.ipynb, and the predictions themselves are stored in mc_predictions.csv. This notebook is adapted from code which was publicly released by \cite{multicalibration-boosting-regression-2023}. 

helpers.R contains helper functions which are called in chest_xray.R and visual_prediction.R. LSboost.py and helper_functions.py contain helper functions for the mcboosting.ipynb notebook.

#Libraries
The R scripts depend on the following libraries:

library(tidyverse)
library(glue)
library(data.table)
library(cluster)
library(Cairo)
library(boot)
library(mltools)

The python notebook depends on the following libraries

import pandas as pd
import numpy as np
import sklearn as sk
import gc
import time
import warnings


#References

@misc{chexpert-dataset-2019,
Author = {Jeremy Irvin and Pranav Rajpurkar and Michael Ko and Yifan Yu and Silviana Ciurea-Ilcus and Chris Chute and Henrik Marklund and Behzad Haghgoo and Robyn Ball and Katie Shpanskaya and Jayne Seekins and David A. Mong and Safwan S. Halabi and Jesse K. Sandberg and Ricky Jones and David B. Larson and Curtis P. Langlotz and Bhavik N. Patel and Matthew P. Lungren and Andrew Y. Ng},
Title = {CheXpert: A Large Chest Radiograph Dataset with Uncertainty Labels and Expert Comparison},
Year = {2019},
Eprint = {arXiv:1901.07031},
}


@article{chexternal-2021,
Author = {Pranav Rajpurkar and Anirudh Joshi and Anuj Pareek and Andrew Y. Ng and Matthew P. Lungren},
Title = {CheXternal: Generalization of Deep Learning Models for Chest X-ray Interpretation to Photos of Chest X-rays and External Clinical Settings},
Year = {2021},
Eprint = {arXiv:2102.08660},
Doi = {10.1145/3450439.3451876},
}

@article{human-collaboration-visual-2021,
  title={Algorithmic and human prediction of success in human collaboration from visual features},
  author={Saveski, M. and Awad, E. and Rahwan, I. and Cebrian, M.},
  journal={Scientific Reports},
  volume={11},
  pages={2756},
  year={2021},
  doi={10.1038/s41598-021-81145-3}
}

@misc{multicalibration-boosting-regression-2023,
      title={Multicalibration as Boosting for Regression}, 
      author={Ira Globus-Harris and Declan Harrison and Michael Kearns and Aaron Roth and Jessica Sorrell},
      year={2023},
      eprint={2301.13767},
      archivePrefix={arXiv},
      primaryClass={cs.LG}
}

