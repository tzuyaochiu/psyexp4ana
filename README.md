# psyexp4ana
### Analysis code for Methods of Psychological Experiment course under R environment.
### Author: Tzu-Yao Chiu, National Chengchi University
### Last Update: 2020.09.21

## Introduction
This is an automatic analyzing code for simple factorial experiments.
With proper set up, the code can generate merged data, statistical tests (ANOVA), and data visualization.

## Settings: 
1. Requirement: ggplot, dplyr, memisc package

2. Data storage: 
The script must be stored in a folder named "data", with each group named as "expname_Gx.csv".
(Only for simplicity, feel free to change loading scripts to customize your data storage.)

3. Parameters: 
There are several parameters need to be setted for the script to execute successfully.
These include experiment name (exp_name), variable names (within_var & between_var), variable labels (relabel section), and plot titles (x/y_title).

4. Output
If properly setup, running the script generates a folder named Routput.
Within you will find ANOVA results, plots, and descriptive statistics (Mean, SE) by condition.

(Feel free to contact me at 105702012@nccu.edu.tw for any bugs or suggestions.)
