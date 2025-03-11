# Endogenous technology change in GCAM via `gcamwrapper`
This repository contains code to set up and run endogenous technological change experiments in GCAM using the `gcamwrapper` package. See `ETC_approach_overview.docx` for background on the approach and information on data sources. 

## Inputs
The raw inputs to the workflow can be found in the `input` folder. These include:
* `learning_rates.csv`: the assumed learning rate for each learning component.
* `t0_cost_deployment.csv`: the assumed cumulative deployment and capital cost of each learning component in the base year. A value of 1 for the cumulative deployment indicates that the learning component has not yet been deployed in the base year and the capital cost is assumed to be the first-of-a-kind cost.
* `tech_FCR`: fixed charge rates (FCR) for each GCAM technology included in the analysis. These are taken from GCAM inputs, and are used to convert between electricity generation and capacity throughout the workflow.
* `mappings`: this folder contains several mapping files:
  * `cooling_tech_map.csv`: mappings between GCAM pass-through technologies and cooling technology- level technologies. Used to aggregate generation for different cooling technologies.
  * `learning_components_deployment_map`: mappings from GCAM technologies to the learning components included in the technologies, used to calculate the cumulative deployment of learning components based on cumulative deployment of GCAM technologies. The `multiplier` column indicates the relationship between technology deployment and learning component deployment. This is usually `1`, but is `2` for the `grid_storage` component due to the assumption in GCAM that VRE technologies with storage must have a 2 to 1 battery to capacity ratio. Thus, for each unit of VRE capacity, there is twice the storage deployed.
  * `learning_components_learning_map.csv`: mappings from learning components back to GCAM technologies, used to aggregate the endogenously calculated costs from the learning components back to GCAM technologies.

## Calibration
Each time one of the inputs is updated, `learning_curve_calculations.R` must be run to re-calibrate capital costs before running an experiment with `gcamwrapper`. This requires a database from the run that you intend to calibrate capital costs to (i.e., the reference scenario). The script uses `rgcam` to query cumulative capacity data from the reference scenario, calculates the endogenous capital costs that would be produced using the inputs (learning rates, initial cost and deployment, and mappings), and derives calibration adders from the difference between the scenario's default exogenous capital costs and these endogenous costs. These adders are written to `params/cal_adders.csv` and are used in the `gcamwrapper` workflow to adjust endogenous capital costs. 

`learning_curve_calculations.R` also produces `params/non_learning_capital.csv`, which contains the capital costs of components of GCAM technology components that are not currently included in the learning components. This is intended to be temporary, as all components of the GCAM technologies will eventually be included in the learning components. Note that the code to produce this file would need to be manually edited if the non-learning capital assumptions change.

## Running `gcamwrapper`
To run an experiment with `gcamwrapper`, execute `climate_macro_gcamwrap_local.R`. This script reads in all inputs and calibration adders, and runs a given GCAM scenario using `gcamwrapper`, endogenously updating technology costs after each future model period according to the learning curve workflow. It automatically loops through all periods and writes the resulting database to a specified location; however, note that writing the database may not work locally, depending on the user's `gcamwrapper` setup. It is still possible to run through the scenario period-by-period and examine outputs manually for testing (by running individual lines of code rather than running the whole loop). The script can then be executed on PIC to produce the database.

## Analyzing outputs
Once a user has run the `gcamwrapper` worklow, they can query the databases and analyse results as usual. `etc_results.R` contains some code to analyze relevant outputs such as technology capital costs, deployment, generation, and carbon prices.

