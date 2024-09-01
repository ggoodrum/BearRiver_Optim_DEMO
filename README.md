Title: BearRiver_Optim_Demo

Contact: greg.goodrum@usu.edu

Purpose: Teaching repository for Bear River Optimization model to be used in shared with CEE 5410/6410 (Water Resources Systems Analysis) students as a teaching tool and is not intended for publication or dispersal beyond the scope of the course.

Description of repository directories are as follows:

BearRiverModel.gpr - Project file for GAMS model equivalent to a working directory.

BearRiverModel.gms - Optimization model for the Bear River. Notes contain explanation of model and input data.

BearRiverModel_INPUT.xlsx - Excel spreadsheet of model inputs. Model inputs are described in the GAMS code (BearRiverModel.gms).

lindo.txt - A text file requiring GAMS to search for the irreducibly inconsistent set (IIS) causing model failure.

NOTE:
This model is currently coded to run with a LINDO solver. For alternative mixed integer nonlinear solvers (MINLP) please consult the GAMS Documentation:  https://www.gams.com/latest/docs/S_MAIN.html.

Validation Scenario:
To run the validation scenario, ensure the following conditions are set:
- BearRiverModel_INPUT.xlsx/MI_Coefficients: Set all 'b' coefficients to 0. This removes projected municipal & industrial demand from the model.
- BearRiverModel.gms/Line 99: Set 'hab_Percent' to /0.00/. This removes any percentage habitat protections reflecting current conditions.
- BearRiverModel.gms/Line 336: Comment out this line with an asterisk '*' and activate Line 337. This prevents the model from adding proposed reservoirs.