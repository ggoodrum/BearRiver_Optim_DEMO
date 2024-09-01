$Title Generalized hydro-economic-environmental water use optimizaiton model and an application on the Bear River Baisn

$OnText

--------------------------------------------------------------------------
Title:

Authors:

Correspondence:

Citation:

Licensing:

--------------------------------------------------------------------------
Introduction:

Documentation available at:

--------------------------------------------------------------------------
Instructions:

Syntax:
* Model formulation uses CAPITAL LETTERS to indicate decision variables and lower-case letters to describe model sets, parameters, and scalars.
AG   = Agriculture
MI   = Municipal and Industrial
Q    = Streamflow
taf  = Thousand acre-feet
B    = Binary
LOSS = Economic loss
*$M  = Million dollars
init = Initial condition in timestep 1
--------------------------------------------------------------------------

$OffText



**** Model formulation begins here:

*===============
* 1. Declare sets
*===============

* The model divides a river network into nodes and links. Nodes are denoted by the set j and it's alias k.
* The links in the model are denoted by a starting node and an end node. Q(j,k), for instance, desrcibes flow (Q) between nodes j and k.

SETS
* Sets
        j                 Nodes in the river network
        t                 Timestep in year_month [YYYY_MM]
        nodeType          Types of nodes [Junction ExistingReservoir ProposedReservoir Demand_AG Demand_MI]
        coefType          Coefficient type for economic loss function linear segments [slope [m] y-intercept [b].
        coefSeason        Coefficient season for economic loss function linear segments [summer [sum] fall spring [int] winter [win]]
  ;

*Define aliases
Alias (j,k)  ;




*=================================
* 2. Define Parameters and Scalars
*=================================

PARAMETERS
* Parameters - Flow
        t_m(t)                                   Month of timestep (t).
        t_d(t)                                   Days in month of timestep (t).
        nodeExist(j,nodeType)                    Assigns nodeType to nodes (j) [Junction ExistingReservoir ProposedReservoir Demand_AG Demand_MI]
        linkExist(j,k)                           Describes whether link exists between node (j) and node (k) [1=yes 0=no]
        Q_init(j,t)                              External inflows that generate flow in the network. Includes headflow and tributaries.  [taf per month]
        Q_delta(j,k,t)                           Gains or losses on flows between node (j) and node (k) due to unaccounted inflow or diversion in system. [taf]
        Q_max(j,k)                               Maximum allowable flow on link between nodes (j) and (k). [taf per month]
        Q_min(j,k)                               Minimum allowable flow on link between nodes (j) and (k) such as a minimum instream flow. [taf per month]
* Parameters - Reservoir and Proposed Reservoir
        STOR_init(j)                             Initial reservoir storage in timestep (ord(t)=t1). [taf]
        STOR_delta(j,t)                          Reservoir gains or losses at reservoir (j) in timestep (t). [taf]
        STOR_min(j)                              Inactive reservoir storage. [taf]
        STOR_max(j)                              Reservoir storage capacity at full pool. [taf]
* Parameters - Economic Demand
        demandAG(j,t)                            Agricultural demand for each timestep. [taf per month]
        coefMI(j,coefSeason,coefType)            Coefficient values for seasonal MI economic loss function linear segments at node (j).
* Parameters - Habitat
        habitat_Length(j,k)                      Suitable habitat length between nodes (j) and (k). [km per month]
        mad(j,k)                                 Unregulated mean annual discharge on link between nodes (j) and (k). [taf]
        habitatExist(j,k)                        Describes whether habitat exists on link between nodes (j) and (k). [1=yes 0=no]
        Tw_init(j,t)                             Input temperatures at headwaters and reservoirs. [deg C]
        hNet(j,k,t)                              Net head exchange on link (jk) in timestep (t). [W m^-2]
   ;

SCALARS
* Scalars -  Economic demand
        priceAG                                  Price of agricultural water [$M per taf] /0.02/
        priceHP                                  Price of hydropower water [$M per taf] /0.002/
* Scalars - Habitat
        hab_Percent                              Percentage of potential suitable habitat length protected in timestep (t) /0.00/
        thold_Pmad_sum                           Suitability threshold for percent MAD in summer months [%] /0.10/
        thold_Pmad_win                           Suitability threshold for percent MAD in winter months [%] /0.05/
        thold_Tw                                 Suitability threshold for stream temperature [°C] /22.0/
        bigM                                     Big M coefficient for binary conditional variables /1000/
*        c_depth                                  Depth function coefficient /0.4429/
*        e_depth                                  Depth function exponent /0.4327/
*        c_vel                                    Velocity function coefficient /0.09168/
*        e_vel                                    Velocity function exponent /0.55851/
*        cp                                       Specific head of water [J per kg per °C] /4185/
*        rho                                      Water density [kg m^-3] /1000/
* Scalars - Conversion factors
        c_sec                                    Conversion factor between seconds and days (60s * 60m * 24h) /86400/
        c_m3s                                    Conversion factor between ft3s-1 and m3s_1 /0.02832/
        c_acft                                   Conversion factor between acft and m3 /1233/
        c_taf                                    Conversion factor between acft and taf /1000/
*  ;


*===================================================
* 3. Read sets and parameter input
*===================================================

*-----------------
* Load - Flow
*-----------------
$CALL GDXXRW.EXE input=BearRiverModel_INPUT.xlsx output=TEST_Input_Data.gdx  Set=t rng=Timestep_t!A1 Rdim=1  Par=t_m rng=t_Month_m!A1 Rdim=1  Par=t_d rng=t_Days_d!A1 Rdim=1  Set=j rng=Nodes_j!A1 Rdim=1 Set=nodeType rng=NodeType!A1 Rdim=1  Par=nodeExist rng=Node_Exist!A1 Rdim=1 Cdim=1  Par=linkExist rng=Link_Exist!A1 Rdim=1 Cdim=1   Par=Q_init rng=Q_Init_taf!A1 Rdim=1 Cdim=1   Par=Q_delta rng=Q_Delta_taf!A1 Rdim=2 Cdim=1  Par=Q_max rng=Q_Max_taf!A1 Rdim=2  Par=Q_min rng=Q_Min_taf!A1 Rdim=2
$GDXIN TEST_Input_Data.gdx
$LOAD t
$LOAD t_m
$LOAD t_d
$LOAD j
$LOAD nodeType
$LOAD nodeExist
$LOAD linkExist
$LOAD Q_init
$LOAD Q_delta
$LOAD Q_max
$LOAD Q_min
$GDXIN

*-----------------
* Load - Reservoir
*-----------------
$CALL GDXXRW.EXE input=BearRiverModel_INPUT.xlsx output=TEST_Input_Data.gdx  Par=STOR_init rng=STOR_Init_taf!A1 Rdim=1  Par=STOR_delta rng=STOR_Delta_taf!A1 Rdim=1 Cdim=1  Par= STOR_min rng=STOR_Min_taf!A1 Rdim=1  Par= STOR_max rng=STOR_Max_taf!A1 Rdim=1
$GDXIN TEST_Input_Data.gdx
$LOAD STOR_init
$LOAD STOR_delta
$LOAD STOR_min
$LOAD STOR_max
$GDXIN

*-----------------
* Load - Economic demand
*-----------------
$CALL GDXXRW.EXE input=BearRiverModel_INPUT.xlsx output=TEST_Input_Data.gdx   Par=demandAG rng=Demand_AG_taf!A1 Rdim=1 Cdim=1  Set=coefType rng=MI_Coefficient_Type!A1 rdim=1  Set=coefSeason rng=MI_Coefficient_Season!A1 rdim=1  Par=coefMI rng=MI_Coefficients!A1 rdim=2 cdim=1
$GDXIN TEST_Input_Data.gdx
$LOAD demandAG
$LOAD coefType
$LOAD coefSeason
$LOAD coefMI
$GDXIN

*-----------------
* Load - Habitat
*-----------------
$CALL GDXXRW.EXE input=BearRiverModel_INPUT.xlsx output=TEST_Input_Data.gdx  Par=habitat_Length rng=HS_Stream_Length_km!A1 Rdim=2  Par=mad rng=HS_MAD_taf!A1 Rdim=2  Par=habitatExist rng=Habitat_Exist!A1 Rdim=1 Cdim=1  Par=Tw_init rng=Tw_Init_degC!A1 Rdim=1 Cdim=1  Par=hNet rng=hNet_Wm2!A1 Rdim=2 Cdim=1
$GDXIN TEST_Input_Data.gdx
$LOAD habitat_Length
$LOAD mad
$LOAD habitatExist
$LOAD Tw_init
$LOAD hNet
$GDXIN




*===================
* 4. Define Variables
*===================

VARIABLES
* Variables - Flow
        Z                        Objective function value. [$M]
        Q(j,k,t)                 Flow in links in timestep (t). [taf per month]
        Q_cms(j,k,t)             Flow in links in timestep (t) converted to instantateous flow rate. [m^3 s^-1]
* Variables - Reservoirs
        STOR(j,t)                Storage in reservoirs (res) in timestep (t). [taf per month].
        Bpr(j)                   Binary decision to build proposed reservoirs. [1=yes 0=no]
* Variables - Economic demand
        LOSS_ag(j,t)             Economic loss at agricultural demand sites ["Demand_AG"]. [$M per month]
        LOSS_mi(j,t)             Economic loss at urband demand sites ["Demand_MI"]. [$M per month]
        LOSS_hp(j,t)             Economic loss at hydropower reservoirs ["Demand_HP"]. [$M per month]
        LOSS_tot(t)              Total economic loss across all AG and MI demand sites. [$M per month].
* Variables - Habitat
        Hab(j,k,t)               Suitable habitat length available between nodes (j) and (k) in timestep (t). [km]
        Hab_month(t)             Monthly total suitable habitat in timestep (t). [km]
        Pmad(j,k,t)              Percentage of unregulated mean annual discharge for flow between nodes (j) and (k) in timestep (t). [%]
        Bhs_Q(j,k,t)             Binary habitat suitability indicator for percent MAD. [1=suitable 0=unsuitable]
*        Tw(j,k,t)                Stream temperature between nodes (j) and (k) in timestep (t). [°C]
*        Bhs_Tw(j,k,t)            Binary habitat suitability indicator for stream temperature. [1=suitable 0=unsuitable]

NONNEGATIVE VARIABLES
* Variables - Economic demand
        LOSS_ag
        LOSS_mi
        LOSS_hp

BINARY VARIABLES
* Variables - Reservoirs
        Bpr
* Variables - habitat
        Bhs_Q
*        Bhs_Tw
   ;

*-----------------
* Variable Bounds
*-----------------
* Upper and lower bounds on links (i.e. stream reaches)
Q.UP(j,k,t)$linkExist(j,k) = Q_max(j,k)$linkExist(j,k)  ;
Q.LO(j,k,t)$linkExist(j,k) = Q_min(j,k)$linkExist(j,k)  ;

*-----------------
* Variable Bounds
*-----------------
* Set starting level for Q that is not zero.
Q.L(j,k,t)$linkExist(j,k) = 1  ;




*============================
* 6. Declare model equations
*============================

EQUATIONS
*-------------------
* Objective Functions
*-------------------
        EQ1                Z  Objective function to minimize water scarcity costs. [$M]
        EQ2(t)             Habitat dual objective to maintain a percentage of naturally suitable habitat in a given timestep. [km]

*-------------------
* Model Constraints - Flow
*-------------------
        EQ3a(j,t)          Streamflow mass balance equations at each nodes.
        EQ3b(j,k,t)        Streamflow conversion from [taf m^-1] to [m^3 s^-1] along links.

*-------------------
* Model Constraints - Reservoir
*-------------------
        EQ4a(j,t)          Reservoir mass balance at timestep 1.
        EQ4b(j,t)          Reservoir mass balance at all subsequent timesteps.
        EQ4c(j,t)          Existing reservoir storage in last timestep must equal initial storage.
        EQ4d(j,t)          Minimum reservoir storage capacity.
        EQ4e(j,t)          Maximum reservoir storage capacity.

*-------------------
* Model Constraints - New reservoir construction
*-------------------
        EQ5a(j)            All binary variables lower limits equal to zero.
        EQ5b(j)            Existing reservoir binary variable must equal 1 (must be constructed).
        EQ5c(j)            Proposed reservoir binary variable upper limit equal to 1. Setting to 0 removes option to construct proposed reservoirs.

*-------------------
* Model Constraints - Economic demand
*-------------------
        EQ6a(j,t)          Economic loss at agricultural demand sites. [$M per month]

        EQ7a_sum(j,t)      Economic loss at MI demand sites ["Demand_MI"] in summer months [June-August]. [$M per month]
        EQ7a_int(j,t)      Economic loss at MI demand sites ["Demand_MI"] in fall or spring months [March-May or September-November]. [$M per month]
        EQ7a_win(j,t)      Economic loss at MI demand sites ["Demand_MI"] in winter months [December-February]. [$M per month]

        EQ8a(j,t)          Economic loss at hydropower reservoirs ["Demand_HP"]. [$M per month]

        EQ9a(t)            Total economic loss across all demand sites. [$M per month]

*-------------------
* Model Constraints - Habitat
*-------------------
        EQ10a(j,k,t)       Suitable habitat available between nodes (j) and (k) in timestep (t). [km]
        EQ10b(t)           Monthly total suitable habitat in timestep (t). [km]

        EQ11a(j,k,t)        Percentage of mean annual discharge between nodes (j) and (k) in timestep (t). [%]

        EQ11b_sum(j,k,t)    Binary habitat suitability indicator for summer discharge (Qhs) must be 0 when Pmad <= 0.1  [summer = April-September]
        EQ11b_win(j,k,t)    Binary habitat suitability indicator for winter discharge (Qhs) must be 0 when Pmad <= 0.05 [winter = October-March]

        EQ11c_sum(j,k,t)    Binary habitat suitability indicator for summer discharge (Qhs) must be 1 when Pmad > 0.1   [summer = April-September]
        EQ11c_win(j,k,t)    Binary habitat suitability indicator for winter discharge (Qhs) must be 1 when Pmad > 0.05  [winter = October-March]

*        EQ12a(j,t)        Stream temperature mixing at juntions. [°C]
*        EQ12b(j,t)        Stream temperature mixing at reservoirs and proposed reservoirs. [°C]
*        EQ12c(j,k,t)      Binary habitat suitability indicator for stream temperature (Bhs_Tw) must be 1 when Tw <= threshold.
*        EQ12d(j,k,t)      Binary habitat suitability indicator for stream temperature (Bhs_Tw) must be 0 when Tw >= threshold.

   ;





*============================
* 7. Define Model Equations
*============================
*-------------------
* Objective Functions
*-------------------

* Water scarcity objective
        EQ1..    Z =E= sum(t, LOSS_tot(t)) ;

* Habitat objective
        EQ2(t)..  sum((j,k)$habitatExist(j,k), Hab(j,k,t)) =G= (sum((j,k)$habitatExist(j,k), habitat_Length(j,k))) * hab_Percent ;

*-------------------
* Model Constraints - Flow
*-------------------
        EQ3a(j,t)$nodeExist(j,'Junction')..    sum(k$linkExist(j,k), Q(j,k,t)) =E= Q_init(j,t) + sum(k$linkExist(k,j), (Q(k,j,t) + Q_delta(k,j,t)))  ;
        EQ3b(j,k,t)$linkExist(j,k)..           Q_cms(j,k,t) =E= (Q(j,k,t) * c_taf * c_acft) / (t_d(t) * c_sec)  ;

*-------------------
* Model Constraints - Reservoirs
*-------------------
        EQ4a(j,t)$((ord(t) eq 1) AND (nodeExist(j,"ExistingReservoir") OR (nodeExist(j,"ProposedReservoir"))))..    STOR(j,t) =E= STOR_init(j) + STOR_delta(j,t) + sum(k$linkExist(k,j), (Q(k,j,t) + Q_delta(k,j,t))) - sum(k$linkExist(j,k), Q(j,k,t))  ;
        EQ4b(j,t)$((ord(t) ge 2) AND (nodeExist(j,"ExistingReservoir") OR (nodeExist(j,"ProposedReservoir"))))..    STOR(j,t) =E= STOR(j,t-1) + STOR_delta(j,t) + sum(k$linkExist(k,j), (Q(k,j,t) + Q_delta(k,j,t))) - sum(k$linkExist(j,k), Q(j,k,t))  ;
        EQ4c(j,t)$((ord(t) eq card(t)) AND (nodeExist(j,"ExistingReservoir")))..                                    STOR(j,t) =E= STOR_init(j)  ;
        EQ4d(j,t)$(nodeExist(j,"ExistingReservoir") OR nodeExist(j,"ProposedReservoir"))..                          STOR(j,t) =G= STOR_min(j) * Bpr(j)  ;
        EQ4e(j,t)$(nodeExist(j,"ExistingReservoir") OR nodeExist(j,"ProposedReservoir"))..                          STOR(j,t) =L= STOR_max(j) * Bpr(j)  ;

*-------------------
* Model Constraints - New reservoir construction
*-------------------
        EQ5a(j)$((nodeExist(j,"ExistingReservoir")) OR (nodeExist(j,"ProposedReservoir")))..    Bpr(j) =G= 0  ;
        EQ5b(j)$nodeExist(j,"ExistingReservoir")..                                              Bpr(j) =G= 1  ;
*        EQ5c(j)$nodeExist(j,"ProposedReservoir")..                                              Bpr(j) =L= 1  ;
        EQ5c(j)$nodeExist(j,"ProposedReservoir")..                                              Bpr(j) =L= 0  ;

*-------------------
* Model Constraints - Economic demand
*-------------------
        EQ6a(j,t)$nodeExist(j,"Demand_AG")..                                                                                           LOSS_ag(j,t) =E= (demandAG(j,t) - sum(k$linkExist(k,j), Q(k,j,t))) * priceAG  ;

        EQ7a_sum(j,t)$((nodeExist(j,"Demand_MI")) AND ((t_m(t) ge 7) AND (t_m(t) le 9)))..                                             LOSS_mi(j,t) =E= (coefMI(j,'sum','m') * sum(k$linkExist(k,j), Q(k,j,t))) + coefMI(j,'sum','b')  ;
        EQ7a_int(j,t)$((nodeExist(j,"Demand_MI")) AND (((t_m(t) ge 3) AND (t_m(t) le 6)) OR ((t_m(t) ge 10) AND (t_m(t) le 11))))..    LOSS_mi(j,t) =E= (coefMI(j,'int','m') * sum(k$linkExist(k,j), Q(k,j,t))) + coefMI(j,'int','b')  ;
        EQ7a_win(j,t)$((nodeExist(j,"Demand_MI")) AND ((t_m(t) le 2) OR (t_m(t) ge 12)))..                                             LOSS_mi(j,t) =E= (coefMI(j,'win','m') * sum(k$linkExist(k,j), Q(k,j,t))) + coefMI(j,'win','b')  ;

        EQ8a(j,t)$nodeExist(j,"Demand_HP")..                                                                                           LOSS_hp(j,t) =E= (STOR_max(j) - STOR(j,t)) * priceHP  ;

        EQ9a(t)..                                                                                                                      LOSS_tot(t) =E= sum(j$nodeExist(j,"Demand_AG"), LOSS_ag(j,t)) + sum(j$nodeExist(j,"Demand_MI"), LOSS_mi(j,t)) + sum(j$nodeExist(j,"Demand_HP"), LOSS_hp(j,t)) ;

*-------------------
* Model Constraints - Habitat
*-------------------
        EQ10a(j,k,t)$habitatExist(j,k)..                                                   Hab(j,k,t) =E= habitat_Length(j,k) * Bhs_Q(j,k,t)  ;
        EQ10b(t)..                                                                         Hab_month(t) =E= sum((j,k)$habitatExist(j,k), Hab(j,k,t))  ;

        EQ11a(j,k,t)$habitatExist(j,k)..                                                   Pmad(j,k,t) =E= Q(j,k,t) / mad(j,k)  ;

        EQ11b_sum(j,k,t)$((habitatExist(j,k)) AND ((t_m(t) ge 4) AND (t_m(t) le 9)))..     Pmad(j,k,t) =G= Bhs_Q(j,k,t) * thold_Pmad_sum  ;
        EQ11b_win(j,k,t)$((habitatExist(j,k)) AND ((t_m(t) le 3) OR (t_m(t) ge 10)))..     Pmad(j,k,t) =G= Bhs_Q(j,k,t) * thold_Pmad_win  ;

        EQ11c_sum(j,k,t)$((habitatExist(j,k)) AND ((t_m(t) ge 4) AND (t_m(t) le 9)))..     Pmad(j,k,t) - thold_Pmad_sum =L= bigM * Bhs_Q(j,k,t)  ;
        EQ11c_win(j,k,t)$((habitatExist(j,k)) AND ((t_m(t) le 3) OR (t_m(t) ge 10)))..     Pmad(j,k,t) - thold_Pmad_win =L= bigM * Bhs_Q(j,k,t)  ;

*        EQ12a(j,t)$((nodeExist(j,'Junction')) AND ((t_m(t) ge 6) AND (t_m(t) le 9)))..     sum(k$habitatExist(j,k), Tw(j,k,t)) =E= Tw_init(j,t) + ((sum(k$linkExist(k,j), (Tw(k,j,t)) * (Q(k,j,t) + Q_delta(k,j,t)))) / (Q_init(j,t) + sum(k$linkExist(k,j), (Q(k,j,t) + Q_delta(k,j,t)))))  ;

*        EQ12b(j,t)$(((nodeExist(j,'ExistingReservoir') OR (nodeExist(j,'ProposedReservoir')))) AND ((t_m(t) ge 6) AND (t_m(t) le 9)))..  sum(k$habitatExist(j,k), Tw(j,k,t)) =E= (Tw_init(j,t) * Bpr(j)) + ((abs(1 - Bpr(j))) * ((sum(k$linkExist(k,j), (Tw(k,j,t)) * (Q(k,j,t) + Q_delta(k,j,t)))) / (Q_init(j,t) + sum(k$linkExist(k,j), (Q(k,j,t) + Q_delta(k,j,t))))))  ;

*        EQ12c(j,k,t)$((habitatExist(j,k)) AND ((t_m(t) ge 6) AND (t_m(t) le 9)))..         Tw(j,k,t) =G= (1 - Bhs_Tw(j,k,t)) * thold_Tw  ;
*        EQ12d(j,k,t)$((habitatExist(j,k)) AND ((t_m(t) ge 6) AND (t_m(t) le 9)))..         Tw(j,k,t) - thold_Tw =L= (1 - Bhs_Tw(j,k,t)) * bigM  ;

*=======================================================================
* 8. Solve model
*=======================================================================

*-------------------
* Define the model, define the solver, and solve all equations listed above
*-------------------
   Model Test_Model /all/  ;
*  Set LINDO as global solver option
   option MINLP = lindo  ;
*  Activate optfile to control Lindo options
   Test_Model.optfile = 1  ;
*  Set termination tolerance to 0. GAMS default is 0.1, which allows the solver to terminate before globally optimal solution is found (https://forum.gamsworld.org/viewtopic.php?t=8502).
*   option optcr = 0.0001, decimals = 8  ;
   option optcr = 0, decimals = 8, savepoint = 1 ;

*-------------------
* Solve Model
*-------------------
* Provide a starting point from a previous model solution
*   execute_loadpoint 'BearRiverModel_p'  ;
   Solve Test_Model minimize Z using MINLP  ;

*-------------------
* Save and export model outputs
*-------------------
* Export all input data and results to a GAMS gdx file
  Execute_Unload "BearRiverModel_OutputData.gdx"  ;
* Dump all the input data and results to an Excel file
  Execute "gdx2xls BearRiverModel_OutputData.gdx"  ;

* Click File menu => RUN (F9) or Solve icon and examine solution report in .LST file

************************************************************************************************
*End of Code
************************************************************************************************
