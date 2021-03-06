## GENERAL NOTES

# From FLEXNET project notes

In the FLEXNET project, flexibility is defined briefly as
> the ability of the energy system to respond to the variability and uncertainty of the residual power load within the limits of the electricity grid

Major characteristics of this definition are:
- The problem (i.e. the demand for flexibility) is caused primarily by the power system;
- The solution (i.e. the supply of flexibility) may come from the energy system as a whole;
- The focus is on changes in residual power load, i.e. total power load minus power production from variable renewable energy (VRE), notably from sun and wind.

three main sources (‘causes’) of the need for flexibility of the power sector:

1. The demand for flexibility  due to  the variability of the residual power load, in particular due to the variability of power generation from VRE sources;
2. The demand for flexibility due to the uncertainty of the residual power load, notably due to the uncertainty (or lower predictability) of electricity output from VRE sources (‘forecast error’);
3. The demand for flexibility due to the congestion (overloading) of the power grid, resulting from the increase and changing profiles of electricity demand due to the increase in electric vehicles, heat pumps, etc. as well as the increase and changing profiles of power supply from VRE sources.t

eflows aims to provide high-level building blocks for doing practical, applied energy management work. This includes
- Interactive analysis
- Dashboarding
- Real-time steering. 

Uses of eflows:
- **Interactive analysis**: Use `eflows` in the console or within short R scripts to explore different scenarios.

- **Real-time steering**: Embed `eflows` in a web infrastructure, and benefit of its speed to indicate to devices what to do. 

- **Dashboarding and visualization**: The speed of `eflows` allow to integrate it within shiny framework (like in this website).

#### Use of dataframes with the first column as POSIXt object {.test}

Time plays a vital role in eflows, yet the time series objects are not prefered. Instead, are used liberally data frames where the first column is time-based (and normally named "datetime"). This decision is taken for the sake of simplicity; a time series most of the times behaves like a data frame wehre the first column has different semantics. Where it is necessary to operate with actual time series, two helping functions to flip between data frames and time series are used: `df_to_ts` and `ts_to_df`

<div class="broadImage">
<img src="../../app/www/images/general/difference.png">
</div>

## Prefixes and nomenclature (outdated)

eflows strive to offer a consistent API to interact with energy flows. 

For instance, the prefix `ef_` connotes working with an eflows object, the basis for the package operations. Thus, after creating a first object with the function `ef_initialize`, different elements can be included in the model. 

A first distiction lies in the distiction between the functions prefixed with `add_` (The data is ready to be included in the model) and the functions prefixed with `gen_` (from "generate". The data is not readily available yet, but it is generated on the spot, thanks to the application of algorithms, normally involving randomness somehow)

Then, these functions are suffixed by the element in itself that refers to. These include: 

- `battery`, that unless stated otherwise, it refers to an stationary one. On some parts of the package, this is refered also as `batt`.
- `ev`, that stands for electric vehicles. This includes a number of factors, among them the battery and the soc available. When talking about batteries, the distinction between stationary batteries and electric vehicles batteries is signified with `batt` and `ev`.

- `infrastructure`: It refers both to the grid (which most important factor is the "grid capactiy", and the charging points `chp` in the jargon (and chaging stations `chs`, that are bundles of charging points)).(future: divide in two functions?)

- Electricity prices
- Misc, for random stuff that can be used with steering?

Once added the elements to the simulation, results can be generated using the function `ef_simulate`. This function uses the `steer` parameter to bundle the preferences for smart energy

- (future) `steer`: A term used to especify the algorithms that direct the energy flows between elements of the system. It includes factors as the preference for charge/discharge, use of Vehicle to Grid, charge thresholds etc. The steering can be very simple if it includes just default values, or very complex if it consists in functions that change over time and consider the forecast, too. 

Once the simulation has been run, there are a number of functions that easily visualize the results (using dygraphs), and that can be used both for exploratory anaysis and to include directly in websites using shiny. These functions are denoted with `graph_` For instance, `graph_flows`, `graph_ev` and else. Last, the functions prefixed by `display_` are used to modify the graph functions, adding new details or functionalities. 

## What the all-round Spctral Utilities is doing (and serves of inspiration professionally)

Features

- Battery
- Smart Meter
- CHP
- Grid
- Elecric Vehicle Charger
- PV
- HVAC
- Wind

Behind the meter services

- Maximizing self-consumption
- Peak shaving
- Time of use shift
- Back-up power
- PV power forecasting
- Demand forecasting
- Microgrid optimization

Grid services

- Demand response
- Frequency response
- Frequency regulation
- Congestion Relief
- Voltage support
- Resource optimization
