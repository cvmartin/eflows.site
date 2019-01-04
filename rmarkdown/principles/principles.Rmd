# Design principles

## Motivation

The energy transition is here, and challenges come with it. In energy systems with more Renewable Energies, Electric Vehicles and Heat Pumps it will be harder to match the generation and consumption of energy. 

Achieving sustainability while minimizing costs will require insights in the energy systems, and the ability to steer their components to decide when to consume (batteries, heat pumps, electric vehicles, smart appliances) and when to produce (in the case of biomass or CHP).

`eflows`is both an experiment to better understand how to realize the energy transition, and a tool to make it happen in practice. Additional tools will be necessary to interface `eflows` with specific hardware in EMS, but for the moment `eflows` can be a reproducible solution for working with the *data* of such EMS.

The package offers a "high-level" object-oriented API, meaning it does complex things with the minimum amount of code. Another way to put it is that the API is "declarative": the user *declares* what he or she wants without the need to know the details of the implementation. 

The intention of this design is to organize all the data relating an EMS in a highly structured `e_frame` object, and then apply "lower-level" functions over this data in an easy way. 

## Functional programming, object-oriented programming

R is a Functional programming language, which means the Object-Oriented (OO) approach described above is unorthodox. `eflows` code often doesn't "feel" as R code. The main difference between the functional and the object-oriented coding approach is that in the later the variables are modified "on the fly", without the need of assigning them to new variables. 

This can make the code harder to understand because the context where it is executed becomes more important. That said, `eflows` tries to minimize errors implementing methods that are "idempotent": Using the same method twice is the same as applying it once (for instance, using `e_frame$set_storage()` twice doesn't set two different storages in `e_frame`; the second application overrides the first one instead.)

While it is debatable whether this way is the right one, `eflows` uses a "classic" object-oriented approach (through the R package `R6`) for a couple of reasons: 

- The build and work of an `e_frame` object has to be quite strict, and lends naturally to the OO paradigm. Using OO programming allows to encapsulate these operations in methods that clarify what can be done with the object. In addition, chaining methods allows to write more terse code.

- EMS software has been traditionally developed in OO languages, like Java. This makes sense, considering that an EMS consists in putting together a number of *objects* with different properties. The intention of `eflows` is to feel familiar to those accustomed to OO programming. 

Ultimately, the objective is to use OO and Functional programming in a synergistic way; instead of being opposite poles in a gradient, both programming styles can reinforce each other by managing the complexity of EMS: organizing the data in objects, and then applying functions over them. 

## Structure

- A core of **C++ algortihms**: C++ is a low-level language. The algorithms (contained in functions) are very "imperative-style" oriented and use plenty of *for loops* that would be too slow implemented in R. C++ is more efficient, and is indeed responsible of `eflows` speed. The algorithms do not include any form of machine learning or predictive modeling; by design `eflows` does not include any form of randomness in its calculations.  

- A first layer of **R functions**: R interacts with C++ thanks to the `Rcpp` package. These functions are more than a simple *wrap* around the C++ functions, because they often "preprocess" the inputs, verifying and preparing them so they can be used by the lower-level C++ functions. These R functions are exposed in `eflows`, so they are properly documented and users can work directly with them if they prefer to skip the OO API. 

- A second layer of **R6 objects**: This is intended as the default API of `eflows`. It makes easy to apply lower-level functions by encapsulating them in methods; for instance, `foreshift()` becomes the method `e_frame$do_foreshift()`. While the function requires a number of arguments as input, the method fetches them directly from the `e_frame` it is called onto.

<div class="broadImage">
<img src="../../www/images/general/structure.png">
</div>

## Extending `eflows`

Another advantage of the OO approach is that `eflows` can be easily extended over its third layer. Because the structure of the `e_frame` objects is consistent, it is simple to design R packages built around functions that take as input an `e_frame`, and then refer to the data stored in specific fields of the object.

This has several interesting possibilities for modularizing code. It could make effortless to relay instructions to specific devices based on the outputs of an `e_frame`, and it would allow to interact with `eflows` directly though an HTTP API using the [OpenCPU](https://www.opencpu.org/) framework (a possibility to be researched). 

`eflows.viz` is an example of this way of working. The idea is to separate concerns: `eflows` takes care of the computation, and `eflows.viz` regards the visualization. 

## The `e_frame` object
`eflows` uses a custom object, called `e_frame`, to organize the relevant data at hand. The object has several fields, each one roughly representing one element of the energy system (production, consumption, storage, infrastructure, etc.). Each field has two subfields: *input* and *output*.

The *input* subfields are populated using a custom method for each one of them (`e_frame$set_demand()`, `e_frame$set_storage()` and so on). Many times, the data introduced with be time-based vectors. The time series itself is defined when initializing the `e_frame`, and then vectors of the same length (to express *demand over time* or *production over time*, for instance) are passed into the methods. In the case of doing real-time steering, the time series would start in the present moment, and then extend into the future. This means that to input the *demand over time* or *production over time* it would be necessary a preliminary work of forecasting them using predictive models.

In any case, the time-based data is *left aligned*. This means that the changes in flows and storages SOC reflect the change of the system once that time step has been concluded. In an hourly-based simulation, the flows that correspond to 14:00 are the ones happened between 14:00 and 15:00, and the SOC of the batteries reflect their status once these energy flows have been resolved. 

While initializing an `e_frame`, it is possible to specify the unit employed, by default *kWh*. Note that this is an energy unit, not a power one. For working with power it is possible to translate it into energy in accordance with the timestep of the timeseries used. A power source of 10 kW will produce a constant energy flow of 10 kWh if the timestep is one hour, or 2.5 kWh if the timestep is 15 minutes.  

A typical workflow consists in initializing the `e_frame` object, populate the *input* subfields with the relevant data, and then apply an algorithm that stores the results in the same object, in the *output* subfields.

The following example contains four statements:

```
#1
testframe <- e_frame$new(sept$datetime[1:168])

#2
testframe$set_demand(
  e_demand$new(fixed = (sept$d_house_smooth[1:168]),
               flex = list(flex_mtx$new(data = as.matrix((rep(0.03,168))),
                                        steps = 4)))
  )
#3
testframe$set_production(e_production$new(fixed = list(solar = sept$solar[1:168])))

#4
testframe$do_foreshift(fit = ~ .demand - .production_fixed)
```
1. Initialize the `e_frame` object. Here it is convenient to setup the timeseries to use (if any). 

2. Use the method `e_frame$set_demand()` to populate the field `testframe$demand$input`. Note how it is used a custom object `e_demand` that distinguishes between fixed demand and flexible one. 

3. Use the method `e_frame$set_production()` to populate the field `testframe$production$input` with a vector representing solar production.

4. Use the method `e_frame$do_foreshift()`, to apply `foreshift()` using the data previously passed into `testframe`. Note how `fit = ~ .demand - .production_fixed` expresses the [objective of the optimization](.#fitting). Try to use `fit = ~ 1*.demand` to do instead "peak shaving". 

Once the `e_frame` has been processed, it contains both the initial information (*input*) and the results of the calculation (*output*). This allows to build over these `e_frame` objects, using them as input to functions that can access their fields to generate new results or side effects. 

For instance, `eflows.viz::viz_fore_input(testframe)` will show the data of `testframe` before it is foreshifted, while `eflows.viz::viz_fore_output(testframe)` will show it for the already processed data. 

