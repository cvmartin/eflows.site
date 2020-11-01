
# Foreshift

![Foreshift](../../app/www/images/foreshift/fore.png)

`foreshift()`allocates in the future demand that would instead be realized in the present. It serves to optimize the consumption of energy that has to be dispatched within a time window. If, say, a water pump has to consume 10 kWh between noon and midnight, this is expressed as a demand of 10kWh at noon  *with a flexibility of 12 hours*. 

Foreshifting is based on the idea that energy flexibility is defined not only by its volume and location in time, but also by *how much can it be shifted into the future*.

In its most basic example, a flexible demand can be presented as a "layer" over a volume of fixed demand. "Peak shaving" is the most recognizable optimization: the flexible demand in the hours of peak consumption is delayed to the hours of lower consumption. 
