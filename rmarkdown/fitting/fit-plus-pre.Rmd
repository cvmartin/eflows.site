The `Fit formula` field allows to edit and try your own fitting formula The code is executed in the server, so for safety reasons only a selection of R functions are available. Additionally, you can choose one of several predefined formulas:

- **Peak shaving** (`~1*.demand`): the "classic peak shaving", allocating the energy from peaks to valleys of consumption. Note how different are the initial and final fitting curves.

- **To the lowest demand** (`~1*.demand_fixed`): The best moments to allocate energy are always the same, so demand concentrate there. Note how the initial and final fitting curves are the same.

- **To the minimum price**(`~1* .price`): The points with lower price are targeted, with the resulting peaks of demand. 

- **To the renewable energy** (`~-1*.production_fixed`): This formula targets the points of maximum renewable energy production. Note that `.production_fixed` is made negative, so the fitting curve is low when production is high.

- **Profit within a limit** (`~ifelse(.demand < .cap, .price, NA)`): Points with lower price are preferred, but without exceeding the grid capacity `.cap`.

- **Net balance**(`~.demand - .production_fixed`): The demand is assigned either during nights or when the sun shines, skipping the morning and evening peaks. To get a "sharper" effect, try to square the formula: `~.demand^2 - .production_fixed^2`

- **Market price**(`~(0.5 * .price) + (0.5 * .demand)`): Using `.demand` generates a feedback loop: The demand is assigned to the points of lower price, but this increases the demand, what makes this same spot less fitting for further allocation of energy. This seems a more realistic scenario than considering `.price` or `.demand` alone.

- **The middle point**(`(0.3 * .price) + (0.4 * .demand) + (-0.3 * .production_fixed))`): A weighted average of energy demand, energy price and renewable energy availability; as the used in the live example above.

- **Conditional day and night**(`ifelse(.production_fixed > 0, .demand - .production_fixed, (0.5 * .price) + (0.5 * .demand))`): Just an example of how customizable is the fitting curve. Depending on whether there is solar production the calculation of the curve flips between *Net balance* and *Market price*.

- **Uniform layer** (`.demand - .demand_fixed`): This makes the demand allocation dependent just of the flexible demand. The resulting profile tends to form a smooth layer over the fixed demand, also when using random profiles. Note the shape of the fitting curve.
