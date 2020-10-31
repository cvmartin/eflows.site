
Note that the `flex` subfield of `demand` admits not only a flexibility matrix, but a *list* of flexibility matrices. This means that an arbitraty number of devices can be managed at the same time, each one of them with its own flexibility preferences. You can see a live example of this in the discussion about [Electric Vehicles charging](.#ev).
<!-- Each flexibility matrix corresponds to a single *item*. An item can be a water pump, a dishwasher, an electric vehicle or anything else with flexible demand. Remarkably, `e_demand$flex` accepts not only a `flex_mtx`, but a *list* of `flex_mtx`, which means that a single execution of `foreshift()` can manage an arbitrary number of items, each one of them with an arbitray flexibility matrix.  -->
<!-- The combination of two or more items in the `e_demand` object offers a powerful option for the management of flexible demans accross several devices. Electric vehicles are a prime case of it, as discusse in THE LINK.  -->
<!-- `foreshift()` allocates flexible demand first by timestep, and second by level of flexibility. In the case of two or more items with the same level of flexibility in the same timestep, the order of the several `flex_mtx` in `e_demand$flex` matter. The items placed first will be allocated first their demand, which means that they may have better utility. The final result doesn't matter in a *for the common good* approach. -->