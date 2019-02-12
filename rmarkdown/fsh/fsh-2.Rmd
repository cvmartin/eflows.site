## Several flexibility layers and devices

A continuous, uniform layer of flexible demand may reflect some user cases, but ideally `foreshift()` should consider all of them. For instance, a volume of flexible demand of 12 hours *and* a flexible demand of 6 hours may match in time. 

To manage energy data, `eflows` uses a custom object, `e_frame`, that is constructed by populating its several fields with specific data. One of these fields is `demand`, that in turn has two subfields: `fixed` and `flex`.

The `fixed` subfield is just a vector representing the fixed demand. `flex` subfield is more complex: it consists of one or more *flexibility matrix*. In this matrix, the rows are the *timesteps* while the columns represent the *level of flexibility*. As a contrived example:

```
#>       [,1]     [,2] [,3] [,4] [,5]
#>  [1,]    0 2.646572    3    0    0
#>  [2,]    0 8.402889    3    0    0
#>  [3,]    0 7.839502    3    0    0
#>  [4,]    0 9.433603    3    0    0
#>  [5,]    0 2.606275    3    0    0
#>  [6,]    0 3.868473    3    0   10
#>  [7,]    0 9.097361    3    0    0
#>  [8,]    0 8.740563    3    0    0
#>  [9,]    0 7.057992    3    0    0
#> [10,]    0 3.064047    3    0    0
```
Each device (electric vehicle, water pump, smart dishwasher, etc.) can have its flexibility described with a flexibility matrix. The one above informs us that this device: 

- Doesn't have any demand with flexibility equal to 1 (the one that has to be dispatched in the next timestep).
- Has *variable* flexibility equal to 2 (it has to be dispatched in the following two timesteps).
- Has a *constant* flexibility equal to 3.
- Has no flexibility at all equal to 4.
- The flexibility equal to 5 is zero except for a lone *peak* of consumption at the sixth timestep.
