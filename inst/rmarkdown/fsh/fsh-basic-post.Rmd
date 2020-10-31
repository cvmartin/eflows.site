Note how: 
- The lower the flexibility of the flexible layer, the more it resembles to the `original` demand. When the flexibility is `1`, it is identical. A flexibility of `0` may sound intuitive, but it doesn't work; it would mean that the demand has to be realized in zero timesteps.
- With high flexibility and enough flexible volume, the total demand "flattens out"; the peaks and valleys of consumption disappear. 
- Having a flexibility of `n` hours doesn't mean that the demand *will* be foreshifted `n` hours; `n` is the maximum number of hours that the demand *can* be foreshifted; it might be shifted a lower number of hours, or not at all, depending on the [fitting formula](.#fitting). 
