
## About this website

`eflows` is more than code. It is also the personal vision of [the author](.#author) about how EMS and the energy sector can look like in the near future. Therefore, this website is in large part an exposition of the theoretical foundations of this vision, that is materialized in `eflows`.

Because code without examples is impractical, you will also find *live examples* that can be interacted with in real time. These are a thin [shiny](https://shiny.rstudio.com/) layer over `eflows` (used to compute in the server-side) and `eflows.viz` (to visualize).

In addition, there are numerous snippets of code that can be directly copy-pasted in R studio for a working example. Note that these snippets use `sept`, the same "dummy dataset" employed in the live examples of this site. In principle, substituting `sept` with your own dataset is all you need to get production-ready results. 

This website, as `eflows` itself, is in active development and hence largely incomplete. Shorter term plans include the integration of functions `backshift()` and `simulate()`, and including heat pumps and batteries as user cases. 
<!-- Please consider subscribing to the mail list to keep updated about `eflows` releases.  -->

Right now the site exposes how `eflows` can be used for a first application: the charging of Electric Vehicles. The full explanation spans several articles, better read in this order: 

1. [Foreshift](.#foreshift): A first grasp of `eflows` possibilities and its take on flexible demand.
2. [Fitting formula and curve](.#fitting): A comprehensive approach to decide when is the best moment to realize this flexible demand. 
3. [Electric Vehicles charging](.#ev): A first practical `eflows` application, almost production-ready for steering in real-time. 
- [Design principles](.#principles): An additional article that delves into the architecture and the design behind `eflows`.

## Helping to improve `eflows`

`eflows` is largely an experiment in the face of the energy transition. Concepts like *fitting formula*, *foreshift* and *flexibility matrix* are only useful to the extent that they work in real life. 

So, the best way to improve `eflows` is testing it; try it with your own data, see what works and what not, and please provide feedback. You can always write me an email, although I'd rather have bug reports and requests of new functionalities in the corresponding Github pages ([here](https://github.com/cvmartin/eflows/issues) and [here](https://github.com/cvmartin/eflows.viz/issues)). 

And of course, feel free to fork the repository, or propose changes through a pull request.
