# CL-RS: A reaction systems simulator

This code is a fast reaction systems simulator. It is written in Common Lisp and it has been tested using SBCL.
The main function is 
```lisp
(defun simulate-rs (filename simulator &optional out))
```
where `filename` is the file containing the description of the reaction system (we use the same format as  [HERESY](https://github.com/aresio/HERESY)), `simulator` is the function used to perform the simulation, can be either `#'simulator-set-theory` or `#'simulator-dependency-graph`. The optional paramter `out` is a filename used to save the output of the simulation (defaulting to the standard output).


## Other simulators

* [HERESY](https://github.com/aresio/HERESY). Written in Python 2, it offers both a CPU-based simulator and a fast GPU-based simulator using CUDA.
* [brsim](https://github.com/scolobb/brsim). Written in Haskell, it offers additional features, including a [web interface](http://combio.abo.fi/research/reaction-systems/reaction-system-simulator/).
