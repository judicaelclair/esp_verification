# CSEE E6863: Formal Verification Project

In this project, we verified ESP's `axislv2noc` component, which will be referred to as the Design-Under-Test (DUT). You can find the ESP repository [here](https://github.com/sld-columbia/esp).

## Project Structure

- `README.md`: describes the project structure and how to run things.
- `src`: directory containing RTL code, namely,
    * `src/axislv2noc_original.vhd`: original implementation of the DUT directly copied from [ESP](https://github.com/sld-columbia/esp). This file is here for reference, but is not used anywhere in this project.
    * `src/axislv2noc.vhd`: modified version of the DUT, which is used for verification.
    * `src/env_pkg.vhd`: contains types, helper functions, constants, etc, which are used throughout this project.
    * `src/fv_util.vhd`: DUT instrumentation.
    * `src/top.vhd`: instantiaties the DUT multiple times, and appropriately connects these instances to the verification tool.
- `properties`: directory containing SVA code, namely,
    * `properties/axislv2noc_sva.sv`: contains all the assertions, assumptions, and cover properties implemented in this project.
    * `properties/util_pkg.sv`: contains utilities used by `axislv2noc_sva.sv`.
-  `doc`: directory containing documents, namely,
    * `doc/report.pdf`: final report of this project.
    * `doc/axislv2noc.pdf`: short document explaining the interface and practical usage of the DUT.

## Running the Project

Due to copyright reasons, the build files cannot be made publicly available. Please send us an email at [jsc2268@columbia.edu](mailto:jsc2268@columbia.edu) for assistance with building the project.

That being said, by default, the NoC model that is as general as possible is enabled. Accordingly, assertions that leverage this model are enabled and take too long to be proven (at least several weeks). If you would like to instead enable the highly constrained NoC model along with its associated highly constrained assertions that take one or two minutes to be proven, set the macro `FAST=1` when building.

If you would like to verify assertions that are **expected to fire**, set the macro `ENABLE_FIRED=1` when building. Note, different assertions are enabled depending on the value of `FAST`. To see all the assertions that are expected to fire to actually fire, run the following,
