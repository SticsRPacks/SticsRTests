# Release Notes


## [STICS 10.5.0](https://forgemia.inra.fr/stics-dev/modulostics/-/tags/stics_v10.5.0) (2025-12-04)

### Documentation
- Edit todo_new_version.md ([!575](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/575))
- Resolve "improve JavaSTICS documentation" ([#408](https://forge.inrae.fr/stics-dev/modulostics/-/work_items/408), [#409](https://forge.inrae.fr/stics-dev/modulostics/-/issues/409))
- Add git_commit_template.md ([#323](https://forge.inrae.fr/stics-dev/modulostics/-/issues/323), [#326](https://forge.inrae.fr/stics-dev/modulostics/-/issues/326))
- Improve release notes contents using issues and conventional commits format ([#325](https://forge.inrae.fr/stics-dev/modulostics/-/issues/325), [#326](https://forge.inrae.fr/stics-dev/modulostics/-/issues/326))
- Update commit template and contributing guidelines ([!501](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/501))
- Favor functional approach over imperative loops ([!481](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/481))
- Update instructions for book generation and deployment ([!462](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/462))
- Fix unit in comment of rlj variable in generated file ([#381](https://forge.inrae.fr/stics-dev/modulostics/-/issues/381))
- Adjust invalid min/max values in inputs.csv (codes 1/2) ([#401](https://forge.inrae.fr/stics-dev/modulostics/-/issues/401))
- Fix unit in comment of rlj variable in generated file ([#381](https://forge.inrae.fr/stics-dev/modulostics/-/issues/381))
- Clarify "des" abbreviation for desiccation ([!497](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/497))

### Pre STICS
- Add initial README with features, usage, and project structure ([#346](https://forge.inrae.fr/stics-dev/modulostics/-/issues/346))
- Replace homemade git functions with simple-git library ([#362](https://forge.inrae.fr/stics-dev/modulostics/-/issues/362))
- Migrate pre_stics to Typescript ([#346](https://forge.inrae.fr/stics-dev/modulostics/-/issues/346), [#362](https://forge.inrae.fr/stics-dev/modulostics/-/issues/362))
- Use tag on current commit instead of main branch latest ([!496](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/496))

### STICS
#### Refactoring
- Centralize degrees to radians angle conversions in a single module ([!470](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/470))
- Centralize PI ([!524](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/524))
- Celsius to kelvin conversion in a single function ([!523](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/523))
- Removing useless variables from structures ([!498](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/498))
- Remove commented debug write statements ([!507](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/507))
- Calculation of day of simulation, doy ([#396](https://forge.inrae.fr/stics-dev/modulostics/-/issues/396))
- N fertilization ([!502](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/502))
- Clean and comment photoperiod calculation + add unit tests ([!467](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/467))
- eauEntrant subroutine ([#349](https://forge.inrae.fr/stics-dev/modulostics/-/work_items/349))


#### Features
- Extend RFPI calculation until imat to fix early maturity drift in short-day conditions ([#368](https://forge.inrae.fr/stics-dev/modulostics/-/issues/368))
- Add validation and error message for P_locferti bounds in fertilizer supply ([!513](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/513))
- Add `.generated` suffix to generated files ([!566](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/566))

#### Bug fixes
- Water supply calculation for intercropping case ([#357](https://forge.inrae.fr/stics-dev/modulostics/-/work_items/357))
- Add missing array initialization of P_upvttapI ([#401](https://forge.inrae.fr/stics-dev/modulostics/-/issues/401))
- Fix transrad computations related to geometry ([#399](https://forge.inrae.fr/stics-dev/modulostics/-/issues/399))
- Update and fix variables end checks ([!548](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/548))
- Doy calculation from days number after simulation start ([#394](https://forge.inrae.fr/stics-dev/modulostics/-/issues/394), [#395](https://forge.inrae.fr/stics-dev/modulostics/-/issues/395))
- Move plant properties from Stics_Transit to Plant structures to fix dominance switch error ([#378](https://forge.inrae.fr/stics-dev/modulostics/-/issues/378))
- Extend command_line_str and cwd buffers to 1024 to handle long paths ([!506](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/506))
- When ktrou parameter is set to -999 and radiative balance is activated ([#317](https://forge.inrae.fr/stics-dev/modulostics/-/issues/317))
- BREAKING CHANGE: replace declination calculation with lookup table ([#337](https://forge.inrae.fr/stics-dev/modulostics/-/issues/337))
- Fix value for vitircarbT for the wheat in usm_fababean_wheat (ficplt2.txt) ([!568](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/568))
- Remove unix-only linebreak rule to allow CRLF on Windows ([!488](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/488))
- Fix gitlab-release-notes eslint configuration ([!487](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/487))

#### Tests
- Migrate end-to-end tests from Fortran to TypeScript ([#336](https://forge.inrae.fr/stics-dev/modulostics/-/issues/336))
- Add --fix option to automatically update expected results ([#389](https://forge.inrae.fr/stics-dev/modulostics/-/issues/389))
- Allow for excluding some usms from e2e using the .exclude flag in the folder name ([!534](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/534))
- Allow auto-update of expected outputs to match actual results ([#338](https://forge.inrae.fr/stics-dev/modulostics/-/issues/338))
- Moved e2e from Fortran to TypeScript and improved section structure ([!532](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/532))

#### Style
- Fix all indentation issues ([!494](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/494))
- Configure fprettify as Fortran formatter with 4-space indentation ([!480](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/480))

### JavaSTICS
#### Refactoring
- Rewrite in modern Java the way AffichageFichierXML works ([!500](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/500))
- Rewrite Usm/UsmData mapping with functional style and records ([!563](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/563))
- Simplify XML node class ([!527](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/527))

#### Bug fixes
- Fix url ([!576](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/576))

### CI
- Use dev container for reproducible development environment ([#391](https://forge.inrae.fr/stics-dev/modulostics/-/issues/391), [#392](https://forge.inrae.fr/stics-dev/modulostics/-/issues/392))
- Split GitLab CI config into multiple files ([#366](https://forge.inrae.fr/stics-dev/modulostics/-/issues/366))
- Use docker-stics docker image in gitlab-ci pipeline ([!489](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/489))
- Include e2e_errors.txt in GitLab job artifacts ([!472](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/472))
- Npm install need to be called inside the main function instead of inside gitlab ci ([!491](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/491))
- Upload executables to GitLab Generic Package Registry ([#335](https://forge.inrae.fr/stics-dev/modulostics/-/issues/335))
- Add ajustJavaStics build and rename Stics exe to stics_modulo ([!461](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/461))
- Create release on tag push ([#334](https://forge.inrae.fr/stics-dev/modulostics/-/issues/334))
- Add script to generate clean source archive for GitLab release ([#332](https://forge.inrae.fr/stics-dev/modulostics/-/issues/332))


### Evaluation
#### Documentation
- Add report templates by species ([!559](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/559))
- Improving notebook_comparison_usms_sms.jl for analysing models' outputs ([!555](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/555))

#### Features
- New notebook for fast model debugging ([!550](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/550))
- Added existing codes for evaluation ([!504](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/504))
- Added julia notebook for variables dynamics comparison for 2 versions of the model. ([!475](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/475))
- Improve notebook_comparison_expected_current.jl ([!541](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/541))
- Add RMSE table to notebook_comparison_expected_current.jl ([!542](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/542))
- Add eval scripts ([!529](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/529))
- Notebook improvement ([!526](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/526))
- Improvement of the julia notebook & N income by water supply ([!503](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/503))
- Simulation re-triggering due to global variable in notebook_comparison_usms_sms.jl ([#398](https://forge.inrae.fr/stics-dev/modulostics/-/issues/398))
- Add evaluation/debugging notebook to Docker and add it as a shortcut command ([!567](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/567))
- Updates for evaluation (utilities, files) ([!511](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/511))

#### Bug fixes
- Evaluation notebook `versions_found` not working properly ([!572](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/572))
- Simulations not properly imported or run on the first launch of the evaluation notebook ([#415](https://forge.inrae.fr/stics-dev/modulostics/-/issues/415))
- Simulation re-triggering due to global variable in notebook_comparison_usms_sms.jl ([#398](https://forge.inrae.fr/stics-dev/modulostics/-/issues/398))
- Fix wrong variable name in notebook_comparison_expected_current.jl ([!545](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/545))

## General
- Revert !543 -> use gdb for all, and lldb only for macos ([!545](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/545))
- Use lldb instead of gdb ([#390](https://forge.inrae.fr/stics-dev/modulostics/-/issues/390))


## [STICS 10.4.1](https://forgemia.inra.fr/stics-dev/modulostics/-/tags/stics_v10.4.1) (2025-07-30)

### JavaSTICS

- Fixed a crash on Linux during LAI interpolation [(#329)](https://forge.inrae.fr/stics-dev/modulostics/-/issues/329)

## [STICS 10.4.0](https://forgemia.inra.fr/stics-dev/modulostics/-/tags/stics_v10.4.0) (2025-07-28)

### Unification of STICS, JavaSTICS and Documentation Repositories

With version **10.4.0**, we are introducing a major structural evolution:
**STICS**, **JavaSTICS**, and the online version of the **STICS Book** are now all hosted in a **single GitLab repository**.

Until now:

* The **STICS engine** was versioned independently (e.g. `v10.3.0`)
* The **JavaSTICS UI** followed its own versioning (e.g. `v1.5.3`)
* The **STICS documentation** (based on [this publication](https://www.quae.com/produit/1809/9782759236794/stics-soil-crop-model)) had a separate lifecycle (with an RMarkdown source maintained elsewhere)

This fragmented setup made it harder to track compatibility and maintain a coherent release strategy.

From now on:

* **One repository**, **one version number**, and **one unified release**.
* The version tag (e.g. `v10.4.0`) applies to the STICS engine, the JavaSTICS UI, and the accompanying documentation.
* This streamlines the development and delivery process, reduces confusion for users, and ensures consistent evolution across components.

We hope this change improves clarity, traceability, and collaboration across the community.
Of course, older versions remain accessible for reference, but we encourage you to adopt this new, unified structure moving forward.

### STICS
#### Features

- Added new output variables [(#324)](https://forge.inrae.fr/stics-dev/modulostics/-/issues/324):
  - `QNfeuilleres` (N content of the leaves)
  - `ratioTF` (Stem to leaves ratio)
    
#### Fixes
- Missing report rows now properly generated in specific edge cases [(#305)](https://forge.inrae.fr/stics-dev/modulostics/-/issues/305)
- Improved leap year handling logic [(!449)](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/449)
- Full validation of `inputs.csv` rows with unit corrections [(#311)](https://forge.inrae.fr/stics-dev/modulostics/-/issues/311)
- Correct variable now used to determine soil fertilisation validity [(!426)](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/426)
- Fixed incorrect `ires` calculation for roots [(#67)](https://forge.inrae.fr/stics-dev/modulostics/-/issues/67)
- Updated version info URL from `stics.paca.hub.inrae.fr` to `stics.inrae.fr` [(#297)](https://forge.inrae.fr/stics-dev/modulostics/-/issues/297)
- Removed 33 compiler warnings (cleaner build output) [(!362)](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/362)
- Renamed `radish_plt.xml` to `proto_radish_plt.xml` to reflect its prototype status [(#270)](https://forge.inrae.fr/stics-dev/modulostics/-/issues/270#note_504084)

### JavaSTICS

#### Features [(#164)](https://forge.inrae.fr/stics-dev/modulostics/-/issues/164)

Several usability enhancements were implemented to streamline the JavaSTICS user interface:: 

- Tabs can now be closed with middle-click [(!434)](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/434)
- Automatically opens the STICS error log file on simulation failure [(!433)](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/433)
- Added ability to cancel multi-USM simulations mid-run [(!430)](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/430)
- Double-click now selects a USM in the multi-run dialog [(!431)](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/431)
- Added "Add all" button in the multi-run dialog for batch selection [(!429)](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/429)

#### Fixes
- Relaxed USM naming constraints [(#310)](https://forge.inrae.fr/stics-dev/modulostics/-/issues/310)
- `culturean` values of `0` now auto-corrected to `2` in `usms.xml` for 2-year crop simulations [(#14)](https://forge.inrae.fr/stics-dev/modulostics/-/issues/14)
- Validation logic now accepts `999` as a null-equivalent value [(#130)](https://forge.inrae.fr/stics-dev/modulostics/-/issues/130)

### Tools
- All development and repository access has been migrated to forge.inrae.fr (formerly ForgeMIA) [(#300)](https://forge.inrae.fr/stics-dev/modulostics/-/issues/300)
- New CI job added for Windows builds [(#292)](https://forge.inrae.fr/stics-dev/modulostics/-/issues/292)
- New utility to fetch recent commits along with their associated merge requests [(#299)](https://forge.inrae.fr/stics-dev/modulostics/-/issues/299)

### Documentation

- Documented the `input-path` parameter in the CLI help [(!422)](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/422)
- Refined the *JavaSTICS 1.5.3 / STICS 10.3.0* section in `RELEASE_NOTES.md` [(!421)](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/421)
- Migrated legacy `changes.pdf` documentation to `RELEASE_NOTES.md` [(#296)](https://forge.inrae.fr/stics-dev/modulostics/-/issues/296)
- Updated evaluation reports for STICS version 10.3 [(!415)](https://forge.inrae.fr/stics-dev/modulostics/-/merge_requests/415)
  
## [JavaSTICS-1.5.3 / STICS 10.3.0](https://forgemia.inra.fr/stics-dev/modulostics/-/tags/stics_v10.3.0) (2025-03-12)

### STICS

#### Features  
- Introduced a new STICS command-line argument `input-path` to specify the USM folder input path [(!383)](https://forgemia.inra.fr/stics-dev/modulostics/-/merge_requests/383)  
- Snow module state variables are now written at the end of a simulation only if the module is activated [(!381)](https://forgemia.inra.fr/stics-dev/modulostics/-/merge_requests/381)  
- Modified the behavior for malformed `prof.mod`: now generates a warning instead of an error [(!402)](https://forgemia.inra.fr/stics-dev/modulostics/-/merge_requests/402)  

#### Fixes  
- Included integer parameters in the forcing process through the `param.sti` file (i.e., names starting with `code` or `cod`) [(!400)](https://forgemia.inra.fr/stics-dev/modulostics/-/merge_requests/400)  
- Filtered irrigation and fertilization operations when the date is `999` [(!364)](https://forgemia.inra.fr/stics-dev/modulostics/-/merge_requests/364)  
- Ensured irrigation dates are below `732` [(!363)](https://forgemia.inra.fr/stics-dev/modulostics/-/merge_requests/363)  
- PET values read from `climat.txt` are now assigned only if they are not `-999` [(!396)](https://forgemia.inra.fr/stics-dev/modulostics/-/merge_requests/396)  
- Resolved compilation warnings related to `maybe-uninitialized` [(!374)](https://forgemia.inra.fr/stics-dev/modulostics/-/merge_requests/374); [(!375)](https://forgemia.inra.fr/stics-dev/modulostics/-/merge_requests/375)  
- Removed duplicate `inputs.csv` and `outputs.csv` files [(!365)](https://forgemia.inra.fr/stics-dev/modulostics/-/merge_requests/365)  
- Increased simulation duration limit from 100 to 1,000 years [(!384)](https://forgemia.inra.fr/stics-dev/modulostics/-/merge_requests/384)  
- Invalid fertilization dates are now ignored, with a warning [(!408)](https://forgemia.inra.fr/stics-dev/modulostics/-/merge_requests/408)  

### JavaSTICS

#### Fixes  
- Fixed issue when cloning a soil profile to add a new one [(!392)](https://forgemia.inra.fr/stics-dev/modulostics/-/merge_requests/392)  

### Book

- Implemented automatic deployment of the book, now available at [https://stics-dev.pages.mia.inra.fr/modulostics/](https://stics-dev.pages.mia.inra.fr/modulostics/) [(!356)](https://forgemia.inra.fr/stics-dev/modulostics/-/merge_requests/356)  
- Added documentation for modifying the book using Git and GitLab [(!391)](https://forgemia.inra.fr/stics-dev/modulostics/-/merge_requests/391); [(!388)](https://forgemia.inra.fr/stics-dev/modulostics/-/merge_requests/388)  
- Updated URLs from `www6.paca.inrae.fr` to `stics.inrae.fr` [(!389)](https://forgemia.inra.fr/stics-dev/modulostics/-/merge_requests/389)  
- Fixed long figure captions defined outside of code chunks [(!360)](https://forgemia.inra.fr/stics-dev/modulostics/-/merge_requests/360)  

---

## [JavaSTICS-1.5.2 / STICS 10.2.0](https://forgemia.inra.fr/stics-dev/modulostics/-/tags/10.2.0) (2024-10-08)
### STICS
#### Fixes
- re-activation of automatic irrigation at sowing [(!347)](https://forgemia.inra.fr/stics-dev/modulostics/-/merge_requests/347)

---

## [JavaSTICS-1.5.2 / STICS 10.1.1](https://forgemia.inra.fr/stics-dev/modulostics/-/tags/10.1.1) (2024-03-07)
### STICS
#### Features
- added  License Cecill C, user conditions and disclaimer [(!191)](https://forgemia.inra.fr/stics-dev/modulostics/-/merge_requests/191)
- Daily output files format (header alignment with columns) [(!164)](https://forgemia.inra.fr/stics-dev/modulostics/-/merge_requests/164)
- Production of soil profiles files for multiple variables [(#7)](https://forgemia.inra.fr/stics-dev/modulostics/-/issues/7)
- Build compatible with MacOS system [(!186)](https://forgemia.inra.fr/stics-dev/modulostics/-/merge_requests/186)
- Code refactoring and quality (checks and unit tests)
- Messages management, content (warnings and errors) and
logging process
- Deactivation of irrigation at sowing/planting in automatic
irrigation use case [(!206)](https://forgemia.inra.fr/stics-dev/modulostics/-/merge_requests/206)
- New output variables in daily files
  - plant code `pla` [(!181)](https://forgemia.inra.fr/stics-dev/modulostics/-/merge_requests/181)
  - dominant status of the crop `is_dominant` [(!212)](https://forgemia.inra.fr/stics-dev/modulostics/-/merge_requests/212)
#### Fixes
- Arrays out of bounds problems
- Roots: avoid negative values in ponderation calculation
### JavaSTICS
#### Features
- added Cecill B, user conditions and disclaimer [!30](https://forgemia.inra.fr/stics-dev/javastics/-/merge_requests/30)
- Display of fields in XML explorers
- Compatibility checks between xml files and model version
- Parameters information display and help
- Parameter selection in optimization feature
- Consistency checks
- Graphics display (legend,...)
- Automatic fix for stages parameters description in workspace
crop management files for automatic irrigation definition
- Java version: upgraded to version 17
#### Fixes
- Formatting weather data files (using an input data subset)
- Optimization process: configuration, criteria choice and
calculation and outputs
- Output variables selection
- Multiple variables selection for graphics
- Setting phenological stages for driving automatic irrigation (XML files automatic fix)
### Book
- Stages duration table

---

## JavaSTICS-1.5.1 / STICS 10.0.0
- Fixes in XML plant files: some varietal parameters for "yield formation"

---

## [JavaSTICS-1.5.0 / STICS 10.0.0](https://forgemia.inra.fr/stics-dev/modulostics/-/tags/stics_v10) (2022-10-26)
**Notice**: this distribution is a major version either for the graphical interface and tools, or for the model. Previous versions XML files are not usable anymore.  
So, XML files structure must be upgraded; see JavaStics user guide for details on using the conversion tools.

### STICS
#### Features
- This new version of the model (V10.0.0) has been developed to
improve the simulation of perennial crops through several new
options :
  - C and N reserves dynamics during the crop cycle and on the
long term (code_acti_reserve)
  - Nitrogen demand and dynamic mortality of roots during the
crop cycle (code_rootdeposition)
  - Two kinds of roots (code_diff_root) are considered, fine and
coarse ones, with different lifespan
  - The effect of photoperiod on biomass and N allocation in the
crop (codephot_part)
  - Roots distribution within the soil profile to have a more robust
simulation of their density in layers (codedisrac)
- These options are available and have been parameterised for
various perennial crops (Miscanthus x giganteus, Medicago sativa L.
and Panicum virgatum)
- This version of the model also allows :
  - Managing long term simulations of cropping systems
including perennial crops.
  - Taking into account rear effet of crop management on
biomass through its effects on crop C and N reserves
  - Reproducing the low soil mineral nitrogen content for
perennial cropping systems by simulating nitrogen crop
uptake and its immobilization by residues (including dead
roots)
  - Improving the simulation of soil organic matter for perennial
cropping systems and rear effect of perennial crop
destruction on the evolution of soil mineral N content (to be
confirmed on larger dataset)
- Some of these new formalisms are also applicable for annual crops
(code_rootdeposition and codedisrac). However, using them may
require a root parameterization improvement. For now, only the
wheat plant file is provided with these news formalisms activated.
#### Documentation
- The new STICS book (numeric, now produced with Rmarkdown) is
describing in details all the above listed formalisms (html version,
available for download on the STICS forge).
- Input parameters and output variables list (inputs.csv, outputs .csv)
attached to new formalisms
- The JavaSTICS documentation using Rmakdown, incuding
information about R packages dedicated to STICS files and
simulations management
#### Parameters
- Some plant parameters are now attached to varieties parameters
- New formalisms parameters have been cleaned: removed (useless
options) or moved in specific parameters files.
#### Fixes in code
- Grass simulations chaining
- Several kind of fertilizers management
- Automatic irrigation between 2 dates or crop stages
- Some initialisations (especially for cut crops)
#### Fixes in files
- Report: table of the soil initial content, units
- Variables description file (outputs.csv): some units were fixed
- Parameters description file (inputs.csv): some names, units or
boundaries were fixed
### JavaSTICS
#### Features
- Parameter estimation can now be done using repetitions of the
optimization process and the configuration interface has been
clearly improved
- Appearance/ergonomy of the graphical interface (including a theme
switch light/dark)
- Using Java 11 virtual machine.
- Reactivity
- Files management
- Simulations are faster than under the previous interface.
#### Files
- New USMs dedicated to Miscanthus simulations (example directory)
- New version of the Wheat plant file adapted for using the model new options
- Evolution of XML files structure for including new options,
parameters, and options integration for varietal parameters in plant files

---

## [Stics 9.2](https://forgemia.inra.fr/stics-dev/modulostics/-/tags/stics_v92) (2021-02-15)

#### Documentation
- Input parameters and output variables list (inputs.csv, outputs .csv)
#### Fixes in code
- Message number related to plant parameters checking
- Nitrogen plant content calculation (fixing vine case)
- N content initialization for falling dead leaves
- Nitrogen plant content for vine
- Report writing for the climatic simulations chaining case
- Snow
  - depth days sum re-initialization for climatic chaining
  - up-scaling the variable dimension for storing snow depth days
sums
#### Fixes in files
- Rounding precision fixes in some general parameters values
#### Features
- The parameter for forcing irrigation at emergence can now be used
either for manual or for automatic irrigation calculation
- New output variables
  - for residues and N plant content management (QNplantenp,
dltaremobilN, restemp)
- Some parameters moves into plant pool
- Stabilization of harvested organs nitrogen content
- Warning for residues list limitation to 10 in the crop management
file
- Fixing the residue type in case of soil incorporation
- Increasing of significant digits for output variables (scientific format)
either for daily or report files

---

## [JavaStics-1.41 / Stics 9.1](https://forgemia.inra.fr/stics-dev/modulostics/-/commit/0839e412830a7c917925383f84d4f360c2ac8be2) (27-09-2019)
### STICS
#### Documentation
- Units for HR_vol
- New calculation of crop residues incorporation depth into soil
- Input parameters and output variables list (inputs.csv, outputs .csv)
#### Fixes in code
- Meteo data recalculation with snow module and wind speed
recalculation when simulating inter-crop usms
- Chaining a sole crop usm after an associated crop usm
- Some tests and variables types
- Some variables initialization
- Calculation of root dry matter and IRcarb for surgarbeet
- Reading *.mod files according to outputs request and files existence
- Snow depth lasting storage for climatic usms chaining
- Taking account of the plastic mulch
- Correction of units for the calculation of the capillary rise
#### Fixes in files
- Bounds values of **nbinflo** and **stdordebour** in plant files
- Moving some parameters in DurumWheat_ALLUR.xml plant file
- Fix nbfeuilplant value to 3 for sugarbeet plant file
- Moving some parameters of the codazorac option for the plant files
proto_sunflower,proto_turmeric and timothy
#### Features
- Some error messages (in PET calculation, …)
- Source code cleaning and refactoring
- New parameter added for fixing mineralization minimum
temperature
- New option added for calculating crop residues incorporation depth
into soil
- New output variables
  - for drainage and leaching at the bottom of each soil layer and
under profmes
  - etm/etr and etr/etm ratios
- new output variables by soil profile
  - Chum, Nhum C_allresidues and N_allresidues
### JavaSTICS
#### Features
- Optimization process can be performed now on vector parameters
(for example, soil parameters attached to soil layers)
- Correction of the Crespc unit in the inputs.csv file

---

## [JavaStics-1.41 / Stics 9.0](https://forgemia.inra.fr/stics-dev/modulostics/-/commit/00be86232f9bdc02d28ccb9db6dd6e3f7862992c) (2018-10-19)
### JavaSTICS
#### Features
- Access to param_newform.xml has been restored
- New simulation unit examples for testing snow module use, and new
plants (timothy, rice, turmeric)
### Parameterization
- Values of parameters associated to unused options have been replaced
with -999 values (parameters which have not been calibrated)
- Plant parameters : some of them have been moved:
  - **tgmin** and **nbfeuilplant** : outside of options in « emergence and
starting » formalism
  - **tcmin** and **tcmax** : outside of options in « leaves » formalism
  - **vitircarb** and **irmax** : outside of options in « yield formation »
formalism
  - **bdilmax** : outside of options in « nitrogen » formalism
- New usm example for the Timothy plant
### STICS
- Documentation
- Formalisms : snow, mineralization
- Model performances evaluations
#### Fixes
- Some variables initialization and tests
- Conditional tests syntax according to variables types, types
conversion for avoiding warnings, unused variables removed
- Test for chaining an usm over years
#### Features
- Parameters consistency checks, bounds checks, values checks for
activated options (-999 values)
- New output variables (snow, N, …) see JavaStics documentation
- Specific module for projects simulations management
- Formalisms / parameters
  - New humus mineralization formalism
  - Mixing/distribution of water and nitrogen soil content after a
soil tillage
  - New module for producing snow cover: recalculation of
minimum and maximum temperature, and precipitations
- New modules for files management and system operations
- Errors management (new specific log file)
- Calculations: avoiding some loops
- New plant files (timothy, rice, turmeric)

---

## [JavaStics-1.40 / Stics 8.50](https://forgemia.inra.fr/stics-dev/modulostics/-/tags/stics_v850) (2019-04-16)
### JavaSTICS
#### Fixes
- Day of year checks for annual or 2 years’ crops
- Parameters optimization process: open variables list, usms selection
(over 2)
#### Features
- Dialog box for exiting confirmation
- Example scripts for using JavaStics command line
- New executable for Mac OS platform: Stics model and utilities
- Updates on OS detection for automatic executable selection (model, utilities)
- Changing command line interface (from Stics.exe to JavaSticsCmd.exe)
- Parameters optimization output file changes: lower criterion value and corresponding parameter values, usms list used in the processing
### Parameterization
- Updated sunflower and sugarbeet plant files
- Plant parameters files renamed : to distinguish prototypes files, cover crop
files and inter crop files
### STICS
#### Fixes
- Calculation and controls of output dates for profiles
- Grass:
  - Cutting management
  - Delayed cutting day calculation (when passing years)
  - Variables initialization over years
  - Seeded grass: restart stage for next year
  - Initialization in successive simulations case
- Senescence calculation and effect
- Automatic irrigations calculation based on upvt
- Variables
- Report file format
- Plant density calculation for intercropping
- Plastic mulch covering use
- Water and nitrogen stress management
#### Features
- Parameters consistency checks
- New output variables
- Profile file content update
- Formalisms / parameters
  - Nitrification and denitrification
  - Grass: roots death, cutting decision criterion,
  - Multiple thinning management
  - Multiple fertilizer types management


---

## [JavaStics-1.31 / Stics 8.41](https://forgemia.inra.fr/stics-dev/modulostics/-/tags/stics_v831) (2015-03-31)
### JavaSTICS
- Climatic variables: bounds set to float, fix for vapor pressure check rule and maximum bound value
- Documentation: default value for CO2
### Parameterization
- Parameters documentation fixes (names,definitions, bounds, codes)
### STICS
#### Fixes
- Variables names in var.mod file
- Increasing message variable dimensions
#### Features
- Extension of optimizable parameters list

---

## [JavaStics-1.30 / Stics 8.40](https://forgemia.inra.fr/stics-dev/modulostics/-/tags/stics_v84) (2015-03-15)
### JavaSTICS
- command line: adaptations to linux OS
- Stics files management
- climatic dialog for files formatting
- keeping selected input file name for creating new one, or copying it
- sorting parameter files list
- dates bounds for validation in usm run dialog
- confirm popup when exiting Javastics
### Parameterization
- plants : vine et durumwheat plant files (special because one file by
genotype) are renamed and cleaned
- parameters documentation fixes (names,definitions, bounds, codes)
- Param_new_form: add of parameters for coupling with pathogen models (not activated)
### STICS
#### Fixes
- last year simulation for yearly climatic sequence
- variable names (AZamm(2), Qles), initializations (msrac,
irazo,ircarb, Qnplante), type (CO2, real), calculation (qmulch)
- getting residues of previous crop: test for artificial mulch
activation, for all crop management systems
- growth restarting calculation after harvest
- dates conversion in report file
- balance calculations for inter-crops:
  - 2 years crops: stages dates calculations for sowing for a leap year 
  - associated crops: mineralisation calculation, taking into
account precipitations before sowing
  - abscission variables indexation
  - irrigations sums
  - leaves exposition: relative area use for previous day dry
matter calculation, and in case of dominance inversion
#### Minor fixes
- balance informations: intermediate temperature sums, stages
- tests: cultivars numbers,
- warnings: profmes==profsol
- exiting: if incompatible values for codebeso and codeetp
- calculations: setting ndebdes with nrec value rather than
nrecbutoir one when the given stage not reached, masec for
strawberries after harvest
- removing non ascii characters
#### Features
- model execution: exiting code when errors (no more stop),
message at the end of successfull execution
- files path management (Record platform compatibility)
- variables
- co2(n), fco2, fco2s, rendementsec
- Macsur project
- cumulatives variables from sowing date to maturity
(*_from_plt)
- water reserves available for plants or for a given
depth (SoilAvW, SoilWatM)
- for optionnal specific outputs in report file
- Agmip project: stages dates to year days
- model version integration when compiling, getting it from
command line
- variables: keeping matuber after harvest (beet), restoring
lessiv
- messages: for tracking parameters and codes values (history
file), removing useless and french messages
- report file: added location,
- balance file: Sum of Maximal ET (eos+eop) instead of sum of
PET, changes for yield formatting
- soil profile file: increasing days numbers

---

## [JavaStics-1.21 / Stics 8.31](https://forgemia.inra.fr/stics-dev/modulostics/-/commit/4aaf9adc41c21923edf76df1b5d04ce42a2f1d8c) (2013-10-18)
### JavaSTICS
- command line: files generation, rotations run
- information: development stages names in initialization, tables headers
content and size
- usm sorting removed for selection in rotations case
### Parameterization
- general parameters: updates about some parameters values, parameters
names
- plant files parameters updates (rapeseed,ryegrass, mustard)
- parameters documentation fixes (definitions, bounds, codes)
- variables documentation fixes (definitions)
### STICS
#### Fixes
- vernalisation management
- matuber value calculation
- roots density distribution over profiles (nouvrac)
- option management for shelter climatic conditions
- climatic series management with uncomplete years
- management of residues content (water, nitrogen)
- senescence process for grass
- management of PET calculation method and control
#### Features
- initial development stage in report file
- new variables in profile (humirac_z et efNrac_z, up to 60 possible
dates)
- new daily output variables (rlj, efnrac_mean, humirac_mean,efda,
efNrac)
- added day in year number to profile file.
- PET calculating method name in balance file
