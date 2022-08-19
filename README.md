# NUMEX
This project has to do with NUMEX (Number-Expression Programming Language). NUMEX programs are written directly in Racket by using the constructors defined by the structs defined at the beginning of project.rkt (which is the code of this project).
<br>
The detailed definition of NUMEX's syntax is explained in project_specification.pdf
## Example
### Input
```
(eval-exp (andalso (bool #f) (div (num 2) (num 0))))
```
### Output
```
(bool #f)
```
