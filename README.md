# Fortran learner repository

All the basics of fortran will be covered in this repo.

## Hello World

The _f90_ extension will be used for the fortran program.

- Fortran Program starts with _PROGRAM <program_name>_
- Program ends with _END PROGRAM <program_name>_

```f90
PROGRAM HELLO_WORLD
    IMPLICIT NONE
    PRINT *, "Hello, World"
END PROGRAM HELLO_WORLD
```

> Fortran 90 is case insensitive but it is standard practice for keywords to be
> in UPPERCASE.

To compile,

```sh
# To compile the program to objects
gfortran -c -o main.o main.f90
# To link the objects to form the binary
gfortran -o hello_world main.o
```

## Types, Variables, Constants, Operators

### Names in fortran

The names in fortran can have a maximum of 31 alphnumeric characters (letters,
numbers, underscore). The first letter of the identifier must be letter.
identifiers are case insensitive.

### Implicit typing in fortran

Fortran defaults to implicit typing. It means it will assign the variable type
based on the first letter of the variable.

If the name of the variable starts with i, j, k, l, m or n, it is an integer.
Otherwise it is an real number.

It is highly discouraged to use implicit typing. To disable implicit typing,

```f90
PROGRAM DISABLE
IMPLICIT NONE
END PROGRAM DISABLE
```

### Variables

A variable is a data object which can be defined and redefined. All variables
have a type to them. Varibles are to be declared at the beggining of a program
or a sub program in a type declaration statement.

To declare a variable,

```f90
PROGRAM VARIABLE
IMPLICIT NONE
INTEGER :: VAR_1
END PROGRAM VARIABLE
```

> All variables have implicit type assigned to them by default. To avoid it, use
> _IMPLICIT NONE_ keyword.

### Constants

A constant is a data object whose value cannot be changed.

A _literal constant_ is a value without a name such as 3.14(real constant),
"Hello"(character constant), (3.0,3.1)(complex constant), true(literal
constant).

A _named constant_ is a constant value with name. To declare a named constant,
declare it with the rest of the variables at the beginning of the program with
the _parameter_ attribute.

```f90
PROGRAM CONSTANTS
IMPLICIT NONE
INTEGER, PARAMETER :: CONST_1 = 500
END PROGRAM CONSTANTS
```

### Types

Fortran has 5 intrinsic data types. Fortran allows you to use derived data types
which are created by the user.

#### INTEGER

_INTEGER_ type allows you to use integers.

#### REAL

For real numbers the processor provides two _REAL_ types,

- single precision (default REAL)
- double precision

#### COMPLEX

For _COMPLEX_ number. Consist of two real numbers for real and complex part. For
_2+1i_, the value is _(2.0,1.0)_

To assign a variable to another complex, use CMPLX() function.

#### LOGICAL

Two _LOGICAL_ values. _.TRUE._ and _.FALSE._. Periods are important.

#### CHARACTER

For characters and strings. Length of the string is specified by the _LEN_
specifier. If no length is specified, the default is 1.

### Arrays

Series of variables can be collected into array. Arrays can be 1 - 7 dimensional
arrays. Arrays are declared with _DIMENSION_ attribute. Fortran arrays follow 1
based indexing.

To declare the array,

```f90
PROGRAM ARRAYS
    IMPLICIT NONE
    ! Array with 3 x 4 x 5 dimension.
    REAL, DIMENSION(3,4,5) :: MATRIX
END PROGRAM ARRAYS
```

To declare explicit lower bounds,

```f90
PROGRAM ARRAYS
    IMPLICIT NONE
    ! Array with 3 x 4 x 5 dimension.
    REAL, DIMENSION(0:4) :: MATRIX
END PROGRAM ARRAYS
```

### Character Strings

String is a character array. The length of the string can be specified with the
_LEN_ parameter. The slices of string can be referred with (m:n) parameter.

To declare a string,

```f90
PROGRAM STRING
    IMPLICIT NONE
    CHARACTER(LEN = 20) :: STRING
    STRING="lorem ipsum"
    PRINT *,STRING
    PRINT *,STRING(:4)
    PRINT *,STRING(4:)
END PROGRAM STRING
```

### Derived Data types

Derived data type is basically a struct. It consists of different types inside
it.

To access the types inside the derived type, use _%_ operator. To create a
derived type,

```f90
PROGRAM DERIVED_TYPE
    TYPE :: PERSON
        CHARACTER(LEN=20) :: NAME
        INTEGER :: AGE
    END TYPE PERSON
    IMPLICIT NONE
    TYPE(PERSON) :: PERSON_1
    PERSON_1%CHARACTER="John Doe"
    PERSON_1%AGE=18
END PROGRAM DERIVED_TYPE
```

## Arithmetic operators

Fortran supports the following operators,

- exponentiation _**_
- multiplication _*_
- division _/_
- addition _+_
- subtraction _-_

> Fortran follows integer division by default, if no types are specified. To do
> real division, use the _REAL()_ function.

## Logical operators

Fortran supports the following operators,

- .NOT.
- .AND.
- .OR.
- .EQV.
- .NEQV.

> .EQV. and .NEQV. are only for logical statements.

## Relational operators

Fortran supports the following operators,

- == (Equal to)
- \> (Greater than)
- \>= (Greater than or equal to)
- < (Less than)
- <= (Less than or equal to)
- /= (Not equal to)

## Intrinsic functions

Fortran provides these intrinsic function by default.

| Function name           | Purpose                                            |
| ----------------------- | -------------------------------------------------- |
| abs(x)                  | absolute value of numeric argument                 |
| acos(x)                 | inverse cosine function                            |
| asin (x)                | inverse sine function                              |
| atan (x)                | inverse tan function                               |
| complx (x, y, [,ikind]) | converts to complex                                |
| cos (x)                 | cos function                                       |
| cosh (x)                | hyperbolic cosine function                         |
| exp (x)                 | exponential function                               |
| floor (x)               | greatest integer less than or equal to x           |
| int (x)                 | convert to int. Removes the decimal off            |
| log (x)                 | calculate the natural log of x                     |
| log10 (x)               | calculate the log base 10 of x                     |
| mod (x,y)               | remainder function of dividing x/y. x - int(x/y)*y |
| modulo (x/y)            | modulo function (x - floor(x/y)*y)                 |
| nint (x, [,ikind])      | round to nearest integer                           |
| real (x, [,ikind])      | convert to the real number                         |
| sin (x)                 | sine function                                      |
| sinh (x)                | hyperbolic sine function                           |
| sqrt (x)                | square root function                               |
| tan (x)                 | tan function                                       |
| tanh (x)                | hyperbolic tan function                            |

## Simple in and output

To output a text in fortran we use _PRINT_ or _WRITE_ functions.

- _PRINT_ will only writes to the stdout.
- _WRITE_ can write to any file descriptor.

### PRINT

Prints the given args to the stdout.

```f90
PRINT f [, iolist]
PRINT grname
```

_PRINT_ takes the following arguments,

- f (format identifier)
- iolist (list of variables, substrings, arrays, records)
- grname (name of Namelist group)

#### Format identifier

_f_ is an format identifier and can be,

- An asterisk (*) which indicates list-directed io
- The label of the _FORMAT_ statement in the program.
- Integer of the label of _FORMAT_
- String that identifies the format.

#### iolist

_iolist_ can be empty or contain these output items.

- Variables
- Substrings
- Arrays
- Array elements
- Record fields
- Any other expression

#### grname

_grname_ is name of the namelist.

### READ

Reads from the unit identifier given to the args specified.

```f90
READ([UNIT=] u [, [FMT=]f] [, IOSTAT=ios] [, REC=rn] [, END=s] [, ERR=s])iolist 
READ f [, iolist] 
READ grname
```

#### Unit identifier

_u_ can either be external unit identifier or internal file identifier.

An external unit identifier must be one of these:

- A nonegative integer expression
- An asterisk(*), identifying the stdin.

#### Format identifier

_f_ is an format identifier and can be,

- An asterisk (*) which indicates list-directed io
- The label of the _FORMAT_ statement in the program.
- Integer of the label of _FORMAT_
- String that identifies the format.

#### I/O STATUS SPECIFIER

_ios_ must be an integer variable or an integer array element. This will be set
to zero if the read is successful and set to positive number if the read is
unsuccessful.

#### Record number

_rn_ must be a positive integer expression, and can be used for direct-access
files only. rn can be specified for internal files.

#### End-of-File Specifier

_s_ must be the label of an executable statement in the same program unit in
which the READ statement occurs.

#### Error Specifier

_s_ must be the label of an executable statement in the same program unit in
which the READ statement occurs.

#### iolist

_iolist_ can be empty or contain these output items.

- Variables
- Substrings
- Arrays
- Array elements
- Record fields
- Any other expression

#### grname

_grname_ is name of the namelist.

### WRITE

Writes to the unit identifier given to the args specified.

```f90
WRITE([UNIT=] u [, [FMT=]f] [, IOSTAT=ios] [, REC=rn] [, END=s] [, ERR=s])iolist 
WRITE f [, iolist] 
WRITE grname
```

#### Unit identifier

_u_ can either be external unit identifier or internal file identifier.

An external unit identifier must be one of these:

- A nonegative integer expression
- An asterisk(*), identifying the stdin.

#### Format identifier

_f_ is an format identifier and can be,

- An asterisk (*) which indicates list-directed io
- The label of the _FORMAT_ statement in the program.
- Integer of the label of _FORMAT_
- String that identifies the format.

#### I/O STATUS SPECIFIER

_ios_ must be an integer variable or an integer array element. This will be set
to zero if the write is successful and set to positive number if the read is
unsuccessful.

#### Record number

_rn_ must be a positive integer expression, and can be used for direct-access
files only. rn can be specified for internal files.

#### End-of-File Specifier

_s_ must be the label of an executable statement in the same program unit in
which the WRITE statement occurs.

#### Error Specifier

_s_ must be the label of an executable statement in the same program unit in
which the WRITE statement occurs.

#### iolist

_iolist_ can be empty or contain these output items.

- Variables
- Substrings
- Arrays
- Array elements
- Record fields
- Any other expression

#### grname

_grname_ is name of the namelist.

## Control Constructs

### If constructs

_IF_ statement is used to run a program if the condition given is true.

```f90
LABEL IF (LOGICAL_EXPRESSION) THEN
    !STATEMENT
END IF LABEL
```

> The label is optional for the if statement.

> Both _ENDIF_ and _END IF_ is allowed.

### If else constructs

_ELSE_ statement can also be used along with the _IF_ for running statements if
condition was false.

```f90
LABEL IF (LOGICAL_EXPRESSION) THEN
    !STATEMENT
    ELSE
    !ALTERNATE STATEMENTS
END IF LABEL
```

Block if statements can be nested.

```f90
LABEL IF (LOGICAL_EXPRESSION) THEN
    !STATEMENT 1
    ELSE IF (LOGICAL_EXPRESSION) THEN
    !STATEMENT 2
    ELSE IF (LOGICAL_EXPRESSION) THEN
    !STATEMENT 3
    ELSE IF (LOGICAL_EXPRESSION) THEN
    !STATEMENT 4
    ELSE 
    !ALTERNATE STATEMENTS
END IF LABEL
```

### Case constructs

The case construct has the following form,

```f90
LABEL SELECT CASE (EXPRESSION)
    CASE (SELECTOR_1):
    !STATEMENT
    CASE (SELECTOR_2):
    !STATEMENT
    CASE (SELECTOR_3):
    !STATEMENT
    CASE DEFAULT:
    !DEFAULT STATEMENT
    END SELECT LABEL
```

> Range of numbers can be specified in a case using (m:n)

## Loops

### Do loops

The simplest loop can be achieved using _DO_ loop in fortran. The following code
is an endless loop.

```f90
LABEL DO
    !STATEMENT
END DO LABEL
```

To break out of the do loop, use the _EXIT_ statement.

```f90
LABEL DO
    !STATEMENT
    IF (EXPRESSION) THEN
    EXIT
    END IF
END DO LABEL
```

To skip to the next iteration use _CYCLE_ statement.

```f90
LABEL DO
    !STATEMENT
    IF (EXPRESSION) THEN
    CYCLE
    END IF
END DO LABEL
```

To specify a do to iterate said times. use a _DO VAR=START, END, STEP_ statement

```f90
INTEGER :: I !CONTROL VARIABLE
DO I=0,5
    !STATEMENTS
END DO
```

### Do while Loop

To execute a condition while true, use _DO WHILE_ loop.

```f90
LABEL DO WHILE (LOGICAL EXPRESSION)
    !STATEMENTS
END DO WHILE LABEL
```

## Procedures

A program can be built up from a collection of program units. They are,

- Main program
- Modules
- External subprogramms or Procedures

## Subprogram or procedures

Subprogramms or Procedures can be called from the program.They can be,

1. Functions
2. Subroutines

### Functions

Functions accepts arguments and returns a single quantity of any type including
arrays.

> Functions in principle should not modify the arguments. It is a bad practice.

Functions are of two types. They are,

1. _Intrinsic Functions:_ They are built into the language itself.
2. _External Functions:_ They are user defined.

Structure of functions is,

```f90
FUNCTION CIRCLE_AREA (RADIUS)
    IMPLICIT NONE
    REAL :: CIRCLE_AREA
    REAL :: RADIUS
    REAL, PARAMETER :: PI = 3.141592654
    CIRCLE_AREA = PI * ( R**2 )
END FUNCTION CIRCLE_AREA
```

Functions return value is the function name itself. Return type and the argument
type should be declared inside the function. Arguments are dummy and the real
variable from where the function is called is used.

> Note that _IMPLICIT NONE_ should be used inside the function as well as the
> main program.

The result of the function can be given a different name using the _RESULT_
option. The _RESULT_ option is optional in most cases but must be used when
using the recursive functions.

```f90
FUNCTION CIRCLE_AREA (RADIUS) RESULT (AREA)
    IMPLICIT NONE
    REAL :: CIRCLE_AREA
    REAL :: RADIUS
    REAL, PARAMETER :: PI = 3.141592654
    AREA = PI * ( R**2 )
END FUNCTION CIRCLE_AREA
```

To call the function from the program, one must declare the function with type
to use. Functions can be assigned to the variables directly.

```f90
PROGRAM MAIN
    IMPLICIT NONE
    REAL, PARAMETER :: RADIUS = 2
    REAL :: CIRCLE_AREA
    PRINT *, CIRCLE_AREA (RADIUS)
END PROGRAM MAIN
FUNCTION CIRCLE_AREA (RADIUS)
    IMPLICIT NONE
    REAL :: CIRCLE_AREA
    REAL :: RADIUS
    REAL, PARAMETER :: PI = 3.141592654
    CIRCLE_AREA = PI * ( R**2 )
END FUNCTION CIRCLE_AREA
```

#### RETURN Statements

To return from a function early, use the _RETURN_ keyword.

# License

This project is licensed under _GNU GPL v3.0 or later_ license. Feel free to use
the project.
