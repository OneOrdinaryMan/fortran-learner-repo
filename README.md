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

### Variables

A variable is a data object which can be defined and redefined. All variables
have a type to them. Varibles are to be declared at the beggining of a program
or a sub program in a type declaration statement.

To declare a variable,

```f90
PROGRAM VARIABLE
IMPLICIT NONE
INTEGER::VAR_1
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
INTEGER, PARAMETER::CONST_1 = 500
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
    REAL, DIMENSION(3,4,5)::MATRIX
END PROGRAM ARRAYS
```

To declare explicit lower bounds,

```f90
PROGRAM ARRAYS
    IMPLICIT NONE
    ! Array with 3 x 4 x 5 dimension.
    REAL, DIMENSION(0:4)::MATRIX
END PROGRAM ARRAYS
```

### Character Strings

String is a character array. The length of the string can be specified with the
_LEN_ parameter. The slices of string can be referred with (m:n) parameter.

To declare a string,

```f90
PROGRAM STRING
    IMPLICIT NONE
    CHARACTER(LEN = 20)::STRING
    STRING="lorem ipsum"
    PRINT *,STRING
    PRINT *,STRING(:4)
    PRINT *,STRING(4:)
END PROGRAM STRING
```

# License

This project is licensed under _GNU GPL v3.0 or later_ license. Feel free to use
the project.
