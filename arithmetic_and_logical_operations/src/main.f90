PROGRAM ARITHMETIC_LOGICAL
    IMPLICIT NONE
    INTEGER, PARAMETER :: VALUE_1 = 5
    INTEGER, PARAMETER :: VALUE_2 = 4
    LOGICAL, PARAMETER :: BOOL_1 = .TRUE.
    LOGICAL, PARAMETER :: BOOL_2 = .FALSE.
    PRINT *, "Value_1:", VALUE_1
    PRINT *, "Value_2:", VALUE_2
    PRINT *, "Sum:    ", VALUE_1 + VALUE_2
    PRINT *, "diff:   ", VALUE_1 - VALUE_2
    PRINT *, "prod:   ", VALUE_1 * VALUE_2
    PRINT *, "div:    ", VALUE_1 / VALUE_2
    PRINT *, "realdiv:", REAL(VALUE_1)/VALUE_2
    PRINT *, "exp:    ", VALUE_1 ** VALUE_2
    PRINT *, "boolean1: ", BOOL_1
    PRINT *, "boolean2: ", BOOL_2
    PRINT *, "not expr: ", .NOT. BOOL_1
    PRINT *, "or expr : ", BOOL_1 .OR. BOOL_2
    PRINT *, "and expr: ", BOOL_1 .AND. BOOL_2
    PRINT *, "eqv expr: ", BOOL_1 .EQV. BOOL_2
    PRINT *, "neqv expr:", BOOL_1 .NEQV. BOOL_2
    PRINT *, "equal: ", VALUE_1 == VALUE_2
    PRINT *, "gt:    ", VALUE_1 > VALUE_2
END PROGRAM ARITHMETIC_LOGICAL
