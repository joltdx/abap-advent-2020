CLASS zcl_advent2020_day14_joltdx DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_advent2020_joltdx .
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS part_1
      RETURNING
        VALUE(result) TYPE string.

    METHODS part_2
      RETURNING
        VALUE(result) TYPE string.

ENDCLASS.



CLASS ZCL_ADVENT2020_DAY14_joltdx IMPLEMENTATION.


  METHOD zif_advent2020_joltdx~solve.

    output = |Part 1: { part_1( ) }\nPart 2:{ part_2( ) }|.

  ENDMETHOD.

  METHOD part_1.
    result = 'todo'.

  ENDMETHOD.

  METHOD part_2.

    result = 'todo'.

  ENDMETHOD.

ENDCLASS.
