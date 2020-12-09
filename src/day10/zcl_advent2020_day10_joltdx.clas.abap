CLASS zcl_advent2020_day10_joltdx DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_advent2020_joltdx .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mt_input TYPE STANDARD TABLE OF string WITH EMPTY KEY.

    METHODS part_1
      RETURNING
        VALUE(result) TYPE string.

    METHODS part_2
      RETURNING
        VALUE(result) TYPE string.
ENDCLASS.



CLASS ZCL_ADVENT2020_DAY10_joltdx IMPLEMENTATION.

  METHOD zif_advent2020_joltdx~solve.

    SPLIT input AT |\n| INTO TABLE mt_input.

    output = |Part 1: { part_1( ) }. Part 2: { part_2( ) }|.

  ENDMETHOD.

  METHOD part_1.

    result = 'todo'.

  ENDMETHOD.

  METHOD part_2.

    result = 'todo'.

  ENDMETHOD.

ENDCLASS.
