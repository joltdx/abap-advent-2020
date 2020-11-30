CLASS zcl_advent2020_day01_joltdx DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_advent2020_joltdx .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ADVENT2020_DAY01_joltdx IMPLEMENTATION.


  METHOD zif_advent2020_joltdx~solve.
    DATA lt_input TYPE STANDARD TABLE OF string WITH EMPTY KEY.
    SPLIT input AT cl_abap_char_utilities=>newline INTO lt_input.

    output = lines( lt_input ).

  ENDMETHOD.
ENDCLASS.
