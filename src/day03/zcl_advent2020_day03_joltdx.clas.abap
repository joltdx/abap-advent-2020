CLASS zcl_advent2020_day03_joltdx DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_advent2020_joltdx .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mt_input TYPE STANDARD TABLE OF string WITH EMPTY KEY.
    DATA mv_repeat_length TYPE i.
    METHODS part_1
      RETURNING
        VALUE(result) TYPE string.
    METHODS part_2
      RETURNING
        VALUE(result) TYPE string.
ENDCLASS.



CLASS ZCL_ADVENT2020_DAY03_joltdx IMPLEMENTATION.

  METHOD zif_advent2020_joltdx~solve.

    SPLIT input AT |\n| INTO TABLE mt_input.
    READ TABLE mt_input INDEX 1 REFERENCE INTO DATA(first_line).
    mv_repeat_length = strlen( first_line ).

    output = |Part 1: { part_1( ) }. Part 2: { part_2( ) }|.
  ENDMETHOD.

  METHOD part_1.
    DATA number_trees TYPE i.
    DATA number_space TYPE i.
    DATA horizontal_position TYPE i.
    DATA landscape TYPE string.
    DATA sy_tabix_not_working TYPE abap_bool.

    LOOP AT mt_input REFERENCE INTO landscape.
      IF sy_tabix_not_working = abap_false.
        sy_tabix_not_working = abap_true.
        CONTINUE.
      ENDIF.

      horizontal_position = ( ( horizontal_position + 3 ) MOD mv_repeat_length ).

      IF landscape+horizontal_position(1) = '#'.
        number_trees = number_trees + 1.
      ELSE.
        number_space = number_space + 1.
      ENDIF.

    ENDLOOP.

    result = number_trees.

  ENDMETHOD.

  METHOD part_2.

    result = 'Todo...'.

  ENDMETHOD.
ENDCLASS.
