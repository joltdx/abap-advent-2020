CLASS zcl_advent2020_day01_joltdx DEFINITION
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
        VALUE(rv_result) TYPE string.
    METHODS part_2
      RETURNING
        VALUE(rv_result) TYPE string.
ENDCLASS.



CLASS ZCL_ADVENT2020_DAY01_joltdx IMPLEMENTATION.

  METHOD zif_advent2020_joltdx~solve.

    SPLIT input AT |\n| INTO mt_input.

    output = |Part 1: { part_1( ) }. Part 2: { part_2( ) }|.

  ENDMETHOD.

  METHOD part_1.

    DATA lv_2020_difference TYPE i.
    DATA lv_first_factor TYPE i.
    DATA lv_second_factor TYPE i.
    DATA lv_sum TYPE i.

    LOOP AT mt_input INTO DATA(lv_first_number).
      lv_first_factor = lv_first_number.
      lv_2020_difference = 2020 - lv_first_factor.
      " Should preferably only loop starting at next index...
      LOOP AT mt_input INTO DATA(lv_second_number).
        lv_second_factor = lv_second_number.
        IF lv_second_factor = lv_2020_difference.
          lv_sum = lv_first_factor * lv_second_factor.
          rv_result = |{ lv_first_factor } * { lv_second_factor } = { lv_sum }|.
          RETURN.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD part_2.
    DATA lv_2020_difference TYPE i.
    DATA lv_first_factor TYPE i.
    DATA lv_second_factor TYPE i.
    DATA lv_third_factor TYPE i.
    DATA lv_sum TYPE i.
    DATA lv_first_index TYPE sy-tabix.
    DATA lv_second_index TYPE sy-tabix.

    LOOP AT mt_input INTO DATA(lv_first_number).
      lv_first_factor = lv_first_number.
      " Should preferably only loop starting at next index...
      LOOP AT mt_input INTO DATA(lv_second_number).
        lv_second_factor = lv_second_number.
        lv_2020_difference = 2020 - ( lv_first_factor + lv_second_factor ).
        IF lv_2020_difference > 0.
          " Should preferably only loop starting at next index...
          LOOP AT mt_input INTO DATA(lv_third_number).
            lv_third_factor = lv_third_number.
            IF lv_third_factor = lv_2020_difference.
              lv_sum = lv_first_factor * lv_second_factor * lv_third_factor.
              rv_result = |{ lv_first_factor } * { lv_second_factor } * { lv_third_factor } = { lv_sum }|.
              RETURN.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
