CLASS zcl_advent2020_day09_joltdx DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_advent2020_joltdx .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mt_input TYPE STANDARD TABLE OF i WITH EMPTY KEY.
    DATA mt_input_original TYPE STANDARD TABLE OF i WITH EMPTY KEY.

    METHODS part_1
      RETURNING
        VALUE(result) TYPE i.

    METHODS part_2
      IMPORTING
        invalid_number TYPE i
      RETURNING
        VALUE(result) TYPE string.

    METHODS find_sum_in_prev_batch
      IMPORTING
        number_to_find TYPE i
      RETURNING
        VALUE(result) TYPE abap_bool.

    METHODS find_weakness_in_prev_batch
      IMPORTING
        number_to_find TYPE i
      RETURNING
        VALUE(result) TYPE i.

ENDCLASS.



CLASS ZCL_ADVENT2020_DAY09_joltdx IMPLEMENTATION.

  METHOD zif_advent2020_joltdx~solve.

    SPLIT input AT |\n| INTO TABLE mt_input_original.

    DATA(invalid_number) = part_1( ).
    DATA(encryption_weakness) = part_2( invalid_number ).
    output = |Part 1: { invalid_number }. Part 2: { encryption_weakness }|.

  ENDMETHOD.

  METHOD find_sum_in_prev_batch.
    LOOP AT mt_input INTO DATA(number_one).
      IF sy-tabix = 26.
        EXIT.
      ENDIF.
      DATA(number_one_index) = sy-tabix.
      LOOP AT mt_input INTO DATA(number_two).
        IF sy-tabix < number_one_index.
          CONTINUE.
        ENDIF.
        IF number_one + number_two = number_to_find.
          result = abap_true.
          RETURN.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD find_weakness_in_prev_batch.
    DATA batch_sum TYPE i.
    DATA smallest TYPE i.
    DATA largest TYPE i.
    LOOP AT mt_input INTO DATA(number_one).
      DATA(number_one_index) = sy-tabix.
      batch_sum = number_one.
      smallest = number_one.
      largest = number_one.
      LOOP AT mt_input INTO DATA(number_two).
        IF sy-tabix <= number_one_index.
          CONTINUE.
        ENDIF.
        batch_sum = batch_sum + number_two.
        IF batch_sum > number_to_find.
          EXIT.
        ENDIF.
        IF number_two < smallest.
          smallest = number_two.
        ELSEIF number_two > largest.
          largest = number_two.
        ENDIF.
        IF batch_sum = number_to_find.
          result = smallest + largest.
          RETURN.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD part_1.

    mt_input = mt_input_original.

    LOOP AT mt_input INTO DATA(current_number).
      DATA(current_index) = sy-tabix.
      IF sy-tabix < 26.
        CONTINUE.
      ENDIF.

      IF find_sum_in_prev_batch( current_number ) = abap_true.
        DELETE mt_input INDEX 1.
        CONTINUE.
      ENDIF.

      result = current_number.
      RETURN.

    ENDLOOP.

  ENDMETHOD.

  METHOD part_2.

    mt_input = mt_input_original.

    result = find_weakness_in_prev_batch( invalid_number ).

  ENDMETHOD.

ENDCLASS.
