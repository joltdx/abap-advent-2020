CLASS zcl_advent2020_day05_joltdx DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_advent2020_joltdx .
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_boarding_pass,
        id TYPE i,
        input TYPE string,
        row TYPE i,
        column TYPE i,
      END OF ty_boarding_pass.

    DATA mt_input TYPE STANDARD TABLE OF string WITH EMPTY KEY.
    DATA mt_boarding_passes TYPE STANDARD TABLE OF ty_boarding_pass WITH EMPTY KEY.

    METHODS part_1
      RETURNING
        VALUE(result) TYPE string.

    METHODS part_2
      RETURNING
        VALUE(result) TYPE string.

    METHODS read_boarding_pass
      IMPORTING
        input TYPE string
      RETURNING
        VALUE(result) TYPE ty_boarding_pass.

ENDCLASS.



CLASS ZCL_ADVENT2020_DAY05_joltdx IMPLEMENTATION.


  METHOD zif_advent2020_joltdx~solve.

    SPLIT input AT |\n| INTO TABLE DATA(lt_input).
    LOOP AT lt_input INTO DATA(input_line).
      APPEND read_boarding_pass( input_line ) TO mt_boarding_passes.
    ENDLOOP.

    output = |Part 1: { part_1( ) }. Part 2: { part_2( ) }|.

  ENDMETHOD.

  METHOD read_boarding_pass.
    DATA iteration TYPE i.
    DATA low TYPE i.
    DATA high TYPE i VALUE 128.
    DATA half_range TYPE i.

    result-input = input.

    DO 7 TIMES.
      half_range = ( high - low ) DIV 2.
      IF input+iteration(1) = 'F'.
        high = high - half_range.
      ELSE.
        low = low + half_range.
      ENDIF.
      iteration = iteration + 1.
    ENDDO.

    result-row = low.

    low = 0.
    high = 8.
    DO 3 TIMES.
      half_range = ( high - low ) DIV 2.
      IF input+iteration(1) = 'L'.
        high = high - half_range.
      ELSE.
        low = low + half_range.
      ENDIF.
      iteration = iteration + 1.
    ENDDO.
    result-column = low.

    result-id = ( result-row * 8 ) + result-column.

  ENDMETHOD.

  METHOD part_1.
    DATA highest_id TYPE i.

    LOOP AT mt_boarding_passes INTO DATA(boarding_pass).
      IF boarding_pass-id > highest_id.
        highest_id = boarding_pass-id.
      ENDIF.
    ENDLOOP.

    result = highest_id.

  ENDMETHOD.

  METHOD part_2.
    DATA previous_id TYPE i.
    DATA my_seat TYPE ty_boarding_pass.

    " SORT mt_boarding_passes BY id.
    " LOOP AT mt_boarding_passes INTO DATA(boarding_pass).
    "   IF previous_id IS INITIAL.
    "     previous_id = boarding_pass-id.
    "   ELSEIF boarding_pass-id = previous_id + 2.
    "     result = boarding_pass-id - 1.
    "     RETURN.
    "   ENDIF.
    " ENDLOOP.

    DATA lt_ids TYPE STANDARD TABLE OF i.
    LOOP AT mt_boarding_passes INTO DATA(boarding_pass).
      APPEND boarding_pass-id TO lt_ids.
    ENDLOOP.

    SORT lt_ids.

    LOOP AT lt_ids INTO DATA(id).
      IF previous_id IS INITIAL.
        previous_id = id.
        CONTINUE.
      ELSEIF id = previous_id + 2.
        result = id - 1.
        RETURN.
      ENDIF.
      previous_id = id.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
