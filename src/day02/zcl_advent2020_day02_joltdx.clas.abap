CLASS zcl_advent2020_day02_joltdx DEFINITION
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



CLASS ZCL_ADVENT2020_DAY02_joltdx IMPLEMENTATION.

  METHOD zif_advent2020_joltdx~solve.
    SPLIT input AT |\n| INTO TABLE mt_input.

    output = |Part 1: { part_1( ) }. Part 2: { part_2( ) }|.

  ENDMETHOD.

  METHOD part_1.
    DATA number_of_valid TYPE i.
    DATA low_range TYPE i.
    DATA high_range TYPE i.
    DATA character TYPE c.

    LOOP AT mt_input INTO DATA(input_line).
      SPLIT input_line AT space INTO TABLE DATA(input_split).

      READ TABLE input_split INDEX 1 INTO DATA(range).
      SPLIT range AT '-' INTO TABLE DATA(range_split).
      READ TABLE range_split INDEX 1 INTO low_range.
      READ TABLE range_split INDEX 2 INTO high_range.

      READ TABLE input_split INDEX 2 INTO character.
      READ TABLE input_split INDEX 3 INTO DATA(password).

      FIND ALL OCCURRENCES OF character IN password MATCH COUNT DATA(match_count).
      IF match_count >= low_range AND match_count <= high_range.
        number_of_valid = number_of_valid + 1.
      ENDIF.
    ENDLOOP.

    rv_result = number_of_valid.
  ENDMETHOD.

  METHOD part_2.
    DATA number_of_valid TYPE i.
    DATA low_pos TYPE i.
    DATA high_pos TYPE i.
    DATA character TYPE c.

    LOOP AT mt_input INTO DATA(input_line).
      SPLIT input_line AT space INTO TABLE DATA(input_split).

      READ TABLE input_split INDEX 1 INTO DATA(range).
      SPLIT range AT '-' INTO TABLE DATA(range_split).
      READ TABLE range_split INDEX 1 INTO low_pos.
      READ TABLE range_split INDEX 2 INTO high_pos.

      READ TABLE input_split INDEX 2 INTO character.
      READ TABLE input_split INDEX 3 INTO DATA(password).

      low_pos = low_pos - 1.
      DATA(low_pos_char) = password+low_pos(1).
      high_pos = high_pos - 1.
      DATA(high_pos_char) = password+high_pos(1).

      IF ( low_pos_char = character AND high_pos_char <> character ) OR
          ( low_pos_char <> character AND high_pos_char = character ).
        number_of_valid = number_of_valid + 1.
      ENDIF.
    ENDLOOP.

    rv_result = number_of_valid.
  ENDMETHOD.
ENDCLASS.
