CLASS zcl_advent2020_day06_joltdx DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_advent2020_joltdx .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mt_input TYPE STANDARD TABLE OF string WITH EMPTY KEY.

    METHODS count_answers
      IMPORTING
        group_input TYPE string
      RETURNING
        VALUE(result) TYPE i.

    METHODS count_answers_everyone
      IMPORTING
        group_input TYPE string
      RETURNING
        VALUE(result) TYPE i.

    METHODS part_1
      RETURNING
        VALUE(result) TYPE string.

    METHODS part_2
      RETURNING
        VALUE(result) TYPE string.
ENDCLASS.



CLASS ZCL_ADVENT2020_DAY06_joltdx IMPLEMENTATION.

  METHOD zif_advent2020_joltdx~solve.

    SPLIT input AT |\n\n| INTO TABLE mt_input.

    output = |Part 1: { part_1( ) }. Part 2: { part_2( ) }|.
  ENDMETHOD.

  METHOD count_answers.

    DATA answered TYPE STANDARD TABLE OF c WITH EMPTY KEY.
    DATA position TYPE i.

    REPLACE ALL OCCURRENCES OF |\n| IN group_input WITH ||.

    DO strlen( group_input ) TIMES.
      position = sy-index - 1.
      APPEND group_input+position(1) TO answered.
    ENDDO.

    SORT answered.
    DELETE ADJACENT DUPLICATES FROM answered.

    result = lines( answered ).
  ENDMETHOD.

  METHOD count_answers_everyone.

    DATA answer TYPE c.
    DATA position TYPE i.
    DATA position_count TYPE i.
    DATA everyone_answer_count TYPE i.

    SPLIT group_input AT |\n| INTO TABLE DATA(group_answers).
    READ TABLE group_answers INDEX 1 INTO DATA(first_persons_answers).
    IF lines( group_answers ) = 1.
      result = strlen( first_persons_answers ).
      RETURN.
    ENDIF.

    DELETE group_answers INDEX 1.

    DO strlen( first_persons_answers ) TIMES.
      position = sy-index - 1.
      answer = first_persons_answers+position(1).
      position_count = 0.
      LOOP AT group_answers INTO DATA(next_persons_answers).
        IF next_persons_answers CA answer.
          position_count = position_count + 1.
        ENDIF.
      ENDLOOP.
      IF position_count = lines( group_answers ).
        everyone_answer_count = everyone_answer_count + 1.
      ENDIF.
    ENDDO.

    result = everyone_answer_count.
  ENDMETHOD.

  METHOD part_1.

    DATA sum_answers TYPE i.

    LOOP AT mt_input INTO DATA(input).
      sum_answers = sum_answers + count_answers( input ).
    ENDLOOP.

    result = sum_answers.

  ENDMETHOD.

  METHOD part_2.

    DATA sum_answers TYPE i.

    LOOP AT mt_input INTO DATA(input).
      sum_answers = sum_answers + count_answers_everyone( input ).
    ENDLOOP.

    result = sum_answers.

  ENDMETHOD.

ENDCLASS.
