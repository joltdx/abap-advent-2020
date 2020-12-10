CLASS zcl_advent2020_day10_joltdx DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_advent2020_joltdx .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mt_input TYPE STANDARD TABLE OF i WITH EMPTY KEY.

    METHODS part_1
      RETURNING
        VALUE(result) TYPE string.

    METHODS part_2
      RETURNING
        VALUE(result) TYPE string.
ENDCLASS.



CLASS ZCL_ADVENT2020_DAY10_joltdx IMPLEMENTATION.

  METHOD zif_advent2020_joltdx~solve.

    DATA int TYPE i.
    SPLIT input AT |\n| INTO TABLE DATA(lt_input).
    LOOP AT lt_input INTO DATA(input_line).
      int = input_line.
      APPEND int TO mt_input.
    ENDLOOP.
    SORT mt_input.

    output = |Part 1: { part_1( ) }. Part 2: { part_2( ) }|.

  ENDMETHOD.

  METHOD part_1.

    DATA joltage TYPE i.
    DATA count_diff_1 TYPE i.
    DATA count_diff_3 TYPE i.

    LOOP AT mt_input INTO DATA(adapter).
      IF adapter = joltage + 1.
        count_diff_1 = count_diff_1 + 1.
      ELSEIF adapter = joltage + 3.
        count_diff_3 = count_diff_3 + 1.
      ENDIF.

      IF adapter <= joltage + 3.
        joltage = adapter.
      ELSE.
        result = 'nope'.
        RETURN.
      ENDIF.
    ENDLOOP.

    joltage = joltage + 3.
    count_diff_3 = count_diff_3 + 1.

    DATA(diff_multiplied) = count_diff_1 * count_diff_3.
    result = |Joltage: { joltage } | &&
             |Diff_1: { count_diff_1 } | &&
             |Diff_3: { count_diff_3 } | &&
             |Diffs multiplied: { diff_multiplied }|.

  ENDMETHOD.

  METHOD part_2.

    result = 'todo'.

  ENDMETHOD.

ENDCLASS.
