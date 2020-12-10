CLASS zcl_advent2020_day10_joltdx DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_advent2020_joltdx .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mt_input TYPE STANDARD TABLE OF i WITH EMPTY KEY.

    METHODS count_options_from_adapter
      IMPORTING
        adapter TYPE i
      RETURNING
        VALUE(result) TYPE i.

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
    DATA lt_adapters TYPE STANDARD TABLE OF i WITH EMPTY KEY.
    DATA options_this_adapter TYPE i.
    DATA combinations_one_adapter_ago TYPE i.
    DATA combinations_two_adapters_ago TYPE i.
    DATA combinations_three_adapters_ago TYPE i.
    DATA combinations TYPE i.

    lt_adapters = mt_input.
    SORT lt_adapters DESCENDING.
    APPEND 0 TO lt_adapters.
    combinations = 1.

    LOOP AT lt_adapters INTO DATA(adapter).
      options_this_adapter = count_options_from_adapter( adapter ).
      CASE options_this_adapter.
        WHEN 3.
          combinations = combinations_one_adapter_ago + combinations_two_adapters_ago + combinations_three_adapters_ago.
        WHEN 2.
          combinations = combinations_one_adapter_ago + combinations_two_adapters_ago.
      ENDCASE.

      combinations_three_adapters_ago = combinations_two_adapters_ago.
      combinations_two_adapters_ago = combinations_one_adapter_ago.
      combinations_one_adapter_ago = combinations.
    ENDLOOP.

    result = combinations.
  ENDMETHOD.

  METHOD count_options_from_adapter.
    DATA adapter_plus_3 TYPE i.
    adapter_plus_3 = adapter + 3.

    LOOP AT mt_input WHERE table_line > adapter INTO DATA(line_adapter).
      IF line_adapter > adapter_plus_3.
        EXIT.
      ENDIF.
      result = result + 1.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
