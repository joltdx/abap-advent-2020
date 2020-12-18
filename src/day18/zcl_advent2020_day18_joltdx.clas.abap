CLASS zcl_advent2020_day18_joltdx DEFINITION
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
        VALUE(result) TYPE i.

    METHODS part_2
      RETURNING
        VALUE(result) TYPE i.

    METHODS calculate
      IMPORTING
        expression TYPE string
      RETURNING
        VALUE(result) TYPE i.

    METHODS calculate_p2
      IMPORTING
        expression TYPE string
      RETURNING
        VALUE(result) TYPE i.

ENDCLASS.



CLASS ZCL_ADVENT2020_DAY18_joltdx IMPLEMENTATION.


  METHOD zif_advent2020_joltdx~solve.
    REPLACE ALL OCCURRENCES OF |\r| IN input WITH ||.
    SPLIT input AT |\n| INTO TABLE mt_input.

    output = |\nPart 1: { part_1( ) }\nPart 2: { part_2( ) }|.

  ENDMETHOD.

  METHOD part_1.

    LOOP AT mt_input INTO DATA(input).
      result = result + calculate( input ).
    ENDLOOP.

  ENDMETHOD.

  METHOD part_2.

    LOOP AT mt_input INTO DATA(input).
      result = result + calculate_p2( input ).
    ENDLOOP.

  ENDMETHOD.

  METHOD calculate.
    DATA looking_for_paranthesis TYPE abap_bool.
    DATA par_replace TYPE string.
    DATA operator TYPE c.

    looking_for_paranthesis = abap_true.
    WHILE looking_for_paranthesis = abap_true.
      FIND REGEX '\(([^()]*)\)' IN expression SUBMATCHES DATA(par_expression).
      IF par_expression IS INITIAL.
        looking_for_paranthesis = abap_false.
      ELSE.
        DATA(paranthesis_value) = calculate( par_expression ).
        par_replace = |({ par_expression })|.
        REPLACE ALL OCCURRENCES OF par_replace IN expression WITH paranthesis_value.
      ENDIF.
    ENDWHILE.

    SPLIT expression AT | | INTO TABLE DATA(tokens).
    LOOP AT tokens INTO DATA(token).
      IF sy-tabix = 1.
        result = token.
        CONTINUE.
      ENDIF.

      CASE token.
        WHEN '+' OR '*'.
          operator = token.
          CONTINUE.
        WHEN OTHERS.
          CASE operator.
            WHEN '+'.
              result = result + token.
            WHEN '*'.
              result = result * token.
          ENDCASE.
          CONTINUE.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.

  METHOD calculate_p2.
    DATA looking_for_paranthesis TYPE abap_bool.
    DATA par_replace TYPE string.
    DATA index_left TYPE i.
    DATA index_right TYPE i.
    DATA index_operator TYPE i.
    DATA left TYPE i.
    DATA right TYPE i.

    looking_for_paranthesis = abap_true.
    WHILE looking_for_paranthesis = abap_true.
      FIND REGEX '\(([^()]*)\)' IN expression SUBMATCHES DATA(par_expression).
      IF par_expression IS INITIAL.
        looking_for_paranthesis = abap_false.
      ELSE.
        DATA(paranthesis_value) = calculate_p2( par_expression ).
        par_replace = |({ par_expression })|.
        REPLACE ALL OCCURRENCES OF par_replace IN expression WITH paranthesis_value.
      ENDIF.
    ENDWHILE.

    SPLIT expression AT | | INTO TABLE DATA(tokens).

    LOOP AT tokens ASSIGNING FIELD-SYMBOL(<token>) WHERE table_line = |+|.
      index_left = sy-tabix - 1.
      index_operator = sy-tabix.
      index_right = sy-tabix + 1.

      READ TABLE tokens INDEX index_left INTO left.
      READ TABLE tokens INDEX index_right INTO right.
      <token> = left + right.
      DELETE tokens INDEX index_right.
      DELETE tokens INDEX index_left.
    ENDLOOP.

    LOOP AT tokens ASSIGNING <token> WHERE table_line = |*|.
      index_left = sy-tabix - 1.
      index_operator = sy-tabix.
      index_right = sy-tabix + 1.

      READ TABLE tokens INDEX index_left INTO left.
      READ TABLE tokens INDEX index_right INTO right.
      <token> = left * right.
      DELETE tokens INDEX index_right.
      DELETE tokens INDEX index_left.
    ENDLOOP.

    READ TABLE tokens INDEX 1 INTO result.

  ENDMETHOD.

ENDCLASS.
