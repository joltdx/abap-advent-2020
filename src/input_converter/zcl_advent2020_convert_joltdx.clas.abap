CLASS zcl_advent2020_convert_joltdx DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS convert_to_value_statement
      IMPORTING
        input TYPE string
      RETURNING
        VALUE(output) TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ADVENT2020_CONVERT_joltdx IMPLEMENTATION.

  METHOD convert_to_value_statement.
    SPLIT input AT |\n| INTO TABLE DATA(input_table).
    LOOP AT input_table INTO DATA(input_line).
      IF input_line IS INITIAL.
        CONTINUE.
      ENDIF.
      IF output IS INITIAL.
        output = |input = \|{ input_line }\\n\||.
      ELSE.
        output = |{ output } &&\n        \|{ input_line }\\n\||.
      ENDIF.
    ENDLOOP.
    output = output && |.|.
  ENDMETHOD.
ENDCLASS.
