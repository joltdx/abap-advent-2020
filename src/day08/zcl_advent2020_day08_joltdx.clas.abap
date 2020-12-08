CLASS zcl_advent2020_day08_joltdx DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_advent2020_joltdx .
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_boot_code,
        line TYPE i,
        operation TYPE string,
        argument TYPE i,
        line_visited TYPE abap_bool,
      END OF ty_boot_code.

    DATA mt_boot_code TYPE STANDARD TABLE OF ty_boot_code.
    DATA mt_lines_of_boot_code TYPE i.

    METHODS parse_input
      IMPORTING
        input TYPE string.

    METHODS run_until_inf_loop
      EXPORTING
        VALUE(result) TYPE i
        VALUE(completed) TYPE abap_bool.

    METHODS clear_visited.

    METHODS part_1
      RETURNING
        VALUE(result) TYPE string.

    METHODS part_2
      RETURNING
        VALUE(result) TYPE string.
ENDCLASS.



CLASS ZCL_ADVENT2020_DAY08_joltdx IMPLEMENTATION.


  METHOD zif_advent2020_joltdx~solve.

    parse_input( input ).

    output = |Part 1: { part_1( ) }. Part 2: { part_2( ) }|.

  ENDMETHOD.

  METHOD parse_input.
    DATA lt_input TYPE STANDARD TABLE OF string WITH EMPTY KEY.
    DATA boot_code_line TYPE ty_boot_code.
    DATA sign TYPE c.

    SPLIT input AT |\n| INTO TABLE lt_input.

    LOOP AT lt_input INTO DATA(line).
      boot_code_line-line = sy-tabix.
      FIND REGEX '(\w+) (\+|-)(\d+)' IN line SUBMATCHES boot_code_line-operation sign boot_code_line-argument.
      IF sign = '-'.
        boot_code_line-argument = 0 - boot_code_line-argument.
      ENDIF.
      APPEND boot_code_line TO mt_boot_code.
    ENDLOOP.

    mt_lines_of_boot_code = lines( mt_boot_code ).
  ENDMETHOD.

  METHOD run_until_inf_loop.
    DATA instruction TYPE i.
    DATA accumulator TYPE i.

    instruction = 1.
    DO.
      READ TABLE mt_boot_code INDEX instruction ASSIGNING FIELD-SYMBOL(<line>).
      IF sy-subrc <> 0.
        result = accumulator.
        IF instruction = mt_lines_of_boot_code + 1.
          completed = abap_true.
        ENDIF.
        RETURN.
      ENDIF.

      IF <line>-line_visited = abap_true.
        result = accumulator.
        RETURN.
      ELSE.
        <line>-line_visited = abap_true.
      ENDIF.

      CASE <line>-operation.
        WHEN 'acc'.
          accumulator = accumulator + <line>-argument.
          instruction = instruction + 1.
        WHEN 'jmp'.
          instruction = instruction + <line>-argument.
        WHEN OTHERS.
          instruction = instruction + 1.
      ENDCASE.
    ENDDO.
  ENDMETHOD.

  METHOD clear_visited.
    LOOP AT mt_boot_code ASSIGNING FIELD-SYMBOL(<boot_code>).
      CLEAR <boot_code>-line_visited.
    ENDLOOP.
  ENDMETHOD.

  METHOD part_1.

    run_until_inf_loop( IMPORTING result = result ).

  ENDMETHOD.

  METHOD part_2.
    DATA completed TYPE abap_bool.

    LOOP AT mt_boot_code ASSIGNING FIELD-SYMBOL(<boot_code>).
      IF <boot_code>-operation = 'jmp'.
        <boot_code>-operation = 'nop'.
      ELSEIF <boot_code>-operation = 'nop'.
        <boot_code>-operation = 'jmp'.
      ELSE.
        CONTINUE.
      ENDIF.

      run_until_inf_loop( IMPORTING result = result
                                    completed = completed ).
      IF completed = abap_true.
        RETURN.
      ENDIF.

      IF <boot_code>-operation = 'jmp'.
        <boot_code>-operation = 'nop'.
      ELSEIF <boot_code>-operation = 'nop'.
        <boot_code>-operation = 'jmp'.
      ENDIF.
      clear_visited( ).
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
