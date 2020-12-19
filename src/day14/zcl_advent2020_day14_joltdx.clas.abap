CLASS zcl_advent2020_day14_joltdx DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_advent2020_joltdx .
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_initialization,
        address TYPE i,
        value TYPE i,
      END OF ty_initialization.

    DATA mt_input TYPE STANDARD TABLE OF string.
    DATA mt_initialization TYPE STANDARD TABLE OF ty_initialization WITH EMPTY KEY.
    DATA mt_initialization_v2 TYPE STANDARD TABLE OF ty_initialization WITH EMPTY KEY.
    DATA mv_or_mask TYPE xstring.
    DATA mv_and_mask TYPE xstring.
    DATA mv_floating_and TYPE xstring.
    DATA mt_floating_or TYPE STANDARD TABLE OF xstring.

    METHODS part_1
      RETURNING
        VALUE(result) TYPE string.

    METHODS part_2
      RETURNING
        VALUE(result) TYPE string.

    METHODS set_mask
      IMPORTING
        mask TYPE string.

    METHODS set_mem
      IMPORTING
        address TYPE i
        value TYPE i.

    METHODS set_mem_v2
      IMPORTING
        address TYPE i
        value TYPE i.
ENDCLASS.



CLASS ZCL_ADVENT2020_DAY14_joltdx IMPLEMENTATION.


  METHOD zif_advent2020_joltdx~solve.
    REPLACE ALL OCCURRENCES OF |\r| IN input WITH ||.
    SPLIT input AT |\n| INTO TABLE mt_input.
    output = |Part 1: { part_1( ) }\nPart 2: { part_2( ) }|.
  ENDMETHOD.

  METHOD part_1.
    DATA mask TYPE string.
    DATA address TYPE i.
    DATA value TYPE i.
    DATA sum TYPE i.
    LOOP AT mt_input INTO DATA(input).
      FIND REGEX 'mask = ([X10]*)' IN input SUBMATCHES mask.
      IF sy-subrc = 0.
        set_mask( mask ).
      ELSE.
        FIND REGEX 'mem\[(\d+)\] = (\d+)' IN input SUBMATCHES address value.
        set_mem( address = address
                 value = value ).
      ENDIF.
    ENDLOOP.
    LOOP AT mt_initialization INTO DATA(init).
      sum = sum + init-value.
    ENDLOOP.
    result = sum.
  ENDMETHOD.

  METHOD part_2.
    DATA mask TYPE string.
    DATA address TYPE i.
    DATA value TYPE i.
    DATA sum TYPE i.
    LOOP AT mt_input INTO DATA(input).
      FIND REGEX 'mask = ([X10]*)' IN input SUBMATCHES mask.
      IF sy-subrc = 0.
        set_mask( mask ).
      ELSE.
        FIND REGEX 'mem\[(\d+)\] = (\d+)' IN input SUBMATCHES address value.
        set_mem_v2( address = address
                    value = value ).
      ENDIF.
    ENDLOOP.
    LOOP AT mt_initialization_v2 INTO DATA(init).
      sum = sum + init-value.
    ENDLOOP.
    result = sum.
  ENDMETHOD.

  METHOD set_mask.
    DATA bit_status TYPE c.
    DATA bit_offset TYPE i.
    DATA bit_number TYPE i.
    DATA floating_mask TYPE xstring.
    mv_or_mask = '0000000000'.
    mv_and_mask = '0FFFFFFFFF'.
    mv_floating_and = '0FFFFFFFFF'.
    CLEAR mt_floating_or[].
    DO 36 TIMES.
      bit_offset = sy-index - 1.
      bit_status = mask+bit_offset(1).
      bit_number = sy-index + 4.
      IF bit_status = '1'.
        SET BIT bit_number OF mv_or_mask TO 1.
      ELSEIF bit_status = '0'.
        SET BIT bit_number OF mv_and_mask TO 0.
      ELSEIF bit_status = 'X'.
        IF lines( mt_floating_or ) = 0.
          floating_mask = '0000000000'.
          APPEND floating_mask TO mt_floating_or.
          SET BIT bit_number OF floating_mask TO 1.
          APPEND floating_mask TO mt_floating_or.
        ELSE.
          DATA(tmp_float_or) = mt_floating_or.
          LOOP AT tmp_float_or INTO DATA(float_or).
            floating_mask = float_or.
            SET BIT bit_number OF floating_mask TO 1.
            APPEND floating_mask TO mt_floating_or.
          ENDLOOP.
        ENDIF.
        SET BIT bit_number OF mv_floating_and TO 0.
      ENDIF.
    ENDDO.
    IF lines( mt_floating_or ) = 0.
      floating_mask = '0000000000'.
      APPEND floating_mask TO mt_floating_or.
    ENDIF.
  ENDMETHOD.

  METHOD set_mem.
    DATA hex_value TYPE xstring.
    hex_value = value.
    DO 5 - ( strlen( hex_value ) / 2 ) TIMES.
      hex_value = |00{ hex_value }|.
    ENDDO.
    READ TABLE mt_initialization WITH KEY address = address ASSIGNING FIELD-SYMBOL(<mem>).
    IF sy-subrc <> 0.
      APPEND INITIAL LINE TO mt_initialization ASSIGNING <mem>.
      <mem>-address = address.
    ENDIF.
    hex_value = ( hex_value BIT-OR mv_or_mask ) BIT-AND mv_and_mask.
    <mem>-value = hex_value.
  ENDMETHOD.

  METHOD set_mem_v2.
    DATA hex_address TYPE xstring.
    DATA floating_address_and TYPE xstring.
    DATA floating_address TYPE xstring.
    DATA int_address TYPE i.
    hex_address = address.
    DO 5 - ( strlen( hex_address ) / 2 ) TIMES.
      hex_address = |00{ hex_address }|.
    ENDDO.
    hex_address = ( hex_address BIT-OR mv_or_mask ) BIT-AND mv_floating_and.
    LOOP AT mt_floating_or INTO DATA(floating_or).
      floating_address = hex_address BIT-OR floating_or.
      int_address = floating_address.
      READ TABLE mt_initialization_v2 WITH KEY address = int_address ASSIGNING FIELD-SYMBOL(<mem>).
      IF sy-subrc <> 0.
        APPEND INITIAL LINE TO mt_initialization_v2 ASSIGNING <mem>.
        <mem>-address = int_address.
      ENDIF.
      <mem>-value = value.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
