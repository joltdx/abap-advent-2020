CLASS zcl_advent2020_day24_joltdx DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_advent2020_joltdx .
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_tile,
        e TYPE i,
        se TYPE i,
        flip TYPE abap_bool,
      END OF ty_tile.

    DATA mt_input TYPE STANDARD TABLE OF string.
    DATA mt_tiles TYPE STANDARD TABLE OF ty_tile WITH EMPTY KEY.
    DATA mt_tiles_wip TYPE STANDARD TABLE OF ty_tile WITH EMPTY KEY.

    METHODS part_1
      RETURNING
        VALUE(result) TYPE string.

    METHODS part_2
      RETURNING
        VALUE(result) TYPE string.

    METHODS simplify_identification
      IMPORTING
        input TYPE string
      EXPORTING
        e TYPE i
        se TYPE i.

    METHODS count_adjacent_flipped
      IMPORTING
        tile TYPE ty_tile
        flip TYPE abap_bool
      RETURNING
        VALUE(result) TYPE i.
ENDCLASS.



CLASS ZCL_ADVENT2020_DAY24_joltdx IMPLEMENTATION.

  METHOD zif_advent2020_joltdx~solve.
    REPLACE ALL OCCURRENCES OF |\r| IN input WITH ||.
    SPLIT input AT |\n| INTO TABLE mt_input.
    output = |Part 1:\n{ part_1( ) }\n\nPart 2:\n{ part_2( ) }|.

  ENDMETHOD.

  METHOD part_1.
    DATA e TYPE i.
    DATA se TYPE i.
    DATA count TYPE i.

    LOOP AT mt_input INTO DATA(input).
      simplify_identification( EXPORTING input = input
                               IMPORTING e = e
                                         se = se ).
      READ TABLE mt_tiles WITH KEY e = e se = se ASSIGNING FIELD-SYMBOL(<tile>).
      IF sy-subrc = 0.
        IF <tile>-flip = abap_false.
          <tile>-flip = abap_true.
        ELSE.
          <tile>-flip = abap_false.
        ENDIF.
      ELSE.
        APPEND INITIAL LINE TO mt_tiles ASSIGNING FIELD-SYMBOL(<new_tile>).
        <new_tile>-e = e.
        <new_tile>-se = se.
        <new_tile>-flip = abap_true.
      ENDIF.
    ENDLOOP.

    LOOP AT mt_tiles WHERE flip = abap_true TRANSPORTING NO FIELDS.
      count = count + 1.
    ENDLOOP.

    result = count.
  ENDMETHOD.

  METHOD part_2.
    DATA adjacent_count TYPE i.
    DATA count TYPE i.

    DO 100 TIMES.
      SORT mt_tiles BY flip DESCENDING.
      mt_tiles_wip = mt_tiles.

      LOOP AT mt_tiles_wip ASSIGNING FIELD-SYMBOL(<tile>).
        adjacent_count = count_adjacent_flipped( tile = <tile> ).
        IF <tile>-flip = abap_true AND ( adjacent_count = 0 OR adjacent_count > 2 ).
          <tile>-flip = abap_false.
        ELSEIF <tile>-flip = abap_false AND adjacent_count = 2.
          <tile>-flip = abap_true.
        ENDIF.
      ENDLOOP.

      mt_tiles = mt_tiles_wip.
    ENDDO.

    count = 0.
    LOOP AT mt_tiles WHERE flip = abap_true TRANSPORTING NO FIELDS.
      count = count + 1.
    ENDLOOP.
    result = |Day 100: { count }\n|.
  ENDMETHOD.

  METHOD simplify_identification.
    DATA count_e TYPE i.
    DATA count_se TYPE i.
    DATA count_sw TYPE i.
    DATA count_w TYPE i.
    DATA count_nw TYPE i.
    DATA count_ne TYPE i.

    FIND REGEX '(e)' IN input MATCH COUNT count_e.
    FIND REGEX '(se)' IN input MATCH COUNT count_se.
    FIND REGEX '(sw)' IN input MATCH COUNT count_sw.
    FIND REGEX '(w)' IN input MATCH COUNT count_w.
    FIND REGEX '(nw)' IN input MATCH COUNT count_nw.
    FIND REGEX '(ne)' IN input MATCH COUNT count_ne.

    count_e = count_e - count_se - count_ne.
    count_w = count_w - count_sw - count_nw.

    DATA move_e TYPE i.
    DATA move_se TYPE i.
    move_e = count_e - count_w.
    move_se = count_se - count_nw.

    move_e = move_e + count_ne - count_sw.
    move_se = move_se - count_ne + count_sw.

    e = move_e.
    se = move_se.
  ENDMETHOD.

  METHOD count_adjacent_flipped.
    DATA adjacent TYPE ty_tile.
    DATA coordinates TYPE STANDARD TABLE OF ty_tile.
    DATA new_tile TYPE ty_tile.

    adjacent-e = tile-e.
    adjacent-se = tile-se - 1.
    APPEND adjacent TO coordinates.
    adjacent-e = adjacent-e + 1.
    APPEND adjacent TO coordinates.
    adjacent-se = adjacent-se + 1.
    APPEND adjacent TO coordinates.
    adjacent-e = adjacent-e - 2.
    APPEND adjacent TO coordinates.
    adjacent-se = adjacent-se + 1.
    APPEND adjacent TO coordinates.
    adjacent-e = adjacent-e + 1.
    APPEND adjacent TO coordinates.

    LOOP AT coordinates INTO DATA(coords).
      READ TABLE mt_tiles WITH KEY e = coords-e se = coords-se INTO adjacent.
      IF sy-subrc <> 0.
        IF tile-flip = abap_true.
          new_tile-e = coords-e.
          new_tile-se = coords-se.
          APPEND new_tile TO mt_tiles.
          APPEND new_tile TO mt_tiles_wip.
        ENDIF.
      ELSEIF adjacent-flip = abap_true.
        result = result + 1.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
