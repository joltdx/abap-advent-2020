CLASS zcl_advent2020_day20_joltdx DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_advent2020_joltdx .
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_tile,
        id TYPE i,
        t TYPE xstring,
        tf TYPE xstring,
        r TYPE xstring,
        rf TYPE xstring,
        b TYPE xstring,
        bf TYPE xstring,
        l TYPE xstring,
        lf TYPE xstring,
        regex TYPE string,
        match_t TYPE i,
        match_tf TYPE i,
        match_r TYPE i,
        match_rf TYPE i,
        match_b TYPE i,
        match_bf TYPE i,
        match_l TYPE i,
        match_lf TYPE i,
      END OF ty_tile.
    TYPES ty_tile_tt TYPE STANDARD TABLE OF ty_tile WITH EMPTY KEY.
    DATA mt_tiles TYPE ty_tile_tt.
    DATA mt_corner_tiles TYPE ty_tile_tt.
    DATA mt_edge_tiles TYPE ty_tile_tt.
    DATA mt_center_tiles TYPE ty_tile_tt.

    METHODS part_1
      RETURNING
        VALUE(result) TYPE i.

    METHODS part_2
      RETURNING
        VALUE(result) TYPE string.

    METHODS parse_tile
      IMPORTING
        tile_data TYPE string.

    METHODS count_edge_matches.

    METHODS organize_tiles.

ENDCLASS.



CLASS ZCL_ADVENT2020_DAY20_joltdx IMPLEMENTATION.


  METHOD zif_advent2020_joltdx~solve.
    REPLACE ALL OCCURRENCES OF |\r| IN input WITH |\n|.
    SPLIT input AT |\n\n| INTO TABLE DATA(tiles).
    LOOP AT tiles INTO DATA(tile).
      parse_tile( tile ).
    ENDLOOP.
    SORT mt_tiles BY id.
    count_edge_matches( ).
    organize_tiles( ).

    output = |Part 1: { part_1( ) }\nPart 2: { part_2( ) }|.

  ENDMETHOD.

  METHOD part_1.
    result = 1.
    LOOP AT mt_corner_tiles INTO DATA(tile).
      result = result * tile-id.
    ENDLOOP.
  ENDMETHOD.

  METHOD part_2.
    result = |todo|.
  ENDMETHOD.

  METHOD parse_tile.
    DATA tile TYPE ty_tile.
    DATA offset TYPE i.
    DATA c TYPE c.
    DATA bit TYPE i.
    tile = VALUE #( t = '0000'
                    tf = '0000'
                    r = '0000'
                    rf = '0000'
                    b = '0000'
                    bf = '0000'
                    l = '0000'
                    lf = '0000' ).

    SPLIT tile_data AT |\n| INTO TABLE DATA(lines).
    LOOP AT lines INTO DATA(line).
      IF sy-tabix = 1.
        FIND REGEX 'Tile (\d+):' IN line SUBMATCHES tile-id.
      ELSEIF sy-tabix = 2.
        DO 10 TIMES.
          offset = sy-index - 1.
          c = line+offset(1).
          IF c = '#'.
            bit = offset + 7.
            SET BIT bit OF tile-t TO 1.
            bit = 16 - offset.
            SET BIT bit OF tile-tf TO 1.
          ENDIF.
        ENDDO.
        c = line+0(1).
        IF c = '#'.
          SET BIT 7 OF tile-l TO 1.
          SET BIT 16 OF tile-lf TO 1.
        ENDIF.
        c = line+9(1).
        IF c = '#'.
          SET BIT 7 OF tile-r TO 1.
          SET BIT 16 OF tile-rf TO 1.
        ENDIF.
      ELSEIF sy-tabix = 11.
        DO 10 TIMES.
          offset = sy-index - 1.
          c = line+offset(1).
          IF c = '#'.
            bit = offset + 7.
            SET BIT bit OF tile-b TO 1.
            bit = 16 - offset.
            SET BIT bit OF tile-bf TO 1.
          ENDIF.
        ENDDO.
        c = line+0(1).
        IF c = '#'.
          SET BIT 16 OF tile-l TO 1.
          SET BIT 7 OF tile-lf TO 1.
        ENDIF.
        c = line+9(1).
        IF c = '#'.
          SET BIT 16 OF tile-r TO 1.
          SET BIT 7 OF tile-rf TO 1.
        ENDIF.
      ELSE.
        c = line+0(1).
        IF c = '#'.
          SET BIT 5 + sy-tabix OF tile-l TO 1.
          SET BIT 18 - sy-tabix OF tile-lf TO 1.
        ENDIF.
        c = line+9(1).
        IF c = '#'.
          SET BIT 5 + sy-tabix OF tile-r TO 1.
          SET BIT 18 - sy-tabix OF tile-rf TO 1.
        ENDIF.
      ENDIF.
    ENDLOOP.

    tile-regex = |({ tile-t })\|({ tile-tf })\|| &&
                 |({ tile-r })\|({ tile-rf })\|| &&
                 |({ tile-b })\|({ tile-bf })\|| &&
                 |({ tile-l })\|({ tile-lf })|.
    APPEND tile TO mt_tiles.
  ENDMETHOD.

  METHOD count_edge_matches.
    LOOP AT mt_tiles ASSIGNING FIELD-SYMBOL(<first>).
      LOOP AT mt_tiles INTO DATA(second) WHERE id <> <first>-id.
        FIND REGEX <first>-t IN second-regex.
        IF sy-subrc = 0.
          <first>-match_t = <first>-match_t + 1.
        ENDIF.
        FIND REGEX <first>-tf IN second-regex.
        IF sy-subrc = 0.
          <first>-match_tf = <first>-match_tf + 1.
        ENDIF.
        FIND REGEX <first>-r IN second-regex.
        IF sy-subrc = 0.
          <first>-match_r = <first>-match_r + 1.
        ENDIF.
        FIND REGEX <first>-rf IN second-regex.
        IF sy-subrc = 0.
          <first>-match_rf = <first>-match_rf + 1.
        ENDIF.
        FIND REGEX <first>-b IN second-regex.
        IF sy-subrc = 0.
          <first>-match_b = <first>-match_b + 1.
        ENDIF.
        FIND REGEX <first>-bf IN second-regex.
        IF sy-subrc = 0.
          <first>-match_bf = <first>-match_bf + 1.
        ENDIF.
        FIND REGEX <first>-l IN second-regex.
        IF sy-subrc = 0.
          <first>-match_l = <first>-match_l + 1.
        ENDIF.
        FIND REGEX <first>-tf IN second-regex.
        IF sy-subrc = 0.
          <first>-match_lf = <first>-match_lf + 1.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD organize_tiles.
    DATA count_zero TYPE i.

    CLEAR mt_center_tiles.
    CLEAR mt_edge_tiles.
    CLEAR mt_corner_tiles.
    LOOP AT mt_tiles INTO DATA(tile).
      count_zero = 0.
      IF tile-match_t = 0 AND tile-match_tf = 0.
        count_zero = count_zero + 1.
      ENDIF.
      IF tile-match_r = 0 AND tile-match_rf = 0.
        count_zero = count_zero + 1.
      ENDIF.
      IF tile-match_b = 0 AND tile-match_bf = 0.
        count_zero = count_zero + 1.
      ENDIF.
      IF tile-match_l = 0 AND tile-match_lf = 0.
        count_zero = count_zero + 1.
      ENDIF.

      CASE count_zero.
        WHEN 0.
          APPEND tile TO mt_center_tiles.
        WHEN 1.
          APPEND tile TO mt_edge_tiles.
        WHEN 2.
          APPEND tile TO mt_corner_tiles.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
