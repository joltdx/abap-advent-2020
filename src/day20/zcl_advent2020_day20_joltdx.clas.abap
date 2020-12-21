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
        content TYPE string,
        placed TYPE abap_bool,
      END OF ty_tile.
    TYPES ty_tile_tt TYPE STANDARD TABLE OF ty_tile WITH EMPTY KEY.

    CONSTANTS mc_sea_monster_regex TYPE string VALUE '#.{77}#.{4}##.{4}##.{4}###.{77}#.{2}#.{2}#.{2}#.{2}#.{2}#'.

    DATA mt_tiles TYPE ty_tile_tt.
    DATA mt_corner_tiles TYPE ty_tile_tt.
    DATA mt_edge_tiles TYPE ty_tile_tt.
    DATA mt_center_tiles TYPE ty_tile_tt.
    DATA mt_ordered_tiles TYPE ty_tile_tt.
    DATA mv_image TYPE string.

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

    METHODS align_right
      IMPORTING
        tile TYPE ty_tile
        left TYPE xstring
      RETURNING
        VALUE(result) TYPE ty_tile.

    METHODS align_below
      IMPORTING
        tile TYPE ty_tile
        top TYPE xstring
      RETURNING
        VALUE(result) TYPE ty_tile.

    METHODS rotate
      CHANGING
        tile TYPE ty_tile.

    METHODS flip
      CHANGING
        tile TYPE ty_tile.

    METHODS rotate_string
      IMPORTING
        width TYPE i
      CHANGING
        content TYPE string.

    METHODS flip_string
      IMPORTING
        width TYPE i
      CHANGING
        content TYPE string.

    METHODS order_tiles.

    METHODS generate_image.

    METHODS layout_for_sea_monsters.

    METHODS highlight_sea_monsters.

    METHODS print_image
      RETURNING
        VALUE(result) TYPE string.

    METHODS estimate_roughness
      RETURNING
        VALUE(result) TYPE string.
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

    output = |Part 1:\n{ part_1( ) }\n\nPart 2:\n{ part_2( ) }|.

  ENDMETHOD.

  METHOD part_1.
    result = 1.
    LOOP AT mt_corner_tiles INTO DATA(tile).
      result = result * tile-id.
    ENDLOOP.
  ENDMETHOD.

  METHOD part_2.
    order_tiles( ).
    generate_image( ).
    layout_for_sea_monsters( ).
    highlight_sea_monsters( ).
    result = |{ print_image( ) }\n{ estimate_roughness( ) }|.
  ENDMETHOD.

  METHOD parse_tile.
    DATA tile TYPE ty_tile.
    DATA offset TYPE i.
    DATA c TYPE c.
    DATA bit TYPE i.
    DATA content TYPE string.

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
        content = line+1(8).
        tile-content = |{ tile-content }{ content }|.
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
        FIND REGEX <first>-lf IN second-regex.
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

  METHOD align_right.
    result = tile.

    IF tile-t = left.
      flip( CHANGING tile = result ).
      rotate( CHANGING tile = result ).
      rotate( CHANGING tile = result ).
      rotate( CHANGING tile = result ).
    ELSEIF tile-r = left.
      flip( CHANGING tile = result ).
    ELSEIF tile-b = left.
      rotate( CHANGING tile = result ).
    ELSEIF tile-l = left.
      RETURN.
    ELSEIF tile-tf = left.
      rotate( CHANGING tile = result ).
      rotate( CHANGING tile = result ).
      rotate( CHANGING tile = result ).
    ELSEIF tile-rf = left.
      rotate( CHANGING tile = result ).
      rotate( CHANGING tile = result ).
    ELSEIF tile-bf = left.
      flip( CHANGING tile = result ).
      rotate( CHANGING tile = result ).
    ELSEIF tile-lf = left.
      flip( CHANGING tile = result ).
      rotate( CHANGING tile = result ).
      rotate( CHANGING tile = result ).
    ENDIF.
  ENDMETHOD.

  METHOD align_below.
    result = tile.

    IF tile-t = top.
      RETURN.
    ELSEIF tile-r = top.
      rotate( CHANGING tile = result ).
      rotate( CHANGING tile = result ).
      rotate( CHANGING tile = result ).
    ELSEIF tile-b = top.
      flip( CHANGING tile = result ).
      rotate( CHANGING tile = result ).
      rotate( CHANGING tile = result ).
    ELSEIF tile-l = top.
      rotate( CHANGING tile = result ).
      flip( CHANGING tile = result ).
    ELSEIF tile-bf = top.
      rotate( CHANGING tile = result ).
      rotate( CHANGING tile = result ).
    ELSEIF tile-lf = top.
      rotate( CHANGING tile = result ).
    ELSEIF tile-rf = top.
      rotate( CHANGING tile = result ).
      rotate( CHANGING tile = result ).
      rotate( CHANGING tile = result ).
      flip( CHANGING tile = result ).
    ELSEIF tile-tf = top.
      flip( CHANGING tile = result ).
    ENDIF.
  ENDMETHOD.

  METHOD rotate.
    DATA(old_tile) = tile.

    tile-r = old_tile-t.
    tile-t = old_tile-lf.
    tile-l = old_tile-b.
    tile-b = old_tile-rf.

    tile-rf = old_tile-tf.
    tile-tf = old_tile-l.
    tile-lf = old_tile-bf.
    tile-bf = old_tile-r.

    tile-match_r = old_tile-match_t.
    tile-match_t = old_tile-match_lf.
    tile-match_l = old_tile-match_b.
    tile-match_b = old_tile-match_rf.

    tile-match_rf = old_tile-match_tf.
    tile-match_tf = old_tile-match_l.
    tile-match_lf = old_tile-match_bf.
    tile-match_bf = old_tile-match_r.

    rotate_string( EXPORTING width = 8
                   CHANGING content = tile-content ).

  ENDMETHOD.

  METHOD flip.
    DATA(old_tile) = tile.

    tile-t = old_tile-tf.
    tile-tf = old_tile-t.

    tile-b = old_tile-bf.
    tile-bf = old_tile-b.

    tile-l = old_tile-r.
    tile-r = old_tile-l.

    tile-lf = old_tile-rf.
    tile-rf = old_tile-lf.

    flip_string( EXPORTING width = 8
                 CHANGING content = tile-content ).
  ENDMETHOD.

  METHOD rotate_string.
    DATA offset1 TYPE i.
    DATA offset2 TYPE i.
    DATA rowpos TYPE i.
    DATA row TYPE i.
    DATA(cont) = content.
    DO strlen( cont ) TIMES.
      offset1 = sy-index - 1.
      rowpos = offset1 MOD width.
      row = offset1 DIV width.
      offset2 = ( ( ( ( width - rowpos ) * width ) - ( width - rowpos ) ) - rowpos ) + row.
      content+offset1(1) = cont+offset2(1).
    ENDDO.
  ENDMETHOD.

  METHOD flip_string.
    DATA offset1 TYPE i.
    DATA offset2 TYPE i.
    DATA rowpos TYPE i.
    DATA row TYPE i.
    DATA(cont) = content.
    DO strlen( cont ) TIMES.
      offset1 = sy-index - 1.
      rowpos = offset1 MOD width.
      row = offset1 DIV width.
      offset2 = ( row * width ) + ( width - rowpos ) - 1.
      content+offset1(1) = cont+offset2(1).
    ENDDO.
  ENDMETHOD.

  METHOD order_tiles.
    DATA find_from TYPE ty_tile.

    READ TABLE mt_corner_tiles INDEX 1 ASSIGNING FIELD-SYMBOL(<top_left>).
    WHILE NOT ( <top_left>-match_t = 0 AND
                <top_left>-match_tf = 0 AND
                <top_left>-match_l = 0 AND
                <top_left>-match_lf = 0 ).
      rotate( CHANGING tile = <top_left> ).
    ENDWHILE.
    INSERT <top_left> INTO TABLE mt_ordered_tiles.
    <top_left>-placed = abap_true.

    DO 10 TIMES.
      READ TABLE mt_ordered_tiles INDEX lines( mt_ordered_tiles ) INTO find_from.
      LOOP AT mt_edge_tiles ASSIGNING FIELD-SYMBOL(<edge_tile>) WHERE placed = abap_false.
        FIND REGEX <edge_tile>-regex IN find_from-r.
        IF sy-subrc = 0.
          INSERT align_right( tile = <edge_tile>
                              left = find_from-r ) INTO TABLE mt_ordered_tiles.
          <edge_tile>-placed = abap_true.
        ENDIF.
      ENDLOOP.
    ENDDO.

    READ TABLE mt_ordered_tiles INDEX lines( mt_ordered_tiles ) INTO find_from.
    LOOP AT mt_corner_tiles ASSIGNING FIELD-SYMBOL(<corner_tile>) WHERE placed = abap_false.
      FIND REGEX <corner_tile>-regex IN find_from-r.
      IF sy-subrc = 0.
        INSERT align_right( tile = <corner_tile>
                            left = find_from-r ) INTO TABLE mt_ordered_tiles.
        <corner_tile>-placed = abap_true.
      ENDIF.
    ENDLOOP.

    DO 11 TIMES.
      READ TABLE mt_ordered_tiles INDEX lines( mt_ordered_tiles ) - 11 INTO find_from.
      LOOP AT mt_edge_tiles ASSIGNING <edge_tile> WHERE placed = abap_false.
        FIND REGEX <edge_tile>-regex IN find_from-b.
        IF sy-subrc = 0.
          INSERT align_below( tile = <edge_tile>
                              top = find_from-b ) INTO TABLE mt_ordered_tiles.
          <edge_tile>-placed = abap_true.
        ENDIF.
      ENDLOOP.

      DO 10 TIMES.
        READ TABLE mt_ordered_tiles INDEX lines( mt_ordered_tiles ) INTO find_from.
        LOOP AT mt_center_tiles ASSIGNING FIELD-SYMBOL(<center_tile>) WHERE placed = abap_false.
          FIND REGEX <center_tile>-regex IN find_from-r.
          IF sy-subrc = 0.
            INSERT align_right( tile = <center_tile>
                                left = find_from-r ) INTO TABLE mt_ordered_tiles.
            <center_tile>-placed = abap_true.
          ENDIF.
        ENDLOOP.
      ENDDO.

      READ TABLE mt_ordered_tiles INDEX lines( mt_ordered_tiles ) INTO find_from.
      LOOP AT mt_edge_tiles ASSIGNING <edge_tile> WHERE placed = abap_false.
        FIND REGEX <edge_tile>-regex IN find_from-r.
        IF sy-subrc = 0.
          INSERT align_right( tile = <edge_tile>
                              left = find_from-r ) INTO TABLE mt_ordered_tiles.
          <edge_tile>-placed = abap_true.
        ENDIF.
      ENDLOOP.
    ENDDO.

    READ TABLE mt_ordered_tiles INDEX lines( mt_ordered_tiles ) - 11 INTO find_from.
    LOOP AT mt_corner_tiles ASSIGNING <corner_tile> WHERE placed = abap_false.
      FIND REGEX <corner_tile>-regex IN find_from-b.
      IF sy-subrc = 0.
        INSERT align_below( tile = <corner_tile>
                            top = find_from-b ) INTO TABLE mt_ordered_tiles.
        <corner_tile>-placed = abap_true.
      ENDIF.
    ENDLOOP.

    DO 10 TIMES.
      READ TABLE mt_ordered_tiles INDEX lines( mt_ordered_tiles ) INTO find_from.
      LOOP AT mt_edge_tiles ASSIGNING <edge_tile> WHERE placed = abap_false.
        FIND REGEX <edge_tile>-regex IN find_from-r.
        IF sy-subrc = 0.
          INSERT align_right( tile = <edge_tile>
                              left = find_from-r ) INTO TABLE mt_ordered_tiles.
          <edge_tile>-placed = abap_true.
        ENDIF.
      ENDLOOP.
    ENDDO.

    READ TABLE mt_ordered_tiles INDEX lines( mt_ordered_tiles ) INTO find_from.
    LOOP AT mt_corner_tiles ASSIGNING <corner_tile> WHERE placed = abap_false.
      FIND REGEX <corner_tile>-regex IN find_from-r.
      IF sy-subrc = 0.
        INSERT align_right( tile = <corner_tile>
                            left = find_from-r ) INTO TABLE mt_ordered_tiles.
        <corner_tile>-placed = abap_true.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD generate_image.
    DATA merged_lines TYPE STANDARD TABLE OF string WITH EMPTY KEY.
    DATA offset TYPE i.
    DATA line TYPE string.

    DO 8 TIMES.
      APPEND || TO merged_lines.
    ENDDO.

    LOOP AT mt_ordered_tiles INTO DATA(tile).
      DATA(tile_index) = sy-tabix.
      DO 8 TIMES.
        offset = ( sy-index - 1 ) * 8.
        line = tile-content+offset(8).
        READ TABLE merged_lines INDEX sy-index ASSIGNING FIELD-SYMBOL(<merged>).
        <merged> = |{ <merged> }{ line }|.
      ENDDO.
      IF tile_index MOD 12 = 0.
        DO 8 TIMES.
          READ TABLE merged_lines INDEX sy-index ASSIGNING <merged>.
          mv_image = |{ mv_image }{ <merged> }|.
          <merged> = ||.
        ENDDO.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD layout_for_sea_monsters.
    DO 4 TIMES.
      FIND REGEX mc_sea_monster_regex IN mv_image.
      IF sy-subrc = 0.
        RETURN.
      ELSE.
        rotate_string( EXPORTING width = 96
                       CHANGING content = mv_image ).
      ENDIF.
    ENDDO.
    flip_string( EXPORTING width = 96
                 CHANGING content = mv_image ).
    DO 4 TIMES.
      FIND REGEX mc_sea_monster_regex IN mv_image.
      IF sy-subrc = 0.
        RETURN.
      ELSE.
        rotate_string( EXPORTING width = 96
                       CHANGING content = mv_image ).
      ENDIF.
    ENDDO.
  ENDMETHOD.

  METHOD highlight_sea_monsters.
    DATA match_offset TYPE i.
    DATA offset TYPE i.

    DO.
      FIND REGEX mc_sea_monster_regex IN mv_image MATCH OFFSET match_offset.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
      offset = match_offset.
      mv_image+offset(1) = |O|.
      offset = offset + 78.
      mv_image+offset(1) = |O|.
      offset = offset + 5.
      mv_image+offset(2) = |OO|.
      offset = offset + 6.
      mv_image+offset(2) = |OO|.
      offset = offset + 6.
      mv_image+offset(3) = |OOO|.
      offset = offset + 80.
      mv_image+offset(1) = |O|.
      offset = offset + 3.
      mv_image+offset(1) = |O|.
      offset = offset + 3.
      mv_image+offset(1) = |O|.
      offset = offset + 3.
      mv_image+offset(1) = |O|.
      offset = offset + 3.
      mv_image+offset(1) = |O|.
      offset = offset + 3.
      mv_image+offset(1) = |O|.
    ENDDO.
  ENDMETHOD.

  METHOD print_image.
    DATA size TYPE i VALUE 96.
    DATA offset TYPE i.
    DATA line TYPE string.

    DO size TIMES.
      offset = ( sy-index - 1 ) * size.
      line = mv_image+offset(size).
      result = |{ result }{ line }\n|.
    ENDDO.
  ENDMETHOD.

  METHOD estimate_roughness.
    DATA roughness TYPE i.
    DATA sea_monster TYPE i.
    FIND |#| IN mv_image MATCH COUNT roughness.
    FIND |O| IN mv_image MATCH COUNT sea_monster.

    result = |Roughness of habitat: { roughness }\nSpace occupied by sea monsters: { sea_monster }|.

  ENDMETHOD.

ENDCLASS.
