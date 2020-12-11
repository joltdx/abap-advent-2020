CLASS zcl_advent2020_day11_joltdx DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_advent2020_joltdx .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mt_original_layout TYPE STANDARD TABLE OF string WITH EMPTY KEY.
    DATA mt_layout TYPE STANDARD TABLE OF string WITH EMPTY KEY.
    DATA mv_layout_width TYPE i.
    DATA mv_layout_height TYPE i.

    METHODS part_1
      RETURNING
        VALUE(result) TYPE string.

    METHODS part_2
      RETURNING
        VALUE(result) TYPE string.

    METHODS movement_round
      RETURNING
        VALUE(changed) TYPE abap_bool.

    METHODS movement_round_2
      RETURNING
        VALUE(changed) TYPE abap_bool.

    METHODS count_occupied_neighbours
      IMPORTING
        column TYPE i
        row TYPE i
      RETURNING
        VALUE(result) TYPE i.

    METHODS count_occupied_neighbours_2
      IMPORTING
        column TYPE i
        row TYPE i
      RETURNING
        VALUE(result) TYPE i.

    METHODS occupied_in_direction
      IMPORTING
        column TYPE i
        row TYPE i
        right TYPE i
        down TYPE i
      RETURNING
        VALUE(result) TYPE i.

ENDCLASS.



CLASS ZCL_ADVENT2020_DAY11_joltdx IMPLEMENTATION.


  METHOD zif_advent2020_joltdx~solve.

    FIND FIRST OCCURRENCE OF |\n| IN input MATCH OFFSET mv_layout_width.
    mv_layout_width = mv_layout_width - 1.

    FIND ALL OCCURRENCES OF |\n| IN input MATCH COUNT mv_layout_height.
    mv_layout_height = mv_layout_height + 1.

    SPLIT input AT |\n| INTO TABLE mt_original_layout.

    output = |Part 1: { part_1( ) }. Part 2: { part_2( ) }|.

  ENDMETHOD.

  METHOD part_1.
    DATA movement TYPE abap_bool.
    DATA occupied_seats TYPE i.
    DATA occupied_this_row TYPE i.

    mt_layout = mt_original_layout.

    DO.
      movement = movement_round( ).
      IF movement = abap_false.
        EXIT.
      ENDIF.
    ENDDO.

    LOOP AT mt_layout INTO DATA(layout).
      FIND ALL OCCURRENCES OF |#| IN layout MATCH COUNT occupied_this_row.
      occupied_seats = occupied_seats + occupied_this_row.
    ENDLOOP.

    result = occupied_seats.
  ENDMETHOD.

  METHOD part_2.
    DATA movement TYPE abap_bool.
    DATA occupied_seats TYPE i.
    DATA occupied_this_row TYPE i.

    mt_layout = mt_original_layout.

    DO.
      movement = movement_round_2( ).
      IF movement = abap_false.
        EXIT.
      ENDIF.
    ENDDO.

    LOOP AT mt_layout INTO DATA(layout).
      FIND ALL OCCURRENCES OF |#| IN layout MATCH COUNT occupied_this_row.
      occupied_seats = occupied_seats + occupied_this_row.
    ENDLOOP.

    result = occupied_seats.

  ENDMETHOD.

  METHOD movement_round.
    DATA row TYPE i.
    DATA column TYPE i.
    DATA this_spot TYPE c.
    DATA change_this_spot TYPE c.
    DATA this_row TYPE string.
    DATA new_row TYPE string.
    DATA new_layout TYPE STANDARD TABLE OF string WITH EMPTY KEY.

    DO mv_layout_height TIMES.
      row = row + 1.
      READ TABLE mt_layout INDEX row INTO this_row.
      CLEAR new_row.
      DO mv_layout_width TIMES.
        column = sy-index - 1.
        this_spot = this_row+column(1).
        change_this_spot = this_spot.
        CASE this_spot.
          WHEN 'L'.
            IF count_occupied_neighbours( column = column + 1
                                          row    = row ) = 0.
              change_this_spot = |#|.
            ENDIF.
          WHEN '#'.
            IF count_occupied_neighbours( column = column + 1
                                          row    = row ) >= 4.
              change_this_spot = |L|.
            ENDIF.
        ENDCASE.
        new_row = |{ new_row }{ change_this_spot }|.
      ENDDO.
      APPEND new_row TO new_layout.
      IF new_row <> this_row.
        changed = abap_true.
      ENDIF.
    ENDDO.

    mt_layout = new_layout.
  ENDMETHOD.

  METHOD movement_round_2.
    DATA row TYPE i.
    DATA column TYPE i.
    DATA this_spot TYPE c.
    DATA change_this_spot TYPE c.
    DATA this_row TYPE string.
    DATA new_row TYPE string.
    DATA new_layout TYPE STANDARD TABLE OF string WITH EMPTY KEY.

    DO mv_layout_height TIMES.
      row = row + 1.
      READ TABLE mt_layout INDEX row INTO this_row.
      CLEAR new_row.
      DO mv_layout_width TIMES.
        column = sy-index - 1.
        this_spot = this_row+column(1).
        change_this_spot = this_spot.
        CASE this_spot.
          WHEN 'L'.
            IF count_occupied_neighbours_2( column = column + 1
                                            row    = row ) = 0.
              change_this_spot = |#|.
            ENDIF.
          WHEN '#'.
            IF count_occupied_neighbours_2( column = column + 1
                                            row    = row ) >= 5.
              change_this_spot = |L|.
            ENDIF.
        ENDCASE.
        new_row = |{ new_row }{ change_this_spot }|.
      ENDDO.
      APPEND new_row TO new_layout.
      IF new_row <> this_row.
        changed = abap_true.
      ENDIF.
    ENDDO.

    mt_layout = new_layout.
  ENDMETHOD.

  METHOD count_occupied_neighbours.
    DATA add_neighbours TYPE string.
    DATA neighbours TYPE string.
    DATA investigate_row TYPE i.
    DATA investigate_col TYPE i.
    DATA investigate_width TYPE i.
    DATA row_layout TYPE string.

    DO 2 TIMES.
      IF sy-index = 1.
        investigate_row = row - 1.
      ELSE.
        investigate_row = row + 1.
      ENDIF.
      IF investigate_row BETWEEN 1 AND mv_layout_height.
        READ TABLE mt_layout INDEX investigate_row INTO row_layout.
        IF sy-subrc = 0.
          IF column = 1.
            investigate_col = column - 1.
            investigate_width = 2.
          ELSEIF column = mv_layout_width.
            investigate_col = column - 2.
            investigate_width = 2.
          ELSE.
            investigate_col = column - 2.
            investigate_width = 3.
          ENDIF.
          add_neighbours = row_layout+investigate_col(investigate_width).
          neighbours = neighbours && add_neighbours.
        ENDIF.
      ENDIF.
    ENDDO.
    READ TABLE mt_layout INDEX row INTO row_layout.
    IF sy-subrc = 0.
      IF column > 1.
        investigate_col = column - 2.
        add_neighbours = row_layout+investigate_col(1).
        neighbours = neighbours && add_neighbours.
      ENDIF.
      IF column < mv_layout_width.
        investigate_col = column.
        add_neighbours = row_layout+investigate_col(1).
        neighbours = neighbours && add_neighbours.
      ENDIF.
    ENDIF.

    FIND ALL OCCURRENCES OF |#| IN neighbours MATCH COUNT result.

  ENDMETHOD.

  METHOD count_occupied_neighbours_2.
    result = result + occupied_in_direction( row = row
                                             column = column
                                             right = -1
                                             down = -1 ).

    result = result + occupied_in_direction( row = row
                                             column = column
                                             right = 0
                                             down = -1 ).

    result = result + occupied_in_direction( row = row
                                             column = column
                                             right = 1
                                             down = -1 ).

    result = result + occupied_in_direction( row = row
                                             column = column
                                             right = 1
                                             down = 0 ).

    result = result + occupied_in_direction( row = row
                                             column = column
                                             right = 1
                                             down = 1 ).

    result = result + occupied_in_direction( row = row
                                             column = column
                                             right = 0
                                             down = 1 ).

    result = result + occupied_in_direction( row = row
                                             column = column
                                             right = -1
                                             down = 1 ).

    result = result + occupied_in_direction( row = row
                                             column = column
                                             right = -1
                                             down = 0 ).
  ENDMETHOD.

  METHOD occupied_in_direction.
    DATA investigate_row TYPE i.
    DATA investigate_col TYPE i.
    DATA row_layout TYPE string.
    DATA check_neighbour TYPE c.
    DATA count_occupied TYPE i.

    investigate_row = row.
    investigate_col = column - 1.
    DO.
      investigate_row = investigate_row + right.
      investigate_col = investigate_col + down.

      IF investigate_row BETWEEN 1 AND mv_layout_height AND
          investigate_col BETWEEN 0 AND mv_layout_width.
        READ TABLE mt_layout INDEX investigate_row INTO row_layout.
        IF sy-subrc = 0.
          check_neighbour = row_layout+investigate_col(1).
          IF check_neighbour = |#|.
            result = 1.
            RETURN.
          ELSEIF check_neighbour = |L|.
            RETURN.
          ENDIF.
        ELSE.
          RETURN.
        ENDIF.
      ELSE.
        RETURN.
      ENDIF.
    ENDDO.

  ENDMETHOD.
ENDCLASS.
