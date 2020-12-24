CLASS zcl_advent2020_day23_joltdx DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_advent2020_joltdx .
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_crab_cups,
        cup TYPE i,
        ref TYPE REF TO zcl_advent2020_cup23_joltdx,
      END OF ty_crab_cups.

    DATA mt_crab_cups TYPE TABLE OF ty_crab_cups.

    METHODS part_1
      RETURNING
        VALUE(result) TYPE string.

    METHODS part_2
      RETURNING
        VALUE(result) TYPE string.

    METHODS new_cup
      IMPORTING
        cup TYPE i
      RETURNING
        VALUE(result) TYPE REF TO zcl_advent2020_cup23_joltdx.

    METHODS find_cup
      IMPORTING
        cup TYPE i
      RETURNING
        VALUE(result) TYPE REF TO zcl_advent2020_cup23_joltdx.

    METHODS play_move
      IMPORTING
        current_cup TYPE REF TO zcl_advent2020_cup23_joltdx
      RETURNING
        VALUE(result) TYPE REF TO zcl_advent2020_cup23_joltdx.

ENDCLASS.



CLASS ZCL_ADVENT2020_DAY23_joltdx IMPLEMENTATION.

  METHOD zif_advent2020_joltdx~solve.

    output = |Part 1:\n{ part_1( ) }\n\nPart 2:\n{ part_2( ) }|.

  ENDMETHOD.

  METHOD part_1.
    DATA offset_destination TYPE i.
    DATA current TYPE i.
    DATA destination TYPE i.
    DATA pick_up TYPE string.
    DATA game TYPE string.
    DATA chunk_1_size TYPE i.
    DATA chunk_2_offset TYPE i.
    DATA chunk_2_size TYPE i.

    game = |157623984|.

    DO 100 TIMES.
      current = game+0(1).
      pick_up = game+1(3).
      IF current = 1.
        destination = 9.
      ELSE.
        destination = current - 1.
      ENDIF.
      WHILE pick_up CS destination.
        destination = destination - 1.
        IF destination = 0.
          destination = 9.
        ENDIF.
      ENDWHILE.
      FIND FIRST OCCURRENCE OF destination IN game MATCH OFFSET offset_destination.
      chunk_2_offset = offset_destination + 1.
      chunk_2_size = 9 - chunk_2_offset.
      chunk_1_size = offset_destination - 3.
      game = |{ game+4(chunk_1_size) }{ pick_up }{ game+chunk_2_offset(chunk_2_size) }{ game+0(1) }|.
      " result = |{ result }{ sy-index }: { game }\n|.
    ENDDO.

    FIND FIRST OCCURRENCE OF 1 IN game MATCH OFFSET offset_destination.
    chunk_2_offset = offset_destination + 1.
    chunk_2_size = 9 - chunk_2_offset.
    chunk_1_size = 8 - chunk_2_size.
    result = |Labels after cup 1: { game+chunk_2_offset(chunk_2_size) }{ game+0(chunk_1_size) }|.
  ENDMETHOD.

  METHOD part_2.
    DATA previous TYPE REF TO zcl_advent2020_cup23_joltdx.
    DATA first_cup TYPE REF TO zcl_advent2020_cup23_joltdx.
    DATA last_cup TYPE REF TO zcl_advent2020_cup23_joltdx.
    DATA init_cup TYPE REF TO zcl_advent2020_cup23_joltdx.
    DATA current_cup TYPE REF TO zcl_advent2020_cup23_joltdx.
    DATA first_to_move TYPE REF TO zcl_advent2020_cup23_joltdx.
    DATA last_to_move TYPE REF TO zcl_advent2020_cup23_joltdx.
    DATA next_cup TYPE REF TO zcl_advent2020_cup23_joltdx.
    DATA where_to TYPE REF TO zcl_advent2020_cup23_joltdx.
    DATA next_id TYPE i.

    previous = NEW zcl_advent2020_cup23_joltdx( 0 ).
    first_cup = previous.
    DO 1000000 TIMES.
      previous->mo_next = new_cup( sy-index ).
      previous = previous->mo_next.
    ENDDO.
    last_cup = previous.
    first_cup = first_cup->mo_next.
    last_cup->mo_next = first_cup.

    first_cup->mo_next = find_cup( 5 ).
    init_cup = find_cup( 5 ).
    init_cup->mo_next = find_cup( 7 ).
    init_cup = find_cup( 7 ).
    init_cup->mo_next = find_cup( 6 ).
    init_cup = find_cup( 6 ).
    init_cup->mo_next = find_cup( 2 ).
    init_cup = find_cup( 2 ).
    init_cup->mo_next = find_cup( 3 ).
    init_cup = find_cup( 3 ).
    init_cup->mo_next = find_cup( 9 ).
    init_cup = find_cup( 9 ).
    init_cup->mo_next = find_cup( 8 ).
    init_cup = find_cup( 8 ).
    init_cup->mo_next = find_cup( 4 ).
    init_cup = find_cup( 4 ).
    init_cup->mo_next = find_cup( 10 ).

    current_cup = find_cup( 1 ).
    DO 10000000 TIMES.
      current_cup = play_move( current_cup ).
    ENDDO.

    current_cup = find_cup( 1 ).
    DATA(after_one) = current_cup->mo_next->mv_cup.
    DATA(after_that_one) = current_cup->mo_next->mo_next->mv_cup.
    result = |{ after_one } * { after_that_one } = { after_one * after_that_one }|.

  ENDMETHOD.

  METHOD new_cup.
    DATA(new_cup) = NEW zcl_advent2020_cup23_joltdx( cup ).
    APPEND VALUE #( cup = cup
                    ref = new_cup ) TO mt_crab_cups.
    result = new_cup.
  ENDMETHOD.

  METHOD find_cup.
    READ TABLE mt_crab_cups INDEX cup INTO DATA(crab_cup).
    result = crab_cup-ref.
  ENDMETHOD.

  METHOD play_move.
    DATA first_to_move TYPE REF TO zcl_advent2020_cup23_joltdx.
    DATA last_to_move TYPE REF TO zcl_advent2020_cup23_joltdx.
    DATA next_cup TYPE REF TO zcl_advent2020_cup23_joltdx.
    DATA where_to TYPE REF TO zcl_advent2020_cup23_joltdx.
    DATA where_to_id TYPE i.
    first_to_move = current_cup->mo_next.
    last_to_move = first_to_move->mo_next->mo_next.
    next_cup = last_to_move->mo_next.
    where_to_id = current_cup->mv_cup.
    WHILE where_to_id = current_cup->mv_cup OR
          where_to_id = first_to_move->mv_cup OR
          where_to_id = first_to_move->mo_next->mv_cup OR
          where_to_id = first_to_move->mo_next->mo_next->mv_cup.
      where_to_id = where_to_id - 1.
      IF where_to_id < 1.
        where_to_id = 1000000.
      ENDIF.
    ENDWHILE.
    where_to = find_cup( where_to_id ).

    last_to_move->mo_next = where_to->mo_next.
    where_to->mo_next = first_to_move.
    current_cup->mo_next = next_cup.
    result = next_cup.

  ENDMETHOD.
ENDCLASS.
