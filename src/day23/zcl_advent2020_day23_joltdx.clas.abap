CLASS zcl_advent2020_day23_joltdx DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_advent2020_joltdx .
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS part_1
      RETURNING
        VALUE(result) TYPE string.

    METHODS part_2
      RETURNING
        VALUE(result) TYPE string.

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

    result = |todo|.

  ENDMETHOD.

ENDCLASS.
