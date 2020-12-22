CLASS zcl_advent2020_day22_joltdx DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_advent2020_joltdx .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mt_player_1_hand TYPE STANDARD TABLE OF i.
    DATA mt_player_2_hand TYPE STANDARD TABLE OF i.

    METHODS part_1
      RETURNING
        VALUE(result) TYPE string.

    METHODS part_2
      RETURNING
        VALUE(result) TYPE string.

ENDCLASS.



CLASS ZCL_ADVENT2020_DAY22_joltdx IMPLEMENTATION.

  METHOD zif_advent2020_joltdx~solve.

    REPLACE ALL OCCURRENCES OF |\r| IN input WITH ||.
    SPLIT input AT |\n\n| INTO TABLE DATA(players_hands).
    READ TABLE players_hands INDEX 1 INTO DATA(player_hand).
    SPLIT player_hand AT |\n| INTO TABLE DATA(hand).
    LOOP AT hand INTO DATA(card).
      IF sy-tabix = 1.
        CONTINUE.
      ENDIF.
      APPEND card TO mt_player_1_hand.
    ENDLOOP.
    READ TABLE players_hands INDEX 2 INTO player_hand.
    SPLIT player_hand AT |\n| INTO TABLE hand.
    LOOP AT hand INTO card.
      IF sy-tabix = 1.
        CONTINUE.
      ENDIF.
      APPEND card TO mt_player_2_hand.
    ENDLOOP.

    output = |Part 1:\n{ part_1( ) }\n\nPart 2:\n{ part_2( ) }|.

  ENDMETHOD.

  METHOD part_1.
    DATA winning_hand TYPE STANDARD TABLE OF i.
    DATA score TYPE i.
    DATA top_card_value TYPE i.

    WHILE lines( mt_player_1_hand ) > 0 AND
          lines( mt_player_2_hand ) > 0.
      READ TABLE mt_player_1_hand INDEX 1 INTO DATA(p1_card).
      DELETE mt_player_1_hand INDEX 1.
      READ TABLE mt_player_2_hand INDEX 1 INTO DATA(p2_card).
      DELETE mt_player_2_hand INDEX 1.
      IF p1_card > p2_card.
        APPEND p1_card TO mt_player_1_hand.
        APPEND p2_card TO mt_player_1_hand.
      ELSE.
        APPEND p2_card TO mt_player_2_hand.
        APPEND p1_card TO mt_player_2_hand.
      ENDIF.
    ENDWHILE.
    IF lines( mt_player_1_hand ) > 0.
      winning_hand = mt_player_1_hand.
      result = |Player 1 wins Crab Combat with a score of |.
    ELSE.
      winning_hand = mt_player_2_hand.
      result = |Player 2 wins Crab Combat with a score of |.
    ENDIF.

    top_card_value = lines( winning_hand ) + 1.
    LOOP AT winning_hand INTO DATA(card).
      score = score + ( card * top_card_value - sy-tabix ).
    ENDLOOP.

    result = |{ result }{ score }|.

  ENDMETHOD.

  METHOD part_2.

    result = |todo|.

  ENDMETHOD.

ENDCLASS.
