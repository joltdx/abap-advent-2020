CLASS zcl_advent2020_day22_joltdx DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_advent2020_joltdx .
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES ty_hand_tt TYPE STANDARD TABLE OF i WITH EMPTY KEY.
    DATA mt_init_player_1_hand TYPE ty_hand_tt.
    DATA mt_init_player_2_hand TYPE ty_hand_tt.

    METHODS part_1
      RETURNING
        VALUE(result) TYPE string.

    METHODS part_2
      RETURNING
        VALUE(result) TYPE string.

    METHODS recursive_combat
      IMPORTING
        hand_1 TYPE ty_hand_tt
        hand_2 TYPE ty_hand_tt
      EXPORTING
        VALUE(new_hand_1) TYPE ty_hand_tt
        VALUE(new_hand_2) TYPE ty_hand_tt
        VALUE(winner) TYPE i.

    METHODS get_hands_string
      IMPORTING
        hand_1 TYPE ty_hand_tt
        hand_2 TYPE ty_hand_tt
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
      APPEND card TO mt_init_player_1_hand.
    ENDLOOP.
    READ TABLE players_hands INDEX 2 INTO player_hand.
    SPLIT player_hand AT |\n| INTO TABLE hand.
    LOOP AT hand INTO card.
      IF sy-tabix = 1.
        CONTINUE.
      ENDIF.
      APPEND card TO mt_init_player_2_hand.
    ENDLOOP.

    output = |Part 1:\n{ part_1( ) }\n\nPart 2:\n{ part_2( ) }|.

  ENDMETHOD.

  METHOD part_1.
    DATA winning_hand TYPE STANDARD TABLE OF i.
    DATA score TYPE i.
    DATA top_card_value TYPE i.

    DATA(player_1_hand) = mt_init_player_1_hand.
    DATA(player_2_hand) = mt_init_player_2_hand.

    WHILE lines( player_1_hand ) > 0 AND
          lines( player_2_hand ) > 0.
      READ TABLE player_1_hand INDEX 1 INTO DATA(p1_card).
      DELETE player_1_hand INDEX 1.
      READ TABLE player_2_hand INDEX 1 INTO DATA(p2_card).
      DELETE player_2_hand INDEX 1.
      IF p1_card > p2_card.
        APPEND p1_card TO player_1_hand.
        APPEND p2_card TO player_1_hand.
      ELSE.
        APPEND p2_card TO player_2_hand.
        APPEND p1_card TO player_2_hand.
      ENDIF.
    ENDWHILE.
    IF lines( player_1_hand ) > 0.
      winning_hand = player_1_hand.
      result = |Player 1 wins Crab Combat with a score of |.
    ELSE.
      winning_hand = player_2_hand.
      result = |Player 2 wins Crab Combat with a score of |.
    ENDIF.

    top_card_value = lines( winning_hand ) + 1.
    LOOP AT winning_hand INTO DATA(card).
      score = score + ( card * ( top_card_value - sy-tabix ) ).
    ENDLOOP.

    result = |{ result }{ score }|.

  ENDMETHOD.

  METHOD part_2.
    DATA winner TYPE i.
    DATA player_1_hand TYPE ty_hand_tt.
    DATA player_2_hand TYPE ty_hand_tt.
    DATA winning_hand TYPE STANDARD TABLE OF i.
    DATA score TYPE i.
    DATA top_card_value TYPE i.

    recursive_combat( EXPORTING hand_1 = mt_init_player_1_hand
                                hand_2 = mt_init_player_2_hand
                      IMPORTING new_hand_1 = player_1_hand
                                new_hand_2 = player_2_hand
                                winner = winner ).

    IF winner = 1.
      winning_hand = player_1_hand.
      result = |Player 1 wins Recursive Crab Combat with a score of |.
    ELSE.
      winning_hand = player_2_hand.
      result = |Player 2 wins Recursive Crab Combat with a score of |.
    ENDIF.

    score = 0.
    top_card_value = lines( winning_hand ) + 1.
    LOOP AT winning_hand INTO DATA(card).
      score = score + ( card * ( top_card_value - sy-tabix ) ).
    ENDLOOP.

    result = |{ result }{ score }|.
  ENDMETHOD.

  METHOD recursive_combat.
    DATA previous_hands TYPE string.
    DATA current_hands TYPE string.
    DATA stack_1 TYPE ty_hand_tt.
    DATA stack_2 TYPE ty_hand_tt.

    DATA(player_1_hand) = hand_1.
    DATA(player_2_hand) = hand_2.

    WHILE lines( player_1_hand ) > 0 AND
          lines( player_2_hand ) > 0.
      current_hands = get_hands_string( hand_1 = player_1_hand
                                        hand_2 = player_2_hand ).
      FIND FIRST OCCURRENCE OF current_hands IN previous_hands MATCH OFFSET DATA(match_offset).
      IF sy-subrc = 0.
        winner = 1.
        RETURN.
      ELSE.
        previous_hands = |{ previous_hands }{ current_hands }|.
        READ TABLE player_1_hand INDEX 1 INTO DATA(p1_card).
        DELETE player_1_hand INDEX 1.
        READ TABLE player_2_hand INDEX 1 INTO DATA(p2_card).
        DELETE player_2_hand INDEX 1.

        IF lines( player_1_hand ) >= p1_card AND
            lines( player_2_hand ) >= p2_card.
          CLEAR stack_1[].
          DO p1_card TIMES.
            READ TABLE player_1_hand INDEX sy-index INTO DATA(card).
            APPEND card TO stack_1.
          ENDDO.
          CLEAR stack_2[].
          DO p2_card TIMES.
            READ TABLE player_2_hand INDEX sy-index INTO card.
            APPEND card TO stack_2.
          ENDDO.
          recursive_combat( EXPORTING hand_1 = stack_1
                                      hand_2 = stack_2
                            IMPORTING winner = winner ).
        ELSEIF p1_card > p2_card.
          winner = 1.
        ELSE.
          winner = 2.
        ENDIF.
      ENDIF.
      IF winner = 1.
        APPEND p1_card TO player_1_hand.
        APPEND p2_card TO player_1_hand.
      ELSE.
        APPEND p2_card TO player_2_hand.
        APPEND p1_card TO player_2_hand.
      ENDIF.
    ENDWHILE.

    new_hand_1 = player_1_hand.
    new_hand_2 = player_2_hand.
  ENDMETHOD.

  METHOD get_hands_string.
    result = |s|.
    LOOP AT hand_1 INTO DATA(card).
      result = |{ result }c{ card }|.
    ENDLOOP.
    result = |{ result }:|.
    LOOP AT hand_2 INTO card.
      result = |{ result }c{ card }|.
    ENDLOOP.
    result = |{ result }e|.
  ENDMETHOD.
ENDCLASS.
