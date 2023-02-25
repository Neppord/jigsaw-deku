module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Array ((..))
import Data.Compactable (compact)
import Data.Foldable (for_)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Profunctor (lcmap)
import Data.Traversable (for)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested ((/\))
import Deku.Attribute ((!:=), (<:=>))
import Deku.Control (blank, (<#~>))
import Deku.DOM.Elt.Defs (defs_)
import Deku.DOM.Elt.Image (image)
import Deku.DOM.Elt.Pattern (pattern)
import Deku.DOM.Elt.Rect (rect)
import Deku.DOM.Elt.Svg (svg)
import Deku.Hooks (useMailboxed, useState)
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Random (randomInt)
import FRP.Event (Event, makeEvent, subscribe)
import FRP.Event.AnimationFrame (animationFrame)
import FRP.Event.Class (fold, gate)
import Data.Array (intercalate) as Array
import Deku.DOM.Attr.Fill (Fill(Fill)) as D
import Deku.DOM.Attr.Height (Height(Height)) as D
import Deku.DOM.Attr.Href (Href(Href)) as D
import Deku.DOM.Attr.Id (Id(Id)) as D
import Deku.DOM.Attr.OnMousedown (OnMousedown(OnMousedown)) as D
import Deku.DOM.Attr.OnMouseup (OnMouseup(OnMouseup)) as D
import Deku.DOM.Attr.Stroke (Stroke(Stroke)) as D
import Deku.DOM.Attr.StrokeWidth (StrokeWidth(StrokeWidth)) as D
import Deku.DOM.Attr.ViewBox (ViewBox(ViewBox)) as D
import Deku.DOM.Attr.Width (Width(Width)) as D
import Deku.DOM.Attr.X (X(X)) as D
import Deku.DOM.Attr.Y (Y(Y)) as D
import Deku.Do as Deku
import FRP.Event.Keyboard (down, up) as Keyboard
import Data.Map.Internal (fromFoldable, lookup) as Map
import Data.Maybe (fromMaybe) as Maybe
import FRP.Event.Mouse (down, getMouse, withPosition) as Mouse
import Effect.Ref (modify_, new, read, write) as Ref
import Data.Set (empty, insert, singleton) as Set

type Piece = Tuple Int Int
type Pieces = Array Piece

width :: Int
width = 3840

height :: Int
height = 2160

pieces_wide :: Int
pieces_wide = 16

pieces_high :: Int
pieces_high = 9

piece_width :: Int
piece_width = width / pieces_wide

piece_height :: Int
piece_height = height / pieces_high

pieces :: Pieces
pieces = Tuple <$> 0 .. (pieces_wide - 1) <*> 0 .. (pieces_high - 1)

main :: Effect Unit
main = do
  selected <- Ref.new Set.empty
  mouse <- Mouse.getMouse
  random_starts <- Map.fromFoldable <$> for pieces \(x /\ y) -> do
    x' <- randomInt 0 width <#> (_ - (x * piece_width))
    y' <- randomInt 0 height <#> (_ - (y * piece_height))
    pure $ (x /\ y) /\ (x' /\ y')
  runInBody Deku.do
    send_piece_dragged /\ receive_piece_dragged <- useState true
    send_selection /\ receive_selection <- useMailboxed
    send_move /\ receive_move <- useMailboxed <<< lcmap \(sender /\ receiver) ->
      sender /\ \address ->
        receiver address <|>
          (Map.lookup address random_starts # Maybe.fromMaybe (0 /\ 0) # pure)
    let

      mouse_position :: Event { x :: Int, y :: Int }
      mouse_position = Mouse.withPosition mouse animationFrame <#> _.pos # compact

      clicked_outside = gate (receive_piece_dragged <#> not)
        (Mouse.down <#> const true)

      drag = gate receive_piece_dragged $ mouse_position - mouse_position
      has_been_dragging =
        (receive_piece_dragged <#> const false)
          <|> (drag <#> const true)

      is_shift_down = Keyboard.down
        <#> case _ of
          "ShiftLeft" -> Just true
          _ -> Nothing
        # compact
      is_shift_up = Keyboard.up
        <#> case _ of
          "ShiftLeft" -> Just false
          _ -> Nothing
        # compact

      is_shift = pure false <|> is_shift_down <|> is_shift_up

      receive_pos = receive_move <#> (fold add (0 /\ 0))
      receive_x = receive_pos <#> map fst
      receive_y = receive_pos <#> map snd

      move_selected :: Event (Tuple Int Int)
      move_selected = makeEvent \k -> do
        void $ subscribe drag \{ x: dx, y: dy } -> do
          let
            move = dx /\ dy
          to_move <- Ref.read selected
          for_ to_move \address -> do
            send_move { address, payload: move }
          k move
        pure $ pure unit

      unselect_all = makeEvent \_ -> do
        void $ subscribe clicked_outside \e ->
          if e then do
            to_unselect <- Ref.read selected
            for_ to_unselect \address ->
              send_selection { address, payload: false }
            selected # Ref.write Set.empty
          else pure unit
        pure $ pure unit

      sink event = event <#~> \_ -> blank
    svg
      (D.Width !:= show (width * 2) <|> D.Height !:= show (height * 2))
      ( [ sink move_selected
        , sink unselect_all
        , defs_ $
            pieces <#> \(x /\ y) -> pattern
              ( D.Id !:= "background-" <> show x <> "-" <> show y
                  <|> D.Width !:= "100%"
                  <|> D.Height !:= "100%"
                  <|> D.ViewBox !:=
                    ( [ 0, 0, piece_width, piece_height ]
                        <#> show
                        # Array.intercalate " "
                    )
              )
              [ image
                  ( D.Href !:= "ship-1366926_crop_4k.png"
                      <|> D.X !:= show (-x * piece_width)
                      <|> D.Y !:= show (-y * piece_height)
                      <|> D.Width !:= show width
                      <|> D.Height !:= show height
                  )
                  []
              ]

        ]
          <>
            ( pieces <#> \(x /\ y) ->
                rect
                  ( D.X <:=> (receive_x (x /\ y) <#> (_ + (x * piece_width)) <#> show)
                      <|> D.Y <:=> (receive_y (x /\ y) <#> (_ + (y * piece_height)) <#> show)
                      <|> D.Width !:= (show $ piece_width)
                      <|> D.Height !:= (show $ piece_height)
                      <|> D.Fill !:= "url(#background-" <> show x <> "-" <> show y <> ")"
                      <|> D.Stroke <:=>
                        ( receive_selection (x /\ y) <#> case _ of
                            true -> "orange"
                            false -> "none"
                        )
                      <|> D.StrokeWidth !:= "5"
                      <|> D.OnMousedown !:= do
                        send_selection { address: x /\ y, payload: true }
                        selected # Ref.modify_ (Set.insert (x /\ y))
                        send_piece_dragged true
                      <|> D.OnMouseup <:=>
                        ( (has_been_dragging || is_shift) <#> case _ of
                            true -> send_piece_dragged false
                            false -> do
                              to_unselect <- Ref.read selected
                              for_ to_unselect \address ->
                                send_selection { address, payload: false }
                              send_selection { address: x /\ y, payload: true }
                              selected # Ref.write (Set.singleton (x /\ y))
                              send_piece_dragged false
                        )
                  )
                  []
            )
      )
