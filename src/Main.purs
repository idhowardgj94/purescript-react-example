module Main where

import Prelude

import Effect (Effect)

import Data.Array (snoc, modifyAt, elemIndex)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)

import Web.HTML.HTMLDocument (toNonElementParentNode) as DOM
import Web.DOM.NonElementParentNode (getElementById) as DOM
import Web.HTML (window) as DOM
import Web.HTML.Window (document) as DOM

import Partial.Unsafe (unsafePartial)

import React as React
import ReactDOM as ReactDOM

import Example.TodoList (todoListClass)
import Example.Types (Todo(..), TodoStatus(..))

-- define in Web.HWML.Window
-- data Window :: Type
-- document :: Window -> Effect HTMLDocument

-- data Unit :: Type
-- Unit type mean only effects is important.
-- void is a function in Functor typeclass.
-- unit is a functor.
main :: Effect Unit
main = void $ do
  -- 基本上，幾乎所有的動作type 都是 effect
  -- Unit 這個type 代表回傳值不重要，搭配 void這個function，可以達到 "return null" 的效果

  -- window :: Effect Window
  window <- DOM.window

  document <- DOM.document window

  let
      node = DOM.toNonElementParentNode document

  element <- DOM.getElementById "example" node

  let
      element' = unsafePartial (fromJust element)

  ReactDOM.render (React.createLeafElement mainClass { }) element'

mainClass :: React.ReactClass { }
mainClass = React.component "Main" component
  where
  component this =
    pure { state:
            { todo: Nothing
            , todos: []
            }
         , render: render <$> React.getState this
         }
    where
    render
      { todo
      , todos
      } =
      React.createLeafElement todoListClass
        { todos
        , todo

        , onAdd: \todo' -> React.modifyState this \a ->
            a { todo = Nothing
              , todos = snoc a.todos todo'
              }

        , onEdit: \todo' -> React.modifyState this
            _ { todo = Just todo'
              }

        , onDone: \todo' -> React.modifyState this \a ->
            a { todos = setStatus a.todos todo' TodoDone
              }

        , onClear : \todo' -> React.modifyState this \a ->
            a { todos = setStatus a.todos todo' TodoCleared
              }
        }

    setStatus todos todo status = fromMaybe todos $ do
      i <- elemIndex todo todos

      modifyAt i (\(Todo a) -> Todo a { status = status }) todos
