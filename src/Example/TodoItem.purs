module Example.TodoItem where

import Prelude

import React as React
import React.Hook (Hook)
import React.DOM as DOM
import React.DOM.Props as Props

import Example.Types (Todo(..), TodoStatus(..))

type TodoItemProps = { todo :: Todo }

todoItem :: TodoItemProps -> Hook React.ReactElement
todoItem
  { todo: Todo
      { text
      , status
      }
  } = pure $
  DOM.div
    [ Props.style { textDecoration } ]
    [ React.toElement text ]
  where
  textDecoration =
    case status of
         TodoDone -> "line-through"
         _ -> "none"
