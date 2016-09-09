import Html.App as App

import Model
import Subscriptions
import Update
import View

init = (Model.init, Update.init)

main = App.program
    { init = init
    , update = Update.update
    , subscriptions = Subscriptions.subscriptions
    , view = View.view
    }


