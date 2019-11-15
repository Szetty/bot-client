module EventHandlers
    ( module EventHandlers.HTTP
    , module EventHandlers.WS
    , module EventHandlers.Common
    ) where

import EventHandlers.HTTP (runHTTPEventServer)
import EventHandlers.WS (runWSEventServer)
import EventHandlers.Common (GameConfig (..))