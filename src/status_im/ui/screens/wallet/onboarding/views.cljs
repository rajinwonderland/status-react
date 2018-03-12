(ns status-im.ui.screens.wallet.onboarding.views
  (:require-macros [status-im.utils.views :as views])
  (:require [status-im.i18n :as i18n]
            [re-frame.core :as re-frame]
            [status-im.ui.components.react :as react]))

(views/defview onboarding []
  (views/letsubs [{:keys [address]} [:get-current-account]]
    [react/view {:flex 1}]))