(ns status-im.chat.handlers
  (:require [clojure.string :as string]
            [re-frame.core :as re-frame]
            [status-im.chat.models :as models]
            [status-im.i18n :as i18n]
            [status-im.ui.components.styles :as components.styles]
            [status-im.utils.handlers :as handlers]
            [status-im.utils.random :as random]
            [status-im.transport.message.v1.group-chat :as group-chat]
            [status-im.transport.message.core :as transport]
            status-im.chat.events
            [status-im.utils.datetime :as datetime]))

(handlers/register-handler-fx
  :leave-group-chat?
  (fn []
    {:show-confirmation {:title               (i18n/label :t/leave-confirmation)
                         :content             (i18n/label :t/leave-group-chat-confirmation)
                         :confirm-button-text (i18n/label :t/leave)
                         :on-accept           #(re-frame/dispatch [:leave-group-chat])}}))

(defn group-name-from-contacts [contacts selected-contacts username]
  (->> (select-keys contacts selected-contacts)
       vals
       (map :name)
       (cons username)
       (string/join ", ")))

(defn prepare-group-chat
  [{:keys [current-public-key username]
    :group/keys [selected-contacts]
    :contacts/keys [contacts]} group-name timestamp]
  (let [selected-contacts'  (mapv #(hash-map :identity %) selected-contacts)
        chat-name (if-not (string/blank? group-name)
                    group-name
                    (group-name-from-contacts contacts selected-contacts username))]
    {:chat-id     (random/id)
     :name        chat-name
     :color       components.styles/default-chat-color
     :group-chat  true
     :group-admin current-public-key
     :is-active   true
     :timestamp timestamp
     :contacts    selected-contacts'
     :last-to-clock-value   0
     :last-from-clock-value 0}))

(handlers/register-handler-fx
  :create-new-group-chat-and-open
  (fn [{:keys [db] :as cofx} [_ group-name]]
    (let [{:group/keys [selected-contacts]} db
          {:keys [chat-id] :as new-chat} (prepare-group-chat (select-keys db [:group/selected-contacts :current-public-key :username
                                                                              :contacts/contacts])
                                                             group-name)]
      (handlers/merge-fx cofx
                         {:db (-> db
                                  (assoc-in [:chats chat-id] new-chat)
                                  (assoc :group/selected-contacts #{}))
                          :save-chat new-chat
                          :dispatch-n [[:navigate-to-clean :home]
                                       [:navigate-to-chat (:chat-id new-chat)]]}
                         (transport/send (group-chat/GroupAdminUpdate. selected-contacts) chat-id)))))

(handlers/register-handler-fx
  :group-chat-invite-received
  (fn [{{:keys [current-public-key] :as db} :db}
       [_ {:keys                                                    [from]
           {:keys [group-id group-name contacts keypair timestamp]} :payload}]]
    (let [contacts' (keep (fn [ident]
                            (when (not= ident current-public-key)
                              {:identity ident})) contacts)
          chat      (get-in db [:chats group-id])
          new-chat  {:chat-id     group-id
                     :name        group-name
                     :group-chat  true
                     :group-admin from
                     :contacts    contacts'
                     :added-to-at timestamp
                     :timestamp   timestamp
                     :is-active   true}]
      (when (or (nil? chat)
                (models/new-update? chat timestamp))
        {:dispatch (if chat
                     [:update-chat! new-chat]
                     [:add-chat group-id new-chat])}))))

(handlers/register-handler
  :leave-group-chat
  ;; stop listening to group here
  (fn [{{:keys [web3 current-chat-id chats current-public-key] :as db} :db :as cofx} _]
    (let [{:keys [public?]} (chats current-chat-id)
          dispatched-events {:dispatch-n [[:remove-chat current-chat-id]
                                          [:navigation-replace :home]]}]
      (if public?
        dispatched-events
        (handlers/merge-fx cofx
                           dispatched-events
                           (transport/send (group-chat/GroupLeave.) current-chat-id ))))))

(handlers/register-handler-fx
  :leave-group-chat?
  (fn []
    {:show-confirmation {:title               (i18n/label :t/leave-confirmation)
                         :content             (i18n/label :t/leave-group-chat-confirmation)
                         :confirm-button-text (i18n/label :t/leave)
                         :on-accept           #(re-frame/dispatch [:leave-group-chat])}}))

(handlers/register-handler-fx
  :show-profile
  (fn [{db :db} [_ identity]]
    {:db (assoc db :contacts/identity identity)
     :dispatch [:navigate-forget :profile]}))
