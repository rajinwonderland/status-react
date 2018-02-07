(ns status-im.ui.screens.group.chat-settings.events
  (:require [re-frame.core :as re-frame]
            [status-im.constants :as constants]
            [status-im.data-store.chats :as chats]
            [status-im.data-store.messages :as messages]
            [status-im.i18n :as i18n]
            [status-im.transport.message.v1.group-chat :as group-chat]
            [status-im.transport.message.core :as transport]
            [status-im.utils.handlers :as handlers]
            [status-im.utils.random :as random]))

;;;; FX

(re-frame/reg-fx
  :data-store/save-chat-property
  (fn [[current-chat-id property-name value]]
    (chats/save-property current-chat-id property-name value)))

(re-frame/reg-fx
  :data-store/add-members-to-chat
  (fn [{:keys [current-chat-id selected-participants]}]
    (chats/add-contacts current-chat-id selected-participants)))

(re-frame/reg-fx
  :data-store/remove-members-from-chat
  (fn [[current-chat-id participants]]
    (chats/remove-contacts current-chat-id participants)))

(defn system-message [message-id content]
  {:from         "system"
   :message-id   message-id
   :content      content
   :content-type constants/text-content-type})

(defn removed-participant-message [chat-id identity contact-name]
  (let [message-text (str "You've removed " (or contact-name identity))]
    (-> (system-message (random/id) message-text)
        (assoc :chat-id chat-id)
        (messages/save))))

(re-frame/reg-fx
  :data-store/create-removing-messages
  (fn [{:keys [current-chat-id participants contacts/contacts]}]
    (doseq [participant participants]
      (let [contact-name (get-in contacts [participant :name])]
        (removed-participant-message current-chat-id participant contact-name)))))

;;;; Handlers

(handlers/register-handler-fx
  :show-group-chat-profile
  (fn [{db :db} [_ chat-id]]
    {:db (assoc db :new-chat-name (get-in db [:chats chat-id :name])
                :group/group-type :chat-group)
     :dispatch [:navigate-to :group-chat-profile]}))

(handlers/register-handler-fx
  :add-new-group-chat-participants
  (fn [{{:keys [current-chat-id selected-participants] :as db} :db :as cofx} _]
    (let [new-identities (map #(hash-map :identity %) selected-participants)]
      (handlers/merge-fx cofx
                         {:db (-> db
                                  (update-in [:chats current-chat-id :contacts] concat new-identities)
                                  (assoc :selected-participants #{}))
                          :data-store/add-members-to-chat (select-keys db [:current-chat-id :selected-participants])}
                         (transport/send current-chat-id (group-chat/map->GroupAdminAdd {:added-participants selected-participants}))))))

(defn- remove-identities [collection identities]
  (remove #(identities (:identity %)) collection))

(handlers/register-handler-fx
  :remove-group-chat-participants
  (fn [{{:keys [current-chat-id] :as db} :db :as cofx} [_ removed-participants]]
    (handlers/merge-fx cofx
                       {:db (update-in db [:chats current-chat-id :contacts] remove-identities removed-participants)
                        :data-store/remove-members-from-chat [current-chat-id removed-participants]
                        :data-store/create-removing-messages (merge {:participants removed-participants}
                                                                    (select-keys db [:current-chat-id :contacts/contacts]))}
                       (transport/send current-chat-id (group-chat/map->GroupAdminRemove {:removed-participants removed-participants})))))

(handlers/register-handler-fx
  :set-group-chat-name
  (fn [{{:keys [current-chat-id] :as db} :db} [_ new-chat-name]]
    {:db (assoc-in db [:chats current-chat-id :name] new-chat-name)
     :data-store/save-chat-property [current-chat-id :name new-chat-name]}))

(handlers/register-handler-fx
  :clear-history
  (fn [{{:keys [current-chat-id] :as db} :db} _]
    {:db (assoc-in db [:chats current-chat-id :messages] {})
     :delete-messages current-chat-id}))

(handlers/register-handler-fx
  :clear-history?
  (fn [_ _]
    {:show-confirmation {:title               (i18n/label :t/clear-history-confirmation)
                         :content             (i18n/label :t/clear-group-history-confirmation)
                         :confirm-button-text (i18n/label :t/clear)
                         :on-accept           #(re-frame/dispatch [:clear-history])}}))
