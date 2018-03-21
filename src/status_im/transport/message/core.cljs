(ns status-im.transport.message.core
  (:require [re-frame.core :as re-frame] [status-im.utils.handlers :as handlers]
            [status-im.data-store.messages :as data-store.messages]
            [status-im.chat.models :as models.chat]
            [status-im.constants :as constants]
            [status-im.chat.models :as models]))

(defprotocol StatusMessage
  "Protocol for transport layed status messages"
  (send [this chat-id cofx])
  (receive [this chat-id signature cofx]))

;; TODO (yenda) implement
;; :pending
;; :discover
;; :discoveries-request
;; :discoveries-response
;; :profile
;; :online

;;TODO (yenda) this is probably not the place to have these

;; Contacts

(defn receive-contact-request
  [public-key
   {:keys [name profile-image address fcm-token]}
   {{:contacts/keys [contacts] :as db} :db :as cofx}]
  (when-not (get contacts public-key)
    (let [contact-props {:whisper-identity public-key
                         :public-key       public-key
                         :address          address
                         :photo-path       profile-image
                         :name             name
                         :fcm-token        fcm-token
                         :pending?         true}
          chat-props    {:name         name
                         :chat-id      public-key
                         :contact-info (prn-str contact-props)}]
      (handlers/merge-fx cofx
                         {:db           (update-in db [:contacts/contacts public-key] merge contact-props)
                          :save-contact contact-props}
                         (models.chat/add-chat public-key chat-props)))))

(defn receive-contact-request-confirmation
  [public-key {:keys [name profile-image address fcm-token]}
   {{:contacts/keys [contacts] :as db} :db :as cofx}]
  (when-let [contact (get contacts public-key)]
    (let [contact-props {:whisper-identity public-key
                         :address          address
                         :photo-path       profile-image
                         :name             name
                         :fcm-token        fcm-token}
          chat-props    {:name    name
                         :chat-id public-key}]
      (handlers/merge-fx cofx
                         {:db           (update-in db [:contacts/contacts public-key] merge contact-props)
                          :save-contact contact-props}
                         (models.chat/upsert-chat chat-props)))))

;; Seen messages
(defn receive-seen
  [chat-id sender message-ids {:keys [db]}]
  (when-let [seen-messages-ids (-> (get-in db [:chats chat-id :messages])
                                   (select-keys message-ids)
                                   keys)]
    {:db (reduce (fn [new-db message-id]
                   (assoc-in new-db [:chats chat-id :messages message-id :user-statuses sender] :seen))
                 db
                 seen-messages-ids)}))

;; Group chat
(defn participants-added [chat-id added-participants {:keys [db] :as cofx}]
  (when added-participants
    {:db (update-in db [:chats chat-id :contacts] concat (mapv #(hash-map :identity %) added-participants))
     :data-store/add-chat-contacts [chat-id added-participants]}))

(defn participants-removed [chat-id removed-participants {:keys [now db] :as cofx}]
  (when removed-participants
    (let [{:keys [is-active timestamp]} (get-in db [:chats chat-id])]
      ;;TODO: not sure what this condition is for
      (when (and is-active (> now timestamp))
        {:data-store/remove-chat-contacts [chat-id removed-participants]
         :db (update-in db [:chats chat-id :contacts]
                        #(remove (fn [{:keys [identity]}]
                                   (removed-participants identity)) %))}))))
